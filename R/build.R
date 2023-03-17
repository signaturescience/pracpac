#' Build a package tar.gz
#'
#' @description
#' Builds a package source tar.gz using [pkgbuild::build] and moves it into a user-specified location (default `docker/`).
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param ... Additional optional arguments passed to [pkgbuild::build].
#'
#' @return Invisibly returns a list of package info returned by [pkg_info], tar.gz source and destination file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Build the example package from tempdir()
#' build_pkg(pkg = file.path(tempdir(), "hellow"))
#' }
build_pkg <- function(pkg_path=".", img_path = NULL, ...) {

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  # Create the target directory if it doesn't already exist
  if(!fs::dir_exists(docker_dir)) {
    create_docker_dir(pkg_path = pkg_path, img_path = img_path)
  }

  # Get package information and construct filepaths to file built by R CMD build and eventual package tar.gz
  info <- pkg_info(pkg_path)
  tarsrc <- fs::path(pkg_path, glue::glue("{info$pkgname}_{info$pkgver}.tar.gz"))

  ## Build the package with pkgbuild::build
  message(glue::glue("Building package {info$pkgname} version {info$pkgver} in {tarsrc}"))
  pkgbuild::build(path = info$pkgroot, dest_path = docker_dir, quiet=TRUE, ...)

  # Return info
  return(invisible(list(info=info, tarsrc=tarsrc)))

}

#' Build a Docker image
#'
#' @description
#' Builds a Docker image created by [use_docker] or [add_dockerfile]. This function is run as part of [use_docker] when `build = TRUE` is set, but can be used on its own.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param cache Logical; should caching be used? Default `TRUE`. Set to `FALSE` to use `--no-cache` in `docker build`.
#' @param tag Image tag to use; default is `NULL` and the image will be tagged with package name version from [pkg_info].
#' @param build Logical as to whether or not the image should be built. Default is `TRUE`, and if `FALSE` the `docker build` command will be messaged.  Setting `build=FALSE` could be useful if additional `docker build` options or different tags are desired. In either case the `docker build` command will be returned invisibly.
#'
#' @return Invisibly returns the `docker build` command. Primarily called for its side effects, which runs the `docker build` as a system command.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Run use_docker to create Docker directory and assets for the example package
#' use_docker(pkg_path = file.path(tempdir(), "hellow"))
#'
#' # Build the image
#' build_image(pkg_path = file.path(tempdir(), "hellow"))
#' # Or construct the image build command without building
#' build_cmd <- build_image(pkg_path = file.path(tempdir(), "hellow"), build=FALSE)
#' build_cmd
#' }
build_image <- function(pkg_path=".", img_path=NULL, cache=TRUE, tag=NULL, build=TRUE) {

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  # Check that a dockerfile exists
  dockerfilepath <- fs::path(docker_dir, "Dockerfile")
  if (!fs::file_exists(dockerfilepath)) stop(glue::glue("Dockerfile doesn't exist: {dockerfilepath}"))

  # Get package info
  info <- pkg_info(pkg_path)

  # Parse docker build options
  cache <- ifelse(cache, "", "--no-cache")

  # Construct tags and build command
  if(is.null(tag)) {
    image_tag1 <- paste0(info$pkgname, ":latest")
    image_tag2 <- paste0(info$pkgname, ":", info$pkgver)
    buildcmd <- glue::glue("docker build {cache} --tag {image_tag1} --tag {image_tag2} {docker_dir}")
  } else {
    image_tag <- tag
    buildcmd <- glue::glue("docker build {cache} --tag {image_tag} {docker_dir}")
  }

  # Message build command and run as a system command if not using a dry run
  message("docker build command:")
  message(buildcmd)
  if (build) {
    message("Building docker image...")
    system(buildcmd, ignore.stdout=TRUE)
  }

  # Return the build command as a character string (this is messaged)
  return(invisible(buildcmd))

}
