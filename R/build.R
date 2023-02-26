#' Build a package tar.gz
#'
#' Build a package tar.gz and move it into a user-specified location (default `docker/`)
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#' @param ... Additional optional arguments passed to [pkgbuild::build]
#'
#' @return (Invisible) A list of package info returned by [pkg_info], tar.gz source and destination file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_pkg()
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
  pkgbuild::build(path = info$pkgroot, dest_path = docker_dir, ...)

  # Return info
  return(invisible(list(info=info, tarsrc=tarsrc)))

}

#' Build a Docker image
#'
#' Build a Docker image created by FIXME function name.
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#' @param cache Logical; should caching be used? Default `TRUE`. Set to `FALSE` to use `--no-cache` in `docker build`.
#'
#' @return (Invisible) The `docker build` command. Called for its side effects, which runs the `docker build` as a system command.
#' @export
#'
#' @examples
#' \dontrun{
#' build_image()
#' }
build_image <- function(pkg_path=".", img_path=NULL, cache=TRUE) {

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

  # Construct and run the build command as a system command
  buildcmd <- glue::glue("docker build {cache} --tag {info$pkgname}:latest --tag {info$pkgname}:{info$pkgver} {docker_dir}")
  message("Building docker image...")
  message(buildcmd)
  system(buildcmd, ignore.stdout=TRUE)

  # Return the build command as a character string (this is messaged)
  return(invisible(buildcmd))

}
