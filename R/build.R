#' Build a package tar.gz
#'
#' Build a package tar.gz and move it into a user-specified location (default `docker/`)
#'
#' @param path Path to the package directory
#'
#' @return (Invisible) A list of package info returned by [pkginfo], tar.gz source and destination file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_pkg()
#' }
build_pkg <- function(path=".") {

  # Construct path to the docker directory
  docker_dir <- fs::path(path, "docker")

  # Create the target directory if it doesn't already exist
  if (!fs::dir_exists(docker_dir)) create_docker_dir(path)

  # Get package information and construct filepaths to file built by R CMD build and eventual package tar.gz
  info <- pkginfo()
  tarsrc <- file.path(glue::glue("{info$pkgname}_{info$pkgver}.tar.gz"))
  tardst <- file.path(docker_dir, glue::glue("{info$pkgname}.tar.gz"))

  message(glue::glue("Bulding package {info$pkgname} version {info$pkgver} in {docker_dir}/{tarsrc}"))
  system(paste("R CMD build", info$pkgroot), ignore.stdout=TRUE)
  fs::file_move(tarsrc, docker_dir)

  # Return info
  return(invisible(list(info=info, tarsrc=tarsrc)))

}

#' Build a Docker image
#'
#' Build a Docker image created by FIXME function name.
#'
#' @param path Path to a package directory containing a `docker` subdirectory, which contains the Dockerfile built by [add_dockerfile]
#' @param cache Logical; should caching be used? Default `TRUE`. Set to `FALSE` to use `--no-cache` in `docker build`.
#'
#' @return (Invisible) The `docker build` command. Called for its side effects, which runs the `docker build` as a system command.
#' @export
#'
#' @examples
#' \dontrun{
#' build_image()
#' }
build_image <- function(path=".", cache=TRUE) {

  # Construct path to the docker directory
  docker_dir <- fs::path(path, "docker")

  # Check that a dockerfile exists
  dockerfilepath <- file.path(docker_dir, "Dockerfile")
  if (!fs::file_exists(dockerfilepath)) stop(glue::glue("Dockerfile doesn't exist: {dockerfilepath}"))

  # Get package info
  info <- pkginfo()

  # Parse docker build options
  cache <- ifelse(cache, "", "--no-cache")

  # Construct and run the build command as a system command
  buildcmd <- glue::glue("docker build {cache} --tag {info$pkgname}:latest --tag {info$pkgname}:{info$pkgver} {docker_dir}")
  message("Building docker image...")
  message(buildcmd)
  system(buildcmd, ignore.stdout=TRUE)

  return(invisible(buildcmd))

}
