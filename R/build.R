#' Build a package tar.gz
#'
#' Build a package tar.gz and move it into a user-specified location (default `docker/`)
#'
#' @param path Path to the package directory
#' @param build Logical; should the package actually be built? Default `TRUE`. Set to `FALSE` for debugging.
#'
#' @return A list of package info returned by [pkginfo], tar.gz source and destination file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_pkg(build=FALSE)
#' build_pkg(build=TRUE)
#' }
build_pkg <- function(path=".", build=TRUE) {

  # Construct path to the docker directory
  docker_dir <- fs::path(path, "docker")

  # Create the target directory if it doesn't already exist
  if (!fs::dir_exists(docker_dir)) create_docker_dir(path)

  # Get package information and construct filepaths to file built by R CMD build and eventual package tar.gz
  info <- pkginfo()
  tarsrc <- file.path(glue::glue("{info$pkgname}_{info$pkgver}.tar.gz"))
  tardst <- file.path(docker_dir, glue::glue("{info$pkgname}.tar.gz"))

  # Build the package
  if (build) {
    message(glue::glue("Bulding package {info$pkgname} version {info$pkgver}: {tarsrc}"))
    system(paste("R CMD build", info$pkgroot), ignore.stdout=TRUE)
    fs::file_move(tarsrc, docker_dir)
  }

  # Return info
  return(list(info=info, tarsrc=tarsrc))

}

#' Build a Docker image
#'
#' Build a Docker image created by FIXME function name.
#'
#' @param docker_dir Directory containing the Dockerfile built by [add_dockerfile]
#' @param build Logical; should the image actually be built? Default `TRUE`. Set to `FALSE` for debugging.
#' @param cache Logical; should caching be used? Default `TRUE`. Set to `FALSE` to use `--no-cache` in `docker build`.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' build_image(build=FALSE)
#' build_image()
#' }
build_image <- function(docker_dir="docker", build=TRUE, cache=TRUE) {

  # Check that a dockerfile exists
  dockerfilepath <- file.path(docker_dir, "Dockerfile")
  if (!fs::file_exists(dockerfilepath)) stop(glue::glue("Dockerfile doesn't exist: {dockerfilepath}"))

  # Get package info
  info <- pkginfo()

  # Parse docker build options
  cache <- ifelse(cache, "", "--no-cache")

  # Construct and run the build command as a system command
  buildcmd <- glue::glue("docker build {cache} --tag {info$pkgname}:latest --tag {info$pkgname}:{info$pkgver} {docker_dir}")
  message(buildcmd)
  if (build) {
    system(buildcmd, ignore.stdout=TRUE)
  }

}
