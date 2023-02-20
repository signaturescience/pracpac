#' Build a package tar.gz
#'
#' Build a package tar.gz and move it into a user-specified location (default `docker/`)
#'
#' @param dir Location for the built package tar.gz. Defaults to `docker/`
#' @param build Logical; should the package actually be built? Default `TRUE`. Set to `FALSE` for debugging.
#'
#' @return A list of package info returned by [pkginfo], tar.gz source and destination file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_pkg(dir="docker", build=FALSE)
#' build_pkg(dir="docker", build=TRUE)
#' }
build_pkg <- function(dir="docker", build=TRUE) {

  # Create the target directory if it doesn't already exist
  if (!fs::dir_exists(dir)) fs::dir_create(dir)

  # Get package information and construct filepaths to file built by R CMD build and eventual package tar.gz
  info <- pkginfo()
  tarsrc <- file.path(glue::glue("{info$pkgname}_{info$pkgver}.tar.gz"))
  tardst <- file.path(dir, glue::glue("{info$pkgname}.tar.gz"))

  # Build the package
  if (build) {
    message(glue::glue("Bulding package {info$pkgname}: {tarsrc} moved to {tardst}"))
    system(paste("R CMD build", info$pkgroot), ignore.stdout=TRUE)
    fs::file_move(tarsrc, tardst)
  }

  # Return info
  return(list(info=info, tarsrc=tarsrc, tardst=tardst))

}

#' Build a Docker image
#'
#' Build a Docker image created by FIXME function name.
#'
#' @param dir Directory containing the Dockerfile built by FIXME
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
build_image <- function(dir="docker", build=TRUE, cache=TRUE) {

  # Check that a dockerfile exists
  dockerfilepath <- file.path(dir, "Dockerfile")
  if (!fs::file_exists(dockerfilepath)) stop(glue::glue("Dockerfile doesn't exist: {dockerfilepath}"))

  # Get package info
  info <- pkginfo()

  # Parse docker build options
  cache <- ifelse(cache, "", "--no-cache")

  # Construct and run the build command as a system command
  buildcmd <- glue::glue("docker build {cache} --tag {info$pkgname}:latest --tag {info$pkgname}:{info$pkgver} {dir}")
  message(buildcmd)
  if (build) {
    system(buildcmd, ignore.stdout=TRUE)
  }

}
