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
  if (!fs::dir_exists(dir)) fs::dir_create(dir)
  info <- pkginfo()
  tarsrc <- sprintf("%s_%s.tar.gz", info$pkgname, info$pkgver)
  tardst <- sprintf("%s.tar.gz", info$pkgname)
  tardst <- file.path(dir, tardst)
  if (build) {
    system(paste("R CMD build", info$pkgroot), ignore.stdout=TRUE)
    fs::file_move(tarsrc, tardst)
    message(sprintf("Built package %s: %s moved to %s", info$pkgname, tarsrc, tardst))
  } else {
    message(sprintf("Would build package %s: %s move to %s", info$pkgname, tarsrc, tardst))
  }
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
  info <- pkginfo()
  cache <- ifelse(cache, "", "--no-cache")
  buildcmd <- sprintf("docker build %s --tag %s:latest --tag %s:%s %s", cache, info$pkgname, info$pkgname, info$pkgver, dir)
  message(buildcmd)
  if (build) {
    system(buildcmd, ignore.stdout=TRUE)
  }
}
