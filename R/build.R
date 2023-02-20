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
#' build_pkg(dir="docker", build=FALSE)
#' \dontrun{
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

