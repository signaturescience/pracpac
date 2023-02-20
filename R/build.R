#' Build a package tar.gz
#'
#' Build a package tar.gz and move it into a user-specified location (default `docker/`)
#'
#' @param dir Location for the built package tar.gz. Defaults to `docker/`
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' build_pkg()
#' }
build_pkg <- function(dir="docker") {
  if (!fs::dir_exists(dir)) fs::dir_create(dir)
  info <- pkginfo()
  tarsrc <- sprintf("%s_%s.tar.gz", info$pkgname, info$pkgver)
  tardst <- sprintf("%s.tar.gz", info$pkgname)
  tardst <- file.path(dir, tardst)
  system(paste("R CMD build", info$pkgroot), ignore.stdout=TRUE)
  fs::file_move(tarsrc, tardst)
  message(sprintf("Built package %s: %s moved to %s", info$pkgname, tarsrc, tardst))
}
