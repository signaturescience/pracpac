#' Get information about the current package
#'
#' Returns information about the current package in a list which can be passed to other functions.
#'
#' @param ... Arguments passed to [rprojroot::find_package_root_file]. E.g. `path="/path/to/dir"`.
#' @return A list fixme
#' @export
#' @examples
#' \dontrun{
#' pkginfo()
#' }
pkginfo <- function(...) {
  # Fixme: wrap this in try, fail gracefully if this is not a package
  pkgroot <- rprojroot::find_package_root_file(...)

  # Find the description file
  descfile <- file.path(pkgroot, "DESCRIPTION")

  # Get the name and version from it
  pkgname <- strsplit(grep("^Package:", readLines(descfile), value=TRUE), split=" ")[[1]][2]
  pkgver <- strsplit(grep("^Version:", readLines(descfile), value=TRUE), split=" ")[[1]][2]

  # Return a list
  return(list(pkgroot=pkgroot, descfile=descfile, pkgname=pkgname, pkgver=pkgver))
}
