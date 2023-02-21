#' Get information about the current package
#'
#' Returns information about the current package in a list which can be passed to other functions.
#'
#' @param path Directory path to a package root
#' @param ... Arguments passed to [rprojroot::find_package_root_file]
#' @return A list fixme
#' @export
#' @examples
#' \dontrun{
#' # This will succeed if this is a package
#' pkginfo()
#' # This will fail if this is not a package location
#' pkginfo(tempdir())
#' }
pkginfo <- function(path=".", ...) {
  # Find the package root
  pkgroot <- pkgroot(path=path, ...)

  # Find the description file
  descfile <- fs::path(pkgroot, "DESCRIPTION")

  # Get package dependencies
  pkgdeps <- strsplit(as.data.frame(read.dcf(descfile))$Imports, split=",\\n")[[1]]

  # Get the name and version from it
  pkgname <- strsplit(grep("^Package:", readLines(descfile), value=TRUE), split=" ")[[1]][2]
  pkgver <- strsplit(grep("^Version:", readLines(descfile), value=TRUE), split=" ")[[1]][2]

  # Return a list
  return(list(pkgroot=pkgroot, pkgdeps=pkgdeps, descfile=descfile, pkgname=pkgname, pkgver=pkgver))
}

#' Find package root
#' @param path Directory path to a package root
#' @param ... Arguments passed to [rprojroot::find_package_root_file]
#' @return A file path of the package root.
pkgroot <- function(path=".", ...) {
  root <- try(rprojroot::find_package_root_file(path=path, ...), silent=TRUE)
  if (inherits(root, "try-error")) {
    stop(glue::glue("{fs::path_abs(path)} is not an R package."))
  } else {
    return(root)
  }
}
