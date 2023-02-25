#' Get information about the current package
#'
#' Returns information about the current package in a list which can be passed to other functions.
#'
#' @param pkg_path Path to the package directory
#' @param ... Arguments passed to [rprojroot::find_package_root_file]
#' @return A list of information about the package
#' - `pkgroot`: Root directory of the package
#' - `pkgdeps`: Package dependencies from `Imports` in the `DESCRIPTION`
#' - `descfile`: File path to the `DESCRIPTION` file
#' - `pkgname`: Package name
#' - `pkgver`: Package version
#' @export
#' @examples
#' \dontrun{
#' # This will succeed if this is a package
#' pkginfo()
#' # This will fail if this is not a package location
#' pkginfo(tempdir())
#' }
pkginfo <- function(pkg_path=".", ...) {
  # Find the package root
  pkgroot <- pkgroot(pkg_path=pkg_path, ...)

  # Find the description file
  descfile <- fs::path(pkgroot, "DESCRIPTION")

  # Get package dependencies
  pkgdeps <- strsplit(as.data.frame(read.dcf(descfile),stringsAsFactors=FALSE)$Imports, split=",\\n")[[1]]

  # Get the name and version from it
  pkgname <- strsplit(grep("^Package:", readLines(descfile), value=TRUE), split=" ")[[1]][2]
  pkgver <- strsplit(grep("^Version:", readLines(descfile), value=TRUE), split=" ")[[1]][2]

  # Return a list
  return(list(pkgroot=pkgroot, pkgdeps=pkgdeps, descfile=descfile, pkgname=pkgname, pkgver=pkgver))
}

#' Find package root
#' @param pkg_path Path to the package directory
#' @param ... Arguments passed to [rprojroot::find_package_root_file]
#' @return A file path of the package root.
pkgroot <- function(pkg_path=".", ...) {
  root <- try(rprojroot::find_package_root_file(path=pkg_path, ...), silent=TRUE)
  if (inherits(root, "try-error")) {
    stop(glue::glue("{fs::path_abs(pkg_path)} is not an R package."))
  } else {
    return(root)
  }
}
