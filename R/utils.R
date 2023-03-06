#' Get information about the current package
#'
#' Returns information about the current package in a list which can be passed to other functions.
#'
#' @param pkg_path Path to the package directory.
#' @param ... Arguments passed to [rprojroot::find_package_root_file].
#' @return A list of information about the package.
#' - `pkgroot`: Root directory of the package.
#' - `pkgdeps`: Package dependencies from `Imports` in the `DESCRIPTION`.
#' - `descfile`: File path to the `DESCRIPTION` file.
#' - `pkgname`: Package name.
#' - `pkgver`: Package version.
#' @export
#' @examples
#' \dontrun{
#' # This will succeed if this is a package
#' pkg_info()
#' # This will fail if this is not a package location
#' pkg_info(tempdir())
#' }
pkg_info <- function(pkg_path=".", ...) {
  # Find the package root
  pkg_root <- pkg_root(pkg_path=pkg_path, ...)

  # Find the description file
  descfile <- fs::path(pkg_root, "DESCRIPTION")

  # Get package dependencies
  pkgdeps <- strsplit(as.data.frame(read.dcf(descfile),stringsAsFactors=FALSE)$Imports, split=",\\n")[[1]]

  # Get the name and version from it
  pkgname <- strsplit(grep("^Package:", readLines(descfile), value=TRUE), split=" ")[[1]][2]
  pkgver <- strsplit(grep("^Version:", readLines(descfile), value=TRUE), split=" ")[[1]][2]

  # Return a list
  return(list(pkgroot=pkg_root, pkgdeps=pkgdeps, descfile=descfile, pkgname=pkgname, pkgver=pkgver))
}

#' Find package root
#' @param pkg_path Path to the package directory.
#' @param ... Arguments passed to [rprojroot::find_package_root_file].
#' @return A file path of the package root.
pkg_root <- function(pkg_path=".", ...) {
  root <- try(rprojroot::find_package_root_file(path=pkg_path, ...), silent=TRUE)
  if (inherits(root, "try-error")) {
    stop(glue::glue("{fs::path_abs(pkg_path)} is not an R package."))
  } else {
    return(root)
  }
}

#' Handle the use case
#'
#' This unexported helper function internally handles the provided use case
#'
#' @param use_case The specified use case.
#'
#' @return List of parsed information for the use case including, the name of the use case, path to Dockerfile template, base image, and path to assets (delimited by `;` if there are multiple and `NA` if there are none).
#'
#' @examples
#' \dontrun{
#' handle_use_case("default")
#' handle_use_case("pipeline")
#' handle_use_case("shiny")
#' handle_use_case("rstudio")
#' handle_use_case("no-such-use-case")
#' }
#'
handle_use_case <- function(use_case) {

  ## remove possible casing issues
  use_case <- tolower(use_case)

  ## define possible use cases based on the use_cases internal data object
  all_use_cases <- use_cases$use_case
  ## created a collapsed string for messaging below if needed
  all_collapsed <- paste0(all_use_cases, collapse  = ",")

  ## validate that use case is among list of possible use cases supported
  if(!use_case %in% all_use_cases) {
    stop(glue::glue("Use case must be one of: {all_collapsed}"))
  }

  ## get use case specifications from internal data object
  use_case_specs <- use_cases[use_cases$use_case == use_case, ]
  return(
    list(
      use_case = use_case_specs$use_case,
      template = use_case_specs$template,
      base_image = use_case_specs$base_image,
      assets = use_case_specs$assets
    )
  )

}
