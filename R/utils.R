#' Get information about the current package
#'
#' @description
#' Returns information about the current package in a list which can be passed to other functions.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
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
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # This will succeed if this is a package
#' pkg_info(pkg_path = file.path(tempdir(), "hellow"))
#' # This will fail if this is not a package location
#' pkg_info(pkg_path = tempdir())
#' }
pkg_info <- function(pkg_path=".", ...) {
  # Find the package root
  pkg_root <- pkg_root(pkg_path=pkg_path, ...)

  # Find the description file
  descfile <- fs::path(pkg_root, "DESCRIPTION")

  # Get package dependencies.
  # If there are no dependencies, make pkgdeps an empty string, character(0).
  # Both of these are perfectly valid, and result in nothing actually being installed:
  # BiocManager::install(c(character(0)), update=FALSE, ask=FALSE)
  # install.packages(c(character(0)))
  imports <- as.data.frame(read.dcf(descfile),stringsAsFactors=FALSE)$Imports
  if (is.null(imports)) {
    pkgdeps <- character(0)
  } else {
    imports <- strsplit(imports, split=",\\n")[[1]]
    # Strip out any version requirements
    pkgdeps <- sapply(imports, function(x) gsub("[ \\(<=>].*", "", x), USE.NAMES = FALSE)
  }
  # Get the name and version from it
  pkgname <- strsplit(grep("^Package:", readLines(descfile), value=TRUE), split=" ")[[1]][2]
  pkgver <- strsplit(grep("^Version:", readLines(descfile), value=TRUE), split=" ")[[1]][2]

  # Return a list
  return(list(pkgroot=pkg_root, pkgdeps=pkgdeps, descfile=descfile, pkgname=pkgname, pkgver=pkgver))
}

#' Find package root
#'
#' @description
#' Unexported helper to find the root of the R package. Returns an error if the path specified is not an R package.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param ... Arguments passed to [rprojroot::find_package_root_file].
#' @return A file path of the package root. If no package is found at the root then the function will `stop` with an error message.
#'
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
#' @description
#' This unexported helper function internally handles the provided use case.
#'
#' @param use_case The specified use case.
#'
#' @return List of parsed information for the use case including, the name of the use case, path to Dockerfile template, base image, and path to assets (delimited by `;` if there are multiple and `NA` if there are none).
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
