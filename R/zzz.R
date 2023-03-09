.onAttach <- function(libname, pkgname) {

  # Are you using git?
  isgit <- fs::dir_exists(".git")
  if (!isgit) packageStartupMessage(glue::glue("{pkgname} writes files. Best to use git when using {pkgname} functions."))

  # Is this a package?
  ispkg <- !inherits(try(rprojroot::find_package_root_file(), silent=TRUE), "try-error")
  if (!ispkg) packageStartupMessage(glue::glue("{pkgname} is typically used within a package directory. See Vignettes."))

}
