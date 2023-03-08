.onAttach <- function(libname, pkgname) {

  # Consent renv
  options(renv.consent = TRUE)

  # Are you using git?
  isgit <- fs::dir_exists(".git")
  if (!isgit) packageStartupMessage(glue::glue("{pkgname} writes files. Best to use git when using pracpac functions."))

  # Is this a package?
  ispkg <- !inherits(try(rprojroot::find_package_root_file(), silent=TRUE), "try-error")
  if (!ispkg) packageStartupMessage(glue::glue("{pkgname} is typically used within a package directory. See Vignettes."))

}
