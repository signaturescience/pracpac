#' Function to create docker directory
#'
#' @param path Path to the package directory
#'
#' @return A list with information about the package. Also called for side-effect, creates docker directory.
#' @export
#'
#' @examples
#' \dontrun{
#' create_docker_dir()
#' }
#'
create_docker_dir <- function(path = ".") {
  # Check that the path is a package, then create a docker directory inside the package
  info <- pkginfo()
  fs::dir_create(fs::path(path, "docker"))

  ## NOTE: originally had an argument to conditionally add to Rbuildignore ... but why??
  ## we ALWAYS want this Rbuildignored i think
  ## that said lets chekc that the rbuildignore file is set up
  ignore_fp <- fs::path(path, ".Rbuildignore")
  if(file.exists(ignore_fp)) {
    # Only append ^docker$ to .Rbuildignore if ^docker$ isn't already there
    if (!any(grepl("\\^docker\\$", readLines(ignore_fp)))) {
      message(glue::glue("Adding ^docker$ to {ignore_fp}"))
      write("^docker$", file = ignore_fp, append=TRUE)
    }
  } else {
    stop(glue::glue("The package at {path} is not configured to include a .Rbuildignore. docker directory cannot be ignored."))
  }

  return(info)
}

#' Add a Dockerfile to the docker directory
#'
#' @param path Path to the package directory
#' @param base_image Name of base image to start FROM in Dockerfile
#' @param pkgs Vector of packages to include in Dockerfile; only relevant if `use_renv = FALSE`
#' @param use_renv Logical as to whether or not to use renv
#'
#' @return A list with information about the package. Also called for side-effect, creates Dockerfile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_dockerfile()
#' }
add_dockerfile <- function(path = ".", base_image = "rocker/r-ver:latest", pkgs = NULL, use_renv = TRUE) {

  # Get canonical path
  path <- fs::path_real(path)

  # Check that path is a package
  info <- pkginfo()

  # Create docker dir if it doesn't exist
  ddir_path <- fs::path(path, "docker")
  if(!dir.exists(ddir_path)) {
    create_docker_dir(path)
  }

  ## create the dockerfile
  dockerfile_fp <- fs::path(ddir_path, "Dockerfile")
  invisible(fs::file_create(dockerfile_fp))

  ## NOTE: conditionally pull different templates for renv or not
  if(use_renv) {
    template_fp <- system.file("templates/Dockerfile-renv.template", package = "pracpac")
    tmpl <- paste0(readLines(template_fp), collapse = "\n")

    dockerfile_contents <- glue::glue(tmpl, base_image = base_image)

  } else {
    template_fp <- system.file("templates/Dockerfile.template", package = "pracpac")
    tmpl <- paste0(readLines(template_fp), collapse = "\n")

    dockerfile_contents <- glue::glue(tmpl, base_image = base_image, pkgs = pkgs)
  }

  # FIXME: need some UI messaging here
  write(dockerfile_contents, file = dockerfile_fp, append = TRUE)

  return(info)
}

#' Get renv dependencies
#'
#' @param path Path to the package directory
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`
#'
#' @return A list with information about the package. Primarily called for side effect. Writes an `renv` lock file to the docker/ directory.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' renv_deps()
#' }
renv_deps <- function(path = ".", other_packages = NULL) {

  # Get canonical path
  path <- fs::path_real(path)

  # Check that path is a package
  info <- pkginfo()

  ## get pkgname from pkginfo helper
  pkgname <- pkginfo()$pkgname

  ## establish out path for the renv lock file
  out_path <- fs::path(path, "docker", "renv.lock")

  ## was thinking we should have a check that docker dir exists
  ## TODO: think through if this is the best way to handle this check
  if(!fs::dir_exists(fs::path(path, "docker"))) {
    stop("The docker/ dir does not exist.")
  }

  ## NOTE: storing the original libpath to reset libpaths at the end of this function
  lps <- .libPaths()

  ## establish tempdir path
  ## used as argument to with_tempdir below
  tmp <- tempdir()

  # FIXME better inline documentation
  withr::with_tempdir(clean=TRUE, tmpdir = tmp, code = {

    ## create the temp dir and file path with a script that loads pkg ...
    ## ... and other_packages as specified
    fs::dir_create(fs::path(tmp, pkgname))
    tmp_fp <- fs::path(tmp, pkgname, "tmp.R")
    fs::file_create(tmp_fp)

    # FIXME use glue for readability
    write(paste0("library(", pkgname, ")"),
          file = tmp_fp,
          append=TRUE)

    ## this allows for other packages that the user may want to be installed
    ## other_packages passed as a vector and we loop over that and append
    if(!is.null(other_packages)) {
      for(i in 1:length(other_packages)) {
        write(paste0("library(", other_packages[i], ")"),
              file = tmp_fp,
              append=TRUE)
      }
    }

    ## TODO: add a helper function that abstracts out the renv consent and sandbox options
    ## NOTE: side effect to set the option to NOT use sandbox
    Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
    renv::consent(provided = TRUE)
    renv::init(fs::path(tmp, pkgname), force = TRUE, restart = FALSE)

    renv::snapshot(fs::path(tmp, pkgname), lockfile = out_path, prompt = FALSE)

    # FIXME some UI messaging here

  })

  ## ensure that libpath is reset to initial state after renv::init side effects
  .libPaths(lps)

}
