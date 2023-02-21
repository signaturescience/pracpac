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
#' @param base_image Name of base image to start `FROM` in Dockerfile
#' @param use_renv Logical as to whether or not to use renv. Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file and the most recent versions will be installed in the image.
#' @param usecase One of the use case templates in inst/templates. Defaults to `NULL` -- no additional Dockerfile boilerplate is added.
#'
#' @return A list with information about the package. Also called for side-effect, creates Dockerfile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=TRUE)
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=TRUE, usecase="helloworld")
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=FALSE)
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=FALSE, usecase="helloworld")
#' }
add_dockerfile <- function(path = ".", base_image = "rocker/r-ver:latest", use_renv = TRUE, usecase=NULL) {

  # Get canonical path
  path <- fs::path_real(path)

  # Check that path is a package
  info <- pkginfo()

  # Turn the string vector: c("a", "b", "c") to the single element string "'a','b','c'"
  pkgs <- paste(paste0("'",info$pkgdeps,"'"), collapse=",")

  # Create docker dir if it doesn't exist
  ddir_path <- fs::path(path, "docker")
  if(!dir.exists(ddir_path)) {
    create_docker_dir(path)
  }

  ## create the dockerfile
  dockerfile_fp <- fs::path(ddir_path, "Dockerfile")
  invisible(fs::file_create(dockerfile_fp))

  # Choose a different base template depending on whether you're using renv
  if(use_renv) {
    if (!fs::file_exists(fs::path(ddir_path, "renv.lock"))) {
      stop(glue::glue("use_renv=TRUE but no renv.lock file found in {ddir_path}. Run renv_deps() to generate."))
    }
    message(glue::glue("Using renv. Dockerfile will build from renv.lock in {ddir_path}."))
    base_template_fp <- system.file("templates/base-renv.dockerfile", package = "pracpac", mustWork = TRUE)
  } else {
    message(glue::glue("Not using renv. Pulling package dependencies from description file: c({pkgs})"))
    base_template_fp <- system.file("templates/base.dockerfile", package = "pracpac", mustWork = TRUE)
  }

  # Read in the base template and create the dockerfile base using glue to pull in base image, other pkgs, pkg name and version
  base_template <- paste0(readLines(base_template_fp), collapse = "\n")
  base_dockerfile <- glue::glue(base_template, base_image = base_image, pkgs = pkgs, pkgname=info$pkgname, pkgver=info$pkgver)

  # If usecase is NULL, no additional Dockerfile boilerplate is added.
  # If defined, look in inst/templates and read in that template. Note these are not parameterized with glue {} params.
  if (is.null(usecase)) {
    usecase_dockerfile <- ""
  } else {
    # Read in the use case template
    usecase_template_fp <- system.file(glue::glue("templates/{usecase}.dockerfile"), package = "pracpac", mustWork = TRUE)
    usecase_dockerfile <- paste0(readLines(usecase_template_fp), collapse = "\n")
  }

  # Stitch the base dockerfile and usecase dockerfile together
  dockerfile_contents <- glue::glue(paste(base_dockerfile, usecase_dockerfile, sep="\n\n"))

  # FIXME: need some UI messaging here
  write(dockerfile_contents, file = dockerfile_fp, append = FALSE)

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
  info <- pkginfo(path)

  ## get pkgname from pkginfo helper
  pkgname <- info$pkgname

  ## establish out path for the renv lock file
  out_path <- fs::path(path, "docker", "renv.lock")

  ## was thinking we should have a check that docker dir exists
  ## TODO: think through if this is the best way to handle this check
  if(!fs::dir_exists(fs::path(path, "docker"))) {
    stop("The docker/ dir does not exist.")
  }

  ## NOTE: need to pass a tempdir in otherwise renv can't find pkgname when run in current directory ...
  ## ... not sure exactly why that is but this seems to work
  renv::snapshot(project = tempdir(), packages = c(info$pkgdeps, other_packages), lockfile = out_path, prompt = FALSE, update = TRUE)

  # FIXME some UI messaging here

}


#' Use docker packaging tools
#'
#' @param path Path to the package directory
#' @param base_image Name of base image to start `FROM` in Dockerfile
#' @param use_renv Logical as to whether or not to use renv. Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file and the most recent versions will be installed in the image.
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`
#' @param build Logical as to wether or not the function should build the Docker image; default is `TRUE`
#'
#' @return
#'
#' Side effects. Creates `docker/` directory, identifies renv dependencies and creates lock file (if `use_renv = TRUE`), writes Dockerfile, builds package tar.gz, moves all relevant assets to the `docker/` directory, and builds Docker image (if `build = TRUE`).
#' @export
#'
#' @examples
#' \dontrun{
#' use_docker()
#' }
use_docker <- function(path = ".", use_renv = TRUE, base_image = "rocker/r-ver:latest" , other_packages = NULL, build = TRUE) {

  ## check the package path
  info <- pkginfo(path)

  ## create docker/ dir
  create_docker_dir(path)

  ## if using renv then make sure the renv_deps runs and outputs lockfile in docker/ dir
  if(use_renv) {
    renv_deps(path = path, other_packages = other_packages)
  }

  ## add the dockerfile to the docker/ dir
  add_dockerfile(path = path, use_renv = use_renv, base_image = base_image)

  ## build the package tar.gz and copy that to the docker dir/
  build_pkg()
  ## conditionally build the image
  if(build) {
    build_image()
  }
}
