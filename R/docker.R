#' Function to create docker directory
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#'
#' @return (Invisible) A list of package info returned by [pkginfo]. Also called for side-effect, creates docker directory.
#' @export
#'
#' @examples
#' \dontrun{
#' create_docker_dir()
#' }
#'
create_docker_dir <- function(pkg_path = ".", img_path = NULL) {

  # Check that the path is a package
  info <- pkginfo(pkg_path)

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  ## if the directory already exists message that
  if (fs::dir_exists(docker_dir)) {
    message(glue::glue("Directory already exists: {docker_dir}"))
  } else {
    message(glue::glue("Creating docker directory: {docker_dir}"))
    fs::dir_create(docker_dir)
  }

  # Check that there's an .Rbuildignore
  # FIXME: if .Rbuildignore doesn't exist, perhaps we should create one
  ignore_fp <- fs::path(pkg_path, ".Rbuildignore")
  if(file.exists(ignore_fp)) {
    # Only append ^docker$ to .Rbuildignore if ^docker$ isn't already there
    if (!any(grepl("\\^docker\\$", readLines(ignore_fp)))) {
      message(glue::glue("Adding ^docker$ to {ignore_fp}"))
      write("^docker$", file = ignore_fp, append=TRUE)
    }
  } else {
    stop(glue::glue("The package at {pkg_path} is not configured to include a .Rbuildignore. docker directory cannot be ignored."))
  }

  # Invisibly return package information
  return(invisible(info))
}

#' Add a Dockerfile to the docker directory
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#' @param base_image Name of base image to start `FROM` in Dockerfile
#' @param use_renv Logical as to whether or not to use renv. Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file and the most recent versions will be installed in the image.
#' @param usecase One of the use case templates in inst/templates. Defaults to `NULL` -- no additional Dockerfile boilerplate is added.
#' @param repos Option to override the repos used for installing packages with `renv` by passing name of repository. Only used if `use_renv = TRUE`. Default is `NULL` meaning that the repos specified in `renv` lockfile will remain as-is and not be overridden.
#'
#' @return (Invisible) A list of package info returned by [pkginfo]. Also called for side-effect, creates Dockerfile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=TRUE)
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=TRUE, usecase="helloworld")
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=FALSE)
#' add_dockerfile(base_image="rocker/r-ver:4.2.2", use_renv=FALSE, usecase="helloworld")
#'
#' # Shiny
#' # FIXME: pare down this example
#' add_dockerfile(base_image="rocker/shiny:4.2.2", use_renv=FALSE, usecase="shiny")
#' fs::file_copy("inst/shiny/app.R", "docker", overwrite=TRUE)
#' build_image(cache=FALSE)
#' # Run container, go to <http://localhost:3838/>.
#'
#' }
add_dockerfile <- function(pkg_path = ".", img_path = NULL, base_image = "rocker/r-ver:latest", use_renv = TRUE, usecase=NULL, repos=NULL) {

  # Get canonical path
  pkg_path <- fs::path_real(pkg_path)

  # Check that path is a package
  info <- pkginfo(pkg_path)

  # Turn the string vector: c("a", "b", "c") to the single element string "'a','b','c'"
  pkgs <- paste(paste0("'",info$pkgdeps,"'"), collapse=",")

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  ## if the docker_dir specified above doesnt exist ... create it with the helper
  if(!fs::dir_exists(docker_dir)) {
    create_docker_dir(pkg_path = pkg_path, img_path = img_path)
  }

  ## create the dockerfile
  dockerfile_fp <- fs::path(docker_dir, "Dockerfile")
  invisible(fs::file_create(dockerfile_fp))

  # Choose a different base template depending on whether you're using renv
  if(use_renv) {
    if (!fs::file_exists(fs::path(docker_dir, "renv.lock"))) {
      stop(glue::glue("use_renv=TRUE but no renv.lock file found in {docker_dir}. Run renv_deps() to generate."))
    }
    message(glue::glue("Using renv. Dockerfile will build from renv.lock in {docker_dir}."))
    base_template_fp <- system.file("templates/base-renv.dockerfile", package = "pracpac", mustWork = TRUE)
  } else {
    message(glue::glue("Not using renv. Pulling package dependencies from description file: c({pkgs})"))
    base_template_fp <- system.file("templates/base.dockerfile", package = "pracpac", mustWork = TRUE)
  }

  # Read in the base template and create the dockerfile base using glue to pull in base image, other pkgs, pkg name and version
  base_template <- paste0(readLines(base_template_fp), collapse = "\n")
  ## NOTE: this step is important ...
  ## the NULL option should NOT be quoted when glued into template ...
  ## but the repo names should be quoted ..
  ## so we need to add a double layer of quotes in this statement if you specify repos
  repos <- ifelse(is.null(repos), 'NULL', paste0('"', repos, '"'))
  base_dockerfile <- glue::glue(base_template, base_image = base_image, pkgs = pkgs, pkgname=info$pkgname, pkgver=info$pkgver, repos = repos)

  # If usecase is NULL, no additional Dockerfile boilerplate is added.
  # If defined, look in inst/templates and read in that template. Note these are not parameterized with glue {} params.
  if (is.null(usecase)) {
    usecase_dockerfile <- ""
  } else {
    # Read in the use case template
    message(glue::glue("Using template: {usecase}"))
    usecase_template_fp <- system.file(glue::glue("templates/{usecase}.dockerfile"), package = "pracpac", mustWork = TRUE)
    usecase_dockerfile <- paste0(readLines(usecase_template_fp), collapse = "\n")
  }

  # Stitch the base dockerfile and usecase dockerfile together
  dockerfile_contents <- glue::glue(paste(base_dockerfile, usecase_dockerfile, sep="\n\n"))

  # Write dockerfile to disk
  message(glue::glue("Writing dockerfile: {dockerfile_fp}"))
  write(dockerfile_contents, file = dockerfile_fp, append = FALSE)

  # Invisibly return pkg info
  return(invisible(info))
}

#' Get renv dependencies
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`
#'
#' @return (Invisible) A list of package info returned by [pkginfo]. Primarily called for side effect. Writes an `renv` lock file to the docker/ directory.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' renv_deps()
#' }
renv_deps <- function(pkg_path = ".", img_path = NULL, other_packages = NULL) {

  # Get canonical path
  pkg_path <- fs::path_real(pkg_path)

  # Check that path is a package
  info <- pkginfo(pkg_path)

  ## get pkgname from pkginfo helper
  pkgname <- info$pkgname

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  ## if the docker_dir specified above doesnt exist ... create it with the helper
  if(!fs::dir_exists(docker_dir)) {
    create_docker_dir(pkg_path = pkg_path, img_path = img_path)
  }

  ## establish out path for the renv lock file
  out_path <- fs::path(docker_dir, "renv.lock")

  ## NOTE: need to pass a tempdir in otherwise renv can't find pkgname when run in current directory ...
  ## ... not sure exactly why that is but this seems to work
  message(glue::glue("Creating renv.lockfile with renv::snapshot: {out_path}"))
  if (!is.null(other_packages)) message(glue::glue("With additional packages: {paste(other_packages, collapse=', ')}"))
  renv::snapshot(project = tempdir(), packages = c(info$pkgdeps, other_packages), lockfile = out_path, prompt = FALSE, update = TRUE)

  # Invisibly return package information
  return(invisible(info))

}


#' Use docker packaging tools
#'
#' @param pkg_path Path to the package directory
#' @param img_path Path to the write the docker image definition contents; default `NULL` will use `docker/` as a sub-directory of the "pkg_path"
#' @param base_image Name of base image to start `FROM` in Dockerfile
#' @param use_renv Logical as to whether or not to use renv. Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file and the most recent versions will be installed in the image.
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`
#' @param build Logical as to wether or not the function should build the Docker image; default is `TRUE`
#' @param usecase One of the use case templates in inst/templates. Defaults to `NULL` -- no additional Dockerfile boilerplate is added
#' @param repos Option to override the repos used for installing packages with `renv` by passing name of repository. Only used if `use_renv = TRUE`. Default is `NULL` meaning that the repos specified in `renv` lockfile will remain as-is and not be overridden.
#'
#' @return (Invisible) A list with information about the package. Primarily called for side effect. Creates `docker/` directory, identifies renv dependencies and creates lock file (if `use_renv = TRUE`), writes Dockerfile, builds package tar.gz, moves all relevant assets to the `docker/` directory, and builds Docker image (if `build = TRUE`).
#' @export
#'
#' @examples
#' \dontrun{
#' use_docker()
#' }
use_docker <- function(pkg_path = ".", img_path = NULL, use_renv = TRUE, base_image = "rocker/r-ver:latest" , other_packages = NULL, build = TRUE , usecase = NULL, repos = NULL) {

  ## check the package path
  info <- pkginfo(pkg_path)

  ## if the image path is not given then construct path as subdirectory of pkg
  ## otherwise use the specified image path
  if(is.null(img_path)) {
    # Construct path to the docker directory
    docker_dir <- fs::path(pkg_path, "docker")
  } else {
    docker_dir <- fs::path(img_path)
  }

  ## if the docker_dir specified above doesnt exist ... create it with the helper
  if(!fs::dir_exists(docker_dir)) {
    create_docker_dir(pkg_path = pkg_path, img_path = img_path)
  }

  ## if using renv then make sure the renv_deps runs and outputs lockfile in docker/ dir
  if(use_renv) {
    renv_deps(pkg_path = pkg_path, img_path = img_path, other_packages = other_packages)
  }

  ## add the dockerfile to the docker/ dir
  add_dockerfile(pkg_path = pkg_path, img_path = img_path, use_renv = use_renv, base_image = base_image, usecase = usecase, repos = repos)

  ## build the package tar.gz and copy that to the docker dir/
  build_pkg(pkg_path = pkg_path, img_path = img_path)
  ## conditionally build the image
  if(build) {
    build_image(pkg_path = pkg_path, img_path = img_path)
  }

  # Invisibly return package info
  return(invisible(info))

}
