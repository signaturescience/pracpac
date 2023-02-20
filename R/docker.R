#' Function to create docker directory
#'
#' @param path Path to the package directory
#'
#' @return
#' Side-effect. Creates directory.
#' @export
#'
#' @examples
#' \dontrun{
#' create_ddir()
#' }
#'
create_ddir <- function(path = getwd()) {
  ## NOTE: path is just getwd()
  ## need to make this configurable with resolve_path
  ## that resolve_path will also have a check to make sure the path is a pkg
  fs::dir_create(fs::path(path, "docker"))

  ## NOTE: originally had an argument to conditionally add to Rbuildignore ... but why??
  ## we ALWAYS want this Rbuildignored i think
  ## that said lets chekc that the rbuildignore file is set up
  ignore_fp <- fs::path(path, ".Rbuildignore")
  if(file.exists(ignore_fp)) {
      write("^docker$", file = ignore_fp, append=TRUE)
  } else {
    stop(sprintf("The package at %s is not configured to include a .Rbuildignore. docker directory cannot be ignored.", path))
    }
}

#' Add a Dockerfile to the docker directory
#'
#' @param path Path to the package directory
#' @param base_image Name of base image to start FROM in Dockerfile
#' @param pkgs Vector of packages to include in Dockerfile; only relevant if `use_renv = FALSE`
#' @param use_renv Logical as to whether or not to use renv
#'
#' @return
#'
#' Side-effect. Creates directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_dockerfile()
#' }
add_dockerfile <- function(path = getwd(), base_image = "rocker/r-ver:latest", pkgs = NULL, use_renv = TRUE) {

  ddir_path <- fs::path(path, "docker")

  if(!dir.exists(ddir_path)) {
    create_ddir(path)
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
  write(dockerfile_contents, file = dockerfile_fp, append = TRUE)

}
