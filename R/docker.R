#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
create_ddir <- function(path = getwd()) {
  ## NOTE: path is just getwd()
  ## need to make this configurable with resolve_path
  ## that resolve_path will also have a check to make sure the path is a pkg
  dir.create(file.path(path, "docker"))

  ## NOTE: originally had an argument to conditionally add to Rbuildignore ... but why??
  ## we ALWAYS want this Rbuildignored i think
  ## that said lets chekc that the rbuildignore file is set up
  ignore_fp <- file.path(path, ".Rbuildignore")
  if(file.exists(ignore_fp)) {
      write("^docker$", file = ignore_fp, append=TRUE)
  } else {
    stop(sprintf("The package at %s is not configured to include a .Rbuildignore. docker directory cannot be ignored.", path))
    }
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
add_dockerfile <- function(path = getwd(), base_image = "rocker/r-ver:latest") {

  ddir_path <- file.path(path, "docker")

  if(!dir.exists(ddir_path)) {
    create_ddir(path)
  }

  ## create the dockerfile
  dockerfile_fp <- file.path(ddir_path, "Dockerfile")
  invisible(file.create(dockerfile_fp))

  ## NOTE we can do better with a template and glue::glue
  write(c(paste0("FROM ", base_image)
          ),
        sep = "\n",
        file=dockerfile_fp,
        append = TRUE)

}
