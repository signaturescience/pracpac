#' Create Docker directory
#'
#' @description
#' Creates a `docker/` directory for a given package. By default, assumes that `docker/` should be a subdirectory of the specified package path.
#'
#' @details
#' This function is run as part of [use_docker] but can be used on its own.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @return Invisibly returns a list of package info returned by [pkg_info]. Primarily called for side-effect to create docker directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Assuming default behavior then docker/ will be created under source root
#' create_docker_dir(pkg_path = file.path(tempdir(), "hellow"))
#'
#' # Alternatively you can specify another directory above, below, or beside package source
#' create_docker_dir(pkg_path = file.path(tempdir(), "hellow"), img_path = file.path(tempdir(), "img"))
#' }
#'
create_docker_dir <- function(pkg_path = ".", img_path = NULL) {

  # Check that the path is a package
  info <- pkg_info(pkg_path)

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

  # If .Rbuildignore doesn't exist, perhaps we should create one
  ignore_fp <- fs::path(pkg_path, ".Rbuildignore")
  if(!file.exists(ignore_fp)) {
    fs::file_create(ignore_fp)
    message(glue::glue("Created {ignore_fp} in package {pkg_path}."))
  }
  # Only append ^docker$ to .Rbuildignore if ^docker$ isn't already there
  if (!any(grepl("\\^docker\\$", readLines(ignore_fp)))) {
    message(glue::glue("Adding ^docker$ to {ignore_fp}"))
    write("^docker$", file = ignore_fp, append=TRUE)
  }

  # Invisibly return package information
  return(invisible(info))
}

#' Add a Dockerfile to the docker directory
#'
#' @description
#' Adds a Dockerfile to the docker directory created by [create_docker_dir].
#' Allows for specification of several preset use cases, whether or not use use
#' renv to manage dependencies, and optional overriding the base image.
#'
#' @details
#' This function is run as part of [use_docker] but can be used on its own.
#'
#' See `vignette("use-cases", package="pracpac")` for details on use cases.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param use_renv Logical; use renv? Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file and the most recent versions will be installed in the image.
#' @param use_case Name of the use case. Defaults to `"default"`, which only uses the base boilerplate. See `vignette("use-cases", package="pracpac")` for other use cases (e.g., `shiny`, `rstudio`, `pipeline`).
#' @param base_image Name of the base image to start `FROM`. Default is `NULL` and the base image will be derived based on `use_case.` Optionally override this by setting the name of the base image (including tag if desired).
#' @param repos Option to override the repos used for installing packages with `renv` by passing name of repository. Only used if `use_renv = TRUE`. Default is `NULL` meaning that the repos specified in `renv` lockfile will remain as-is and not be overridden.
#'
#' @return Invisibly returns a list of package info returned by [pkg_info]. Primarily called for side-effect to create Dockerfile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Default: FROM rocker/r-ver:latest with no additional template
#' # By default add_dockerfile requires you either to specify use_renv = FALSE
#' # Or run renv_deps() prior to add_dockerfile()
#' # The use_docker() wrapper runs these sequentially, and is recommended for most usage
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), use_renv = FALSE)
#' # Specify tidyverse base image
#' renv_deps(pkg_path = file.path(tempdir(), "hellow"))
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), base_image="rocker/tidyverse:4.2.2")
#' # Specify different default repo
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), repos="https://cran.wustl.edu/")
#' # RStudio template
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), use_case="rstudio")
#' # Shiny template
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), use_case = "shiny")
#' # Pipeline template
#' add_dockerfile(pkg_path = file.path(tempdir(), "hellow"), use_case="pipeline")
#' }
add_dockerfile <- function(pkg_path = ".", img_path = NULL, use_renv = TRUE, use_case="default", base_image = NULL, repos=NULL) {

  # handle the use_case argument
  ## this first check if the use case is valid
  ## then will pull out relevant specs (stored as a named list) that we can use later to construct dockerfile
  use_case_specs <- handle_use_case(use_case)

  # Get canonical path
  pkg_path <- fs::path_real(pkg_path)

  # Check that path is a package
  info <- pkg_info(pkg_path)

  # Turn the string vector: c("a", "b", "c") to the single element string "'a','b','c'".
  # If info$pkgdeps is empty, create a string 'character(0)' that gets glued into the dockerfile template.
  # Note this must be a quoted string 'character(0)', not an empty string character(0), for the glue to work properly.
  if (length(info$pkgdeps==0L)) {
    pkgs <- paste(paste0('"', info$pkgdeps,'"'), collapse=',')
  } else {
    pkgs <- 'character(0)'
  }

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
    base_template_fp <- system.file("templates/base/base-renv.dockerfile", package = "pracpac", mustWork = TRUE)
  } else {
    message(glue::glue("Not using renv. Pulling package dependencies from description file: c({pkgs})"))
    base_template_fp <- system.file("templates/base/base.dockerfile", package = "pracpac", mustWork = TRUE)
  }

  ## handle base image ...
  ## using either the "base_image" argument (if not NULL) ...
  ## or the base_image from use_case_specs
  if(is.null(base_image)) {
    base_image <- use_case_specs$base_image
  }

  # Read in the base template and create the dockerfile base using glue to pull in base image, other pkgs, pkg name and version
  base_template <- paste0(readLines(base_template_fp), collapse = "\n")
  ## NOTE: this step is important ...
  ## the NULL option should NOT be quoted when glued into template ...
  ## but the repo names should be quoted ..
  ## so we need to add a double layer of quotes in this statement if you specify repos
  repos <- ifelse(is.null(repos), 'NULL', paste0('"', repos, '"'))
  base_dockerfile <- glue::glue(base_template, base_image = base_image, pkgs = pkgs, pkgname=info$pkgname, pkgver=info$pkgver, repos = repos)

  ## check the use case in use_case_specs (see above for parsing with handle_use_case)
  ## if it is default then just use the base dockerfile
  ## if not then find the template and append that to the base dockerfile
  if (use_case_specs$use_case == "default") {
    dockerfile_contents <- base_dockerfile
  } else {
    # Read in the use case template
    message(glue::glue("Using template for the specified use case: {use_case_specs$use_case}"))
    use_case_template_fp <- system.file(use_case_specs$template, package = "pracpac", mustWork = TRUE)
    use_case_dockerfile <- paste0(readLines(use_case_template_fp), collapse = "\n")
    # Stitch the base dockerfile and use_case dockerfile together
    dockerfile_contents <- glue::glue(paste(base_dockerfile, use_case_dockerfile, sep="\n\n"))
  }

  # Write dockerfile to disk
  message(glue::glue("Writing dockerfile: {dockerfile_fp}"))
  write(dockerfile_contents, file = dockerfile_fp, append = FALSE)

  # Invisibly return pkg info
  return(invisible(info))
}

#' Get dependencies using renv
#'
#' @description
#' Get dependencies using renv. This function will inspect your package specified
#' at `pkg_path` (default is current working directory, `.`), and create an renv lock file (`renv.lock`) in
#' the `docker/` directory. More information about the `renv` implementation is provided in the Details section.
#
#' @details
#' The `renv.lock` file will capture all your package's dependencies (and all
#' their dependencies) at the current version installed on your system at the
#' time this function is run. When using the default `use_renv=TRUE` in
#' [use_docker] or [add_dockerfile], the resulting `Dockerfile` will install
#' packages from this `renv.lock` file using [renv::restore]. This ensures that
#' versions of dependencies in the image mirror what is installed on your system
#' at the time of image creation, rather than potentially newer versions on package repositories like
#' CRAN or Bioconductor, which may come with breaking changes that you are unaware of at the
#' time of package development.
#'
#' If there are additional R packages that may be useful for the Docker image you plan to build (but may not be captured under your package dependencies), then you can add these packages to the `renv` procedure with the "other_packages" argument.
#'
#' This function is run as part of [use_docker] but can be used on its own.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`.
#' @param overwrite Logical; should an existing lock file should be overwritten? Default is `TRUE`.
#' @param consent_renv Logical; give renv consent in this session with `options(renv.consent = TRUE)`? Default is `TRUE`. See [renv::consent] for details.
#'
#' @return Invisibly returns a list of package info returned by [pkg_info]. Primarily called for side effect. Writes an `renv` lock file to the docker/ directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Run using defaults; only gets current package dependencies
#' renv_deps(pkg_path = file.path(tempdir(), "hellow"))
#' # Add additional packages not explicitly required by your package
#' renv_deps(pkg_path = file.path(tempdir(), "hellow"), other_packages=c("shiny", "knitr"))
#' }
renv_deps <- function(pkg_path = ".", img_path = NULL, other_packages = NULL, overwrite = TRUE, consent_renv=TRUE) {

  # Consent renv
  if (consent_renv) {
    options(renv.consent = TRUE)
  }

  # Get canonical path
  pkg_path <- fs::path_real(pkg_path)

  # Check that path is a package
  info <- pkg_info(pkg_path)

  ## get pkgname from pkg_info helper
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

  ## check if existing lockfile should be retained
  if(fs::file_exists(out_path)) {
    if(!overwrite) {
      message(glue::glue("Overwrite option is set to FALSE and lock file exists: {out_path}"))
      return(invisible(info))
    } else {
      message(glue::glue("Overwrite option is set to TRUE and existing lock file will be written: {out_path}"))
    }
  }
  ## NOTE: need to pass a tempdir in otherwise renv can't find pkgname when run in current directory ...
  ## ... not sure exactly why that is but this seems to work
  message(glue::glue("Creating renv lock file with renv::snapshot: {out_path}"))
  if (!is.null(other_packages)) message(glue::glue("With additional packages: {paste(other_packages, collapse=', ')}"))
  renv::snapshot(project = tempdir(), packages = c(info$pkgdeps, other_packages), lockfile = out_path, prompt = FALSE, update = TRUE)

  # Invisibly return package information
  return(invisible(info))

}


#' Add assets for the specified use case
#'
#' @description
#' Add template assets for the use case specified in [add_dockerfile] or [use_docker].
#'
#' @details
#' Example #1: the `"shiny"` use case requires than an `app.R` file moved into
#' `/srv/shiny-server/` in the container image. Using `add_assets(use_case="shiny")`
#' (or when using the `"shiny"` use case in [add_dockerfile] or [use_docker])
#' will create a placeholder `assets/app.R` in the `docker/` directory. The
#' Dockerfile for the `"shiny"` use case will place `COPY assets/app.R/srv/shiny-server` into the Dockerfile.
#'
#' Example #2: the `"pipeline"` use case creates boilerplate for moving pre- and
#' post-processing R and shell scripts into the container at
#' `add_assets(use_case="pipeline")` (or when using the `"pipeline"` use case in
#' [add_dockerfile] or [use_docker]) will create a placeholder `assets/pre.R`,
#' `assets/post.R`, and `assets/run.sh` into the `docker/assets` directory. The
#' Dockerfile for the `"pipeline"` use case will place `COPY assets/run.sh /run.sh` into the Dockerfile.
#'
#' This function is run as part of [use_docker] but can be used on its own.
#'
#' See `vignette("use-cases", package="pracpac")` for details on use cases.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param use_case Name of the use case. Defaults to `"default"`, which only uses the base boilerplate.
#' @param overwrite Logical; should existing assets should be overwritten? Default is `TRUE`.
#'
#' @return Invisibly returns assets per [handle_use_case]. Called primarily for its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Add assets for shiny use case
#' add_assets(pkg_path = file.path(tempdir(), "hellow"), use_case="shiny")
#' # Add assets for pipeline use case
#' add_assets(pkg_path = file.path(tempdir(), "hellow"), use_case="pipeline")
#' }
add_assets <- function(pkg_path = ".", img_path = NULL, use_case = "default", overwrite = TRUE) {

  # handle the use_case argument
  ## this first check if the use case is valid
  ## then will pull out relevant specs (stored as a named list) that we can use later to construct dockerfile
  use_case_specs <- handle_use_case(use_case)

  # Get canonical path
  pkg_path <- fs::path_real(pkg_path)

  # Check that path is a package
  info <- pkg_info(pkg_path)

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

  ## if there are no assets then output a message saying so
  if(is.na(use_case_specs$assets)) {
    message(glue::glue("No assets to add for the specfied use case: {use_case_specs$use_case}"))
  } else {
    ## otherwise split assets string (separated by ";" if there is more than one)
    assets <- strsplit(use_case_specs$assets, split = ";")[[1]]

    assets_dir <- fs::path(docker_dir, "assets")

    ## create the assets subdirectory in docker dir if it is not already there
    if(!fs::dir_exists(assets_dir)) {
      message(glue::glue("The directory will be created at {assets_dir} \nAssets for the specified use case ({use_case_specs$use_case}) will be copied there."))
      fs::dir_create(assets_dir)
    } else {
      message(glue::glue("The assets directory already exists at {assets_dir} \nAssets for the specified use case ({use_case_specs$use_case}) will be copied there."))
    }

    ## copy each asset to a subdirectory of docker dir called assets
    for(i in 1:length(assets)) {
      ## get path to the asset
      tmp_asset <- assets[i]
      ## get basename to make it easier to construct destination path
      tmp_asset_bn <- basename(tmp_asset)
      message(glue::glue("The specified use case ({use_case_specs$use_case}) includes the following asset: {tmp_asset_bn}"))
      ## copy the asset from the installed pracpac package files to the destination dir
      fs::file_copy(system.file(tmp_asset, package = "pracpac", mustWork = TRUE), fs::path(docker_dir, "assets", tmp_asset_bn), overwrite = overwrite)
      ## insert pkg name in R files
      if(grepl("\\.[rR]$", fs::path(docker_dir, "assets", tmp_asset_bn))) {
        tmp_r <- paste0(readLines(fs::path(docker_dir, "assets", tmp_asset_bn)),  collapse="\n")
        write(paste0("library(", info$pkgname, ")\n", tmp_r), file = fs::path(docker_dir, "assets", tmp_asset_bn), append = FALSE)
      }
    }

  }

  # Invisibly return package information
  return(invisible(use_case_specs$assets))

}

#' Use docker packaging tools
#'
#' @description
#' Wrapper function around other `pracpac` functions. See help for the functions linked below for detail on individual functions.
#' All arguments to `use_docker()` are passed to downstream functions. `use_docker()` will sequentially run:
#' 1. [pkg_info] to get information about the current R package.
#' 1. [create_docker_dir] to create the `docker/` directory in the specified location, if it doesn't already exist.
#' 1. [renv_deps] (if `use_renv=TRUE`, the default) to capture package dependencies with renv and create an `renv.lock` file
#' 1. [add_dockerfile] to create a Dockerfile using template specified by `use_case`
#' 1. [add_assets] depending on the `use_case`
#' 1. [build_pkg] to build the current R package source .tar.gz, and place it into the `docker/` directory
#' 1. [build_image] optional, default `FALSE`; if TRUE, will build the Docker image.
#'
#' The default `build=FALSE` means that everything up to `build_image()` is run,
#' but the image is not actually built. Instead, `use_docker()` will message the
#' `docker build` command, and return that string in `$buildcmd` in the
#' invisibly returned output.
#'
#' See `vignette("use-cases", package="pracpac")` for details on use cases.
#'
#' @param pkg_path Path to the package directory. Default is `"."` for the current working directory, which assumes developer is working in R package root. However, this can be set to another path as needed.
#' @param img_path Path to the write the docker image definition contents. The default `NULL` will use `docker/` as a subdirectory of the `pkg_path`.
#' @param use_renv Logical; use renv? Defaults to `TRUE`. If `FALSE`, package dependencies are scraped from the `DESCRIPTION` file without version information.
#' @param use_case Name of the use case. Defaults to `"default"`, which only uses the base boilerplate.
#' @param base_image Name of the base image to start `FROM`. Default is `NULL` and the base image will be derived based on `use_case`. Optionally override this by setting the name of the base image (including tag if desired).
#' @param other_packages Vector of other packages to be included in `renv` lock file; default is `NULL`.
#' @param build Logical as to whether or not the image should be built. Default is `TRUE`, and if `FALSE` the `docker build` command will be messaged.  Setting `build=FALSE` could be useful if additional `docker build` options or different tags are desired. In either case the `docker build` command will be returned invisibly.
#' @param repos Option to override the repos used for installing packages with `renv` by passing name of repository. Only used if `use_renv = TRUE`. Default is `NULL` meaning that the repos specified in `renv` lockfile will remain as-is and not be overridden.
#' @param overwrite_assets Logical; should existing asset files should be overwritten? Default is `TRUE`.
#' @param overwrite_renv Logical; should an existing lock file should be overwritten? Default is `TRUE`; ignored if `use_renv = TRUE`.
#' @param consent_renv Logical; give renv consent in this session with `options(renv.consent = TRUE)`? Default is `TRUE`. See [renv::consent] for details.
#'
#' @return Invisibly returns a list with information about the package (`$info`) and
#'   the `docker build` command (`$buildcmd`). Primarily called for side effect.
#'   Creates `docker/` directory, identifies renv dependencies and creates lock
#'   file (if `use_renv = TRUE`), writes Dockerfile, builds package tar.gz,
#'   moves all relevant assets to the `docker/` directory, and builds Docker
#'   image (if `build = TRUE`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Specify path to example package source and copy to tempdir()
#' # Note that in practice you do not need to copy to a tempdir()
#' # And in fact it may be easiest to use pracpac relative to your package directory root
#' ex_pkg_src <- system.file("hellow", package = "pracpac", mustWork = TRUE)
#' file.copy(from = ex_pkg_src, to = tempdir(), recursive = TRUE)
#'
#' # Run use_docker to create Docker directory and assets for the example package
#' use_docker(pkg_path = file.path(tempdir(), "hellow"))
#' # To not use renv
#' use_docker(pkg_path = file.path(tempdir(), "hellow"), use_renv=FALSE)
#' # To specify a use case
#' use_docker(pkg_path = file.path(tempdir(), "hellow"), use_case="pipeline")
#' # To overwrite the default base image
#' use_docker(pkg_path = file.path(tempdir(), "hellow"), base_image="alpine:latest")
#' }
use_docker <- function(pkg_path = ".",
                       img_path = NULL,
                       use_renv = TRUE,
                       use_case = "default",
                       base_image = NULL,
                       other_packages = NULL,
                       build = FALSE,
                       repos = NULL,
                       overwrite_assets = TRUE,
                       overwrite_renv = TRUE,
                       consent_renv = TRUE) {

  ## check the package path
  info <- pkg_info(pkg_path)

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
    renv_deps(pkg_path = pkg_path, img_path = img_path, other_packages = other_packages, overwrite = overwrite_renv, consent_renv=consent_renv)
  }

  ## add the dockerfile to the docker/ dir
  add_dockerfile(pkg_path = pkg_path,
                 img_path = img_path,
                 use_renv = use_renv,
                 base_image = base_image,
                 use_case = use_case,
                 repos = repos)

  ## add the assets
  add_assets(pkg_path = pkg_path,
             img_path = img_path,
             use_case = use_case,
             overwrite = overwrite_assets)

  ## build the package tar.gz and copy that to the docker dir/
  build_pkg(pkg_path = pkg_path, img_path = img_path)

  ## conditionally build the image
  buildcmd <- build_image(pkg_path = pkg_path, img_path = img_path, build = build)

  # Invisibly return package info
  return(invisible(list(info=info, buildcmd=buildcmd)))

}
