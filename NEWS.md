# pracpac 0.2.0

## New Features

### Shiny use case documentation

As of this release, `pracpac` now includes documentation for the "shiny" use case. The package now features a vignette dedicated to the templating a Docker image to host a Shiny web app. To motivate the example usage, there is now also another example package in the `pracpac` source at `inst/ocf`.

## Bug Fixes

### `renv_deps()` and strictly versioned package depedencies

Previously the `renv_deps()` function was not able to parse dependencies in the `DESCRIPTION` file that stated strict versions of packages. The function can now handle this case, such that all dependencies in `DESCRIPTION` (whether they include a strict version number or not) will be passed into the `renv` snapshot procedure.

# pracpac 0.1.0

Initial release! 

For more information see <https://doi.org/10.48550/arXiv.2303.07876> or `browseVignettes(package = "pracpac")`.
