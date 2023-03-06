
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pracpac

<!-- badges: start -->

[![R-CMD-check](https://github.com/signaturescience/pracpac/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/signaturescience/pracpac/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of pracpac is to …

## Installation

You can install the development version of pracpac from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("signaturescience/pracpac")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pracpac)
# Build this package without renv
build_pkg()
add_dockerfile(use_renv = FALSE)
build_image()
```

``` sh
docker run --rm pracpac Rscript -e 'sapply(c("rprojroot", "glue", "pracpac"), packageVersion); R.version.string'
```
