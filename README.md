
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pracpac

<!-- badges: start -->
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
#> $rprojroot
#> [1] 2 0 3
#> 
#> $glue
#> [1] 1 6 2
#> 
#> $pracpac
#> [1]    0    0    0 9000
#> 
#> [1] "R version 4.2.2 (2022-10-31)"
```
