
library(fs)
library(withr)
tmp <- tempdir()

test_that("use_docker pipeline creates expected directories and files with defaults", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    use_docker(pkg_path = path(tmp, "test", "hellow"))
  },
  tmpdir = tmp)

  expect_true(dir_exists(path(tmp, "test", "hellow", "docker")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "Dockerfile")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "renv.lock")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "hellow_0.1.0.tar.gz")))

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Option to ignore renv works", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE)
  },
  tmpdir = tmp)

  expect_false(file_exists(path(tmp, "test", "hellow", "docker", "renv.lock")))

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Alternative directory structure works", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker ... have already checked that option above
    use_docker(pkg_path = path(tmp, "test", "hellow"), img_path = path(tmp, "test"), use_renv = FALSE)
  },
  tmpdir = tmp)

  expect_true(file_exists(path(tmp, "test", "Dockerfile")))
  expect_false(file_exists(path(tmp, "test", "renv.lock")))
  expect_true(file_exists(path(tmp, "test", "hellow_0.1.0.tar.gz")))

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("The base image override option works", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker ... have already checked that option above
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE, base_image = "debian:10")
  },
  tmpdir = tmp)

  ## check the base image
  dockerfile <- readLines(path(tmp, "test", "hellow", "docker", "Dockerfile"))
  expect_equal(dockerfile[1], "FROM debian:10")

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("The other packages option works", {

  # packages passed to other_packages=... must actually be installed. Check this first.
  expect_true("palmerpenguins" %in% rownames(installed.packages()))

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: have to set use_renv = TRUE for this one ...
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = TRUE, other_packages = "palmerpenguins")
  },
  tmpdir = tmp)

  ## check the base image
  renvlock <- readLines(path(tmp, "test", "hellow", "docker", "renv.lock"))
  expect_true(any(sapply(renvlock, function(x) grepl("palmerpenguins", x))))

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Use case templates work (shiny)", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker ... have already checked that option above
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE, use_case = "shiny")
  },
  tmpdir = tmp)

  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "assets", "app.R")))

  ## check the base image
  dockerfile <- readLines(path(tmp, "test", "hellow", "docker", "Dockerfile"))
  expect_equal(dockerfile[1], "FROM rocker/shiny:latest")

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Use case templates work (rstudio)", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker ... have already checked that option above
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE, use_case = "rstudio")
  },
  tmpdir = tmp)

  ## check the base image
  dockerfile <- readLines(path(tmp, "test", "hellow", "docker", "Dockerfile"))
  expect_equal(dockerfile[1], "FROM rocker/rstudio:latest")

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Use case templates work (pipeline)", {

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker ... have already checked that option above
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE, use_case = "pipeline")
  },
  tmpdir = tmp)

  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "assets", "run.sh")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "assets", "pre.R")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "assets", "post.R")))

  ## check the base image
  dockerfile <- readLines(path(tmp, "test", "hellow", "docker", "Dockerfile"))
  expect_equal(dockerfile[1], "FROM rocker/r-ver:latest")

  ## clean up
  dir_delete(path(tmp, "test"))
})
