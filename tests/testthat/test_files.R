library(fs)
library(withr)
tmp <- tempdir()

## NOTE: as of v0.2.0 none of the tests use renv snapshot per CRAN req

test_that("use_docker pipeline creates expected directories and files with defaults", {

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE)
  },
  tmpdir = tmp)

  expect_true(dir_exists(path(tmp, "test", "hellow", "docker")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "Dockerfile")))
  expect_true(file_exists(path(tmp, "test", "hellow", "docker", "hellow_0.1.0.tar.gz")))

})

test_that("build_image returns build command as expected", {

  skip_on_cran()

  # Check build command
  buildcmd <- suppressMessages(build_image(pkg_path = path(tmp, "test", "hellow"), build=FALSE))
  expect_identical(as.character(buildcmd), paste("docker build  --tag hellow:latest --tag hellow:0.1.0", path(tmp, "test", "hellow", "docker")))

  ## clean up
  ## NOTE: this clean up is not performed in the test prior to this ...
  ## lets us retain the files to check build command
  ## but now we can cleanup
  dir_delete(path(tmp, "test"))
})


test_that("Option to ignore renv works", {

  skip_on_cran()

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

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker
    use_docker(pkg_path = path(tmp, "test", "hellow"), img_path = path(tmp, "test"), use_renv = FALSE)
  },
  tmpdir = tmp)

  expect_true(file_exists(path(tmp, "test", "Dockerfile")))
  expect_false(file_exists(path(tmp, "test", "renv.lock")))
  expect_true(file_exists(path(tmp, "test", "hellow_0.1.0.tar.gz")))

})

test_that("build_image returns build command as expected with alternative directory", {

  skip_on_cran()

  # Check build command
  buildcmd <- suppressMessages(build_image(pkg_path = path(tmp, "test", "hellow"), img_path = path(tmp, "test"), build=FALSE))
  expect_identical(as.character(buildcmd), paste("docker build  --tag hellow:latest --tag hellow:0.1.0", path(tmp, "test")))

  ## clean up
  ## NOTE: this clean up is not performed in the test prior to this ...
  ## lets us retain the files to check build command
  ## but now we can cleanup
  dir_delete(path(tmp, "test"))
})

test_that("The base image override option works", {

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker
    use_docker(pkg_path = path(tmp, "test", "hellow"), use_renv = FALSE, base_image = "debian:10")
  },
  tmpdir = tmp)

  ## check the base image
  dockerfile <- readLines(path(tmp, "test", "hellow", "docker", "Dockerfile"))
  expect_equal(dockerfile[1], "FROM debian:10")

  ## clean up
  dir_delete(path(tmp, "test"))
})

test_that("Use case templates work (shiny)", {

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker
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

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker
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

  skip_on_cran()

  ex_pkg <- system.file("hellow", package = "pracpac")
  dir_create(path = path(tmp, "test"), recurse = TRUE)
  dir_copy(ex_pkg, path(tmp, "test"))
  with_tempdir({
    ## NOTE: setting use_renv to FALSE to make this test run quicker
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
