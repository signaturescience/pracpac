
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
