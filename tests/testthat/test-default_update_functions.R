# ===========================================================================
# default_github_update_extension()
# ===========================================================================

test_that("default_github_update_extension records the repo on the function", {
  fun <- default_github_update_extension("user/myrepo@dev")
  expect_type(fun, "closure")
  expect_equal(attr(fun, "ntrd_github_repo"), "user/myrepo@dev")
})


# ===========================================================================
# get_github_version()
# ===========================================================================

test_that("get_github_version reads Version: with a 5 second timeout by default", {
  captured <- list()
  local_mocked_bindings(
    read_url_lines = function(url, timeout) {
      captured <<- list(url = url, timeout = timeout)
      c("Package: myrepo", "Version: 1.2.3")
    }
  )

  expect_equal(get_github_version("user/myrepo"), package_version("1.2.3"))
  expect_equal(
    captured$url,
    "https://raw.githubusercontent.com/user/myrepo/HEAD/DESCRIPTION"
  )
  expect_equal(captured$timeout, 5)
})

test_that("get_github_version honors the ntrd.update_check_timeout option and @ref", {
  captured <- list()
  local_mocked_bindings(
    read_url_lines = function(url, timeout) {
      captured <<- list(url = url, timeout = timeout)
      "Version: 0.1.0"
    }
  )
  withr::local_options(ntrd.update_check_timeout = 2)

  get_github_version("user/myrepo@v0.1")
  expect_equal(
    captured$url,
    "https://raw.githubusercontent.com/user/myrepo/v0.1/DESCRIPTION"
  )
  expect_equal(captured$timeout, 2)
})

test_that("get_github_version errors when DESCRIPTION has no Version field", {
  local_mocked_bindings(read_url_lines = function(url, timeout) "Package: x")
  expect_error(get_github_version("user/myrepo"), "Version")
})

test_that("read_url_lines restores the timeout option", {
  withr::local_options(timeout = 60)
  path <- withr::local_tempfile(lines = c("a", "b"))

  expect_equal(read_url_lines(paste0("file://", path), timeout = 3), c("a", "b"))
  expect_equal(getOption("timeout"), 60)
})
