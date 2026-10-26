# Tests for R/extension_updates.R
#
# Strategy: instead of trying to register real namespaces, we use
# testthat::local_mocked_bindings() to temporarily replace the internal
# get_pkg_namespace() seam inside the ntrd package with a stub that
# returns a fake environment containing whichever update hooks the test
# needs. local_mocked_bindings() requires the binding to exist in the
# package namespace, which is why R/extension_updates.R defines
# get_pkg_namespace() as a thin wrapper around base::getNamespace().

# Tiny base-R `%||%` so we don't need R >= 4.4 or rlang in tests.
`%||%` <- function(x, y) if (is.null(x)) y else x


# Build a fake namespace environment containing the requested hooks.
make_fake_ns <- function(
  has_check = TRUE,
  has_install = TRUE,
  check_fn = NULL,
  install_fn = NULL
) {
  ns <- new.env(parent = baseenv())
  if (has_check) {
    ns$ntrd_update_available <- check_fn %||%
      function() update_result(available = FALSE)
  }
  if (has_install) {
    ns$ntrd_update_extension <- install_fn %||%
      function() invisible(NULL)
  }
  ns
}


# ===========================================================================
# extension_supports_updates()
# ===========================================================================

test_that("extension_supports_updates returns FALSE for bad inputs", {
  expect_false(extension_supports_updates(NULL))
  expect_false(extension_supports_updates(NA_character_))
  expect_false(extension_supports_updates(1L))
  expect_false(extension_supports_updates(c("a", "b")))
})

test_that("extension_supports_updates returns FALSE when get_pkg_namespace errors", {
  local_mocked_bindings(get_pkg_namespace = function(name) {
    stop("no such namespace")
  })
  expect_false(extension_supports_updates("anything"))
})

test_that("extension_supports_updates returns FALSE when neither hook is exported", {
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(has_check = FALSE, has_install = FALSE)
    }
  )
  expect_false(extension_supports_updates("fakepkg"))
})

test_that("extension_supports_updates returns TRUE when both hooks are exported", {
  local_mocked_bindings(get_pkg_namespace = function(name) make_fake_ns())
  expect_true(extension_supports_updates("fakepkg"))
})

test_that("extension_supports_updates warns once when only ntrd_update_available is exported", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(has_check = TRUE, has_install = FALSE)
    }
  )

  expect_warning(
    result <- extension_supports_updates("fakepkg_check_only"),
    regexp = "exports one but not both update hooks"
  )
  expect_false(result)

  # Second call: warning already emitted, no second warning
  expect_no_warning(extension_supports_updates("fakepkg_check_only"))
})

test_that("extension_supports_updates warns once when only ntrd_update_extension is exported", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(has_check = FALSE, has_install = TRUE)
    }
  )

  expect_warning(
    result <- extension_supports_updates("fakepkg_install_only"),
    regexp = "exports one but not both update hooks"
  )
  expect_false(result)
})


# ===========================================================================
# clear_update_cache()
# ===========================================================================

test_that("clear_update_cache clears all entries when called with no argument", {
  assign(
    "pkg_a",
    list(checked_at = Sys.time(), result = update_result()),
    envir = .update_cache
  )
  assign(
    "pkg_b",
    list(checked_at = Sys.time(), result = update_result()),
    envir = .update_cache
  )

  clear_update_cache()

  expect_equal(ls(envir = .update_cache, all.names = TRUE), character(0))
})

test_that("clear_update_cache clears only the named package", {
  withr::defer(clear_update_cache())
  assign(
    "pkg_a",
    list(checked_at = Sys.time(), result = update_result()),
    envir = .update_cache
  )
  assign(
    "pkg_b",
    list(checked_at = Sys.time(), result = update_result()),
    envir = .update_cache
  )

  clear_update_cache("pkg_a")

  cache_keys <- ls(envir = .update_cache, all.names = FALSE)
  expect_false("pkg_a" %in% cache_keys)
  expect_true("pkg_b" %in% cache_keys)
})

test_that("clear_update_cache is a no-op when named package is not in cache", {
  clear_update_cache()
  expect_no_error(clear_update_cache("nonexistent_pkg"))
})

test_that("clear_update_cache returns invisible NULL", {
  result <- withVisible(clear_update_cache())
  expect_null(result$value)
  expect_false(result$visible)
})


# ===========================================================================
# validate_update_check_result()
# ===========================================================================

test_that("validate_update_check_result passes through a valid update_result unchanged", {
  r <- update_result(available = TRUE, current = "0.1.0", latest = "0.2.0")
  expect_identical(validate_update_check_result(r), r)
})

test_that("validate_update_check_result coerces a well-formed list", {
  x <- list(
    available = TRUE,
    current = "1.0.0",
    latest = "2.0.0",
    news_url = "https://example.com"
  )
  result <- validate_update_check_result(x)
  expect_true(S7::S7_inherits(result, update_result))
  expect_true(result@available)
  expect_equal(result@current, "1.0.0")
  expect_equal(result@latest, "2.0.0")
  expect_equal(result@news_url, "https://example.com")
})

test_that("validate_update_check_result strips extra fields from a list", {
  x <- list(
    available = FALSE,
    extra_field = "should be ignored",
    current = "1.0.0"
  )
  result <- validate_update_check_result(x)
  expect_true(S7::S7_inherits(result, update_result))
})

test_that("validate_update_check_result warns and returns default for non-list, non-update_result", {
  expect_warning(
    result <- validate_update_check_result("a string"),
    regexp = "not an.*update_result.*or list"
  )
  expect_equal(result, update_result())
})

test_that("validate_update_check_result warns and returns default when list cannot be coerced", {
  bad_list <- list(available = "yes") # wrong type; should be logical
  expect_warning(
    result <- validate_update_check_result(bad_list, package = "mypkg"),
    regexp = "could not be coerced"
  )
  expect_equal(result, update_result())
})

test_that("validate_update_check_result includes package name in warning message", {
  expect_warning(
    validate_update_check_result(42, package = "coolpkg"),
    regexp = "coolpkg"
  )
})

test_that("validate_update_check_result uses 'extension' in warning when package is NULL", {
  expect_warning(
    validate_update_check_result(42),
    regexp = "extension"
  )
})


# ===========================================================================
# check_extension_update()
# ===========================================================================

test_that("check_extension_update returns default when extension does not support updates", {
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(has_check = FALSE, has_install = FALSE)
    }
  )
  result <- check_extension_update("fakepkg_no_support")
  expect_equal(result, update_result())
})

test_that("check_extension_update calls ntrd_update_available and returns its result", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(
        check_fn = function() {
          update_result(available = TRUE, current = "1.0", latest = "2.0")
        }
      )
    }
  )

  result <- check_extension_update("fakepkg_has_update", force = TRUE)
  expect_true(result@available)
  expect_equal(result@latest, "2.0")
})

test_that("check_extension_update caches results across calls", {
  withr::defer(clear_update_cache())
  call_count <- 0L
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(
        check_fn = function() {
          call_count <<- call_count + 1L
          update_result(available = FALSE)
        }
      )
    }
  )

  check_extension_update("fakepkg_cached", force = TRUE)
  check_extension_update("fakepkg_cached")
  check_extension_update("fakepkg_cached")

  expect_equal(call_count, 1L)
})

test_that("check_extension_update force = TRUE bypasses cache", {
  withr::defer(clear_update_cache())
  call_count <- 0L
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(
        check_fn = function() {
          call_count <<- call_count + 1L
          update_result()
        }
      )
    }
  )

  check_extension_update("fakepkg_force", force = TRUE)
  check_extension_update("fakepkg_force", force = TRUE)

  expect_equal(call_count, 2L)
})

test_that("check_extension_update refreshes after TTL expires", {
  withr::defer(clear_update_cache())
  call_count <- 0L
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(
        check_fn = function() {
          call_count <<- call_count + 1L
          update_result()
        }
      )
    }
  )

  check_extension_update("fakepkg_ttl", ttl = 0)
  check_extension_update("fakepkg_ttl", ttl = 0)

  expect_equal(call_count, 2L)
})

test_that("check_extension_update warns and returns default when ntrd_update_available() errors", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(check_fn = function() stop("simulated network error"))
    }
  )

  expect_warning(
    result <- check_extension_update("fakepkg_errors", force = TRUE),
    regexp = "Update check for.*fakepkg_errors.*failed"
  )
  expect_equal(result, update_result())
})

test_that("check_extension_update does not error when ntrd_update_available() errors", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(check_fn = function() stop("boom"))
    }
  )

  expect_no_error(
    suppressWarnings(check_extension_update("fakepkg_safe", force = TRUE))
  )
})

test_that("check_extension_update validates raw list returned by ntrd_update_available()", {
  withr::defer(clear_update_cache())
  local_mocked_bindings(
    get_pkg_namespace = function(name) {
      make_fake_ns(
        check_fn = function() {
          list(available = TRUE, current = "1.0", latest = "1.1")
        }
      )
    }
  )

  result <- check_extension_update("fakepkg_raw_list", force = TRUE)
  expect_true(S7::S7_inherits(result, update_result))
  expect_true(result@available)
})


# ===========================================================================
# build_update_restart_command()
# ===========================================================================

test_that("build_update_restart_command errors on non-string input", {
  local_mocked_bindings(
    extension_install_repo = function(package) NULL
  )
  expect_error(build_update_restart_command(NULL))
  expect_error(build_update_restart_command(1L))
  expect_error(build_update_restart_command(NA_character_))
  expect_error(build_update_restart_command(""))
  expect_error(build_update_restart_command(c("a", "b")))
})

test_that("build_update_restart_command returns a single string", {
  result <- build_update_restart_command("mypkg", repo = "user/myrepo")
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("build_update_restart_command embeds unusual package names correctly", {
  cmd <- build_update_restart_command("my.ext_pkg", repo = "user/myrepo")
  expect_match(cmd, "my.ext_pkg", fixed = TRUE)
})


# Evaluates a restart command with remotes::install_github(),
# rstudioapi::restartSession(), and getNamespace() replaced by fakes. Any
# other `::` or `:::` access (e.g. to ntrd) is an error, which proves
# nothing else gets loaded.
run_restart_command <- function(
  cmd,
  install = function(...) invisible(NULL),
  get_namespace = function(name) stop("no such package '", name, "'")
) {
  calls <- new.env()
  calls$installs <- list()
  calls$namespaces_requested <- character()
  calls$restart <- NULL

  fake_colons <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg == "remotes" && name == "install_github") {
      return(function(...) {
        calls$installs <- c(calls$installs, list(list(...)))
        install(...)
      })
    }
    if (pkg == "rstudioapi" && name == "restartSession") {
      return(function(command) calls$restart <- command)
    }
    stop("unexpected access to ", pkg, "::", name)
  }

  env <- new.env(parent = baseenv())
  env$`::` <- fake_colons
  env$`:::` <- fake_colons
  env$getNamespace <- function(name) {
    calls$namespaces_requested <- c(calls$namespaces_requested, name)
    get_namespace(name)
  }
  utils::capture.output(
    for (expr in parse(text = cmd)) eval(expr, env),
    type = "output"
  )
  calls
}

test_that("build_update_restart_command installs a GitHub repo without loading ntrd", {
  cmd <- build_update_restart_command("mypkg", repo = "user/myrepo")
  calls <- run_restart_command(cmd)

  expect_length(calls$installs, 1L)
  install_args <- calls$installs[[1]]
  expect_equal(install_args[[1]], "user/myrepo")
  expect_equal(install_args$upgrade, "always")
  expect_true(install_args$dependencies)
  expect_length(calls$namespaces_requested, 0L)

  expect_equal(calls$restart, 'ntrd::update_finalize(TRUE, "mypkg")')
})

test_that("build_update_restart_command finalizes with FALSE when the GitHub install fails", {
  cmd <- build_update_restart_command("mypkg", repo = "user/myrepo")

  expect_message(
    calls <- run_restart_command(cmd, install = function(...) stop("boom")),
    "Install error:.*boom"
  )
  expect_equal(calls$restart, 'ntrd::update_finalize(FALSE, "mypkg")')
})

test_that("build_update_restart_command keeps an @ref in the repo", {
  cmd <- build_update_restart_command("mypkg", repo = "user/myrepo@v1.2")
  calls <- run_restart_command(cmd)
  expect_equal(calls$installs[[1]][[1]], "user/myrepo@v1.2")
})

test_that("build_update_restart_command reads the repo from the extension's install function", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) {
      default_github_update_extension("user/myrepo")
    }
  )
  cmd <- build_update_restart_command("mypkg")
  expect_match(cmd, 'remotes::install_github("user/myrepo"', fixed = TRUE)
  expect_no_match(cmd, "getNamespace", fixed = TRUE)
})

test_that("build_update_restart_command calls the extension's own install function without loading ntrd", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) function() invisible(NULL)
  )
  cmd <- build_update_restart_command("mypkg")
  expect_match(cmd, 'getNamespace("mypkg")$ntrd_update_extension()', fixed = TRUE)
  expect_no_match(cmd, "install_github", fixed = TRUE)

  install_called <- FALSE
  calls <- run_restart_command(
    cmd,
    get_namespace = function(name) {
      expect_equal(name, "mypkg")
      list2env(list(ntrd_update_extension = function() install_called <<- TRUE))
    }
  )
  expect_true(install_called)
  expect_equal(calls$restart, 'ntrd::update_finalize(TRUE, "mypkg")')
})

test_that("build_update_restart_command finalizes with FALSE when the custom install errors", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) function() invisible(NULL)
  )
  cmd <- build_update_restart_command("mypkg")

  expect_message(
    calls <- run_restart_command(
      cmd,
      get_namespace = function(name) {
        list2env(list(ntrd_update_extension = function() stop("install boom")))
      }
    ),
    "Install error:.*install boom"
  )
  expect_equal(calls$restart, 'ntrd::update_finalize(FALSE, "mypkg")')
})


# ===========================================================================
# extension_install_repo()
# ===========================================================================

test_that("extension_install_repo returns the repo recorded by the factory", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) {
      default_github_update_extension("user/myrepo@dev")
    }
  )
  expect_equal(extension_install_repo("mypkg"), "user/myrepo@dev")
})

test_that("extension_install_repo returns NULL for custom install functions", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) function() invisible(NULL)
  )
  expect_null(extension_install_repo("mypkg"))
})

test_that("extension_install_repo returns NULL when the lookup fails", {
  local_mocked_bindings(
    get_from_namespace = function(name, ns) stop("no such namespace")
  )
  expect_null(extension_install_repo("mypkg"))
})


# ===========================================================================
# supports_command_restart()
# ===========================================================================

test_that("supports_command_restart returns a scalar logical", {
  result <- supports_command_restart()
  expect_type(result, "logical")
  expect_length(result, 1L)
  expect_false(is.na(result))
})

test_that("supports_command_restart returns FALSE when RSTUDIO env var is unset", {
  withr::with_envvar(
    list(RSTUDIO = ""),
    expect_false(supports_command_restart())
  )
})
