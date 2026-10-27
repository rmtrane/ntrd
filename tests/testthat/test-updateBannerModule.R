test_that("update banner renders and confirms when an update is available", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  local_reproducible_output()

  app <- shinytest2::AppDriver$new(
    update_banner_app(fake_update = TRUE, testing = FALSE),
    name = "updateBannerMod",
    variant = shinytest2::platform_variant(),
    height = 600,
    width = 1000
  )
  on.exit(app$stop(), add = TRUE)

  # The banner should be present in the rendered page. update_banner_app
  # wires the fake update to whichever source is selected at startup, so
  # the banner element is rendered as soon as the data-select module
  # produces its initial `update_info`.
  app$wait_for_idle()

  html <- app$get_html(".update-banner")
  expect_match(html, "update-banner", fixed = TRUE)
  expect_match(html, "0.0.0.9000", fixed = TRUE) # fake current
  expect_match(html, "0.0.0.9999", fixed = TRUE) # fake latest

  app$expect_screenshot(name = "update-banner-visible")

  # Clicking the Update button opens the confirmation modal. The modal's
  # primary button is the namespaced "banner-confirm_update" input.
  app$click(selector = "#banner-do_update")
  app$wait_for_idle()

  modal_html <- app$get_html(".modal")
  expect_match(modal_html, "Update", fixed = TRUE)
  expect_match(modal_html, "banner-confirm_update", fixed = TRUE)

  app$expect_screenshot(name = "update-confirm-modal")
})

test_that("update banner is suppressed when no update is available", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    update_banner_app(fake_update = update_result(), testing = FALSE), # available = FALSE
    name = "updateBannerMod-noUpdate",
    variant = shinytest2::platform_variant(),
    height = 600,
    width = 1000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()
  app$expect_screenshot(name = "update-banner-no-update")

  # The uiOutput slot exists, but render_update_banner returned NULL, so
  # there should be no .update-banner element anywhere on the page.
  full_html <- app$get_html("body")
  expect_false(grepl("update-banner", full_html, fixed = TRUE))
})

# The tests above run the app in a separate process via shinytest2, so they
# cannot mock anything inside it. The tests below drive the module server
# in-process with shiny::testServer(), which lets local_mocked_bindings()
# replace the restart seams and check what the confirm handler does with
# them.
fake_update_info <- function(package = "fakeExt") {
  shiny::reactive(list(
    package = package,
    result = update_result(
      available = TRUE,
      current = "0.0.0.9000",
      latest = "0.0.0.9999"
    )
  ))
}

test_that("confirming an update queues restart_session() with the restart command", {
  restart_calls <- character()

  local_mocked_bindings(
    supports_command_restart = function() TRUE,
    build_update_restart_command = function(package, ...) {
      paste0("restart-command-for-", package)
    },
    restart_session = function(command) {
      restart_calls <<- c(restart_calls, command)
    }
  )

  shiny::testServer(
    update_banner_server,
    args = list(update_info = fake_update_info()),
    {
      session$setInputs(do_update = 1)
      session$setInputs(confirm_update = 1)

      # The restart is deferred until the session ends, so nothing has
      # been called yet.
      expect_length(restart_calls, 0)

      session$close()
    }
  )

  expect_identical(restart_calls, "restart-command-for-fakeExt")
})

test_that("confirming an update never restarts when command restarts are unsupported", {
  restart_calls <- character()

  local_mocked_bindings(
    supports_command_restart = function() FALSE,
    build_update_restart_command = function(package, ...) {
      paste0("restart-command-for-", package)
    },
    restart_session = function(command) {
      restart_calls <<- c(restart_calls, command)
    }
  )

  shiny::testServer(
    update_banner_server,
    args = list(update_info = fake_update_info()),
    {
      session$setInputs(do_update = 1)
      session$setInputs(confirm_update = 1)
      session$close()
    }
  )

  expect_length(restart_calls, 0)
})
