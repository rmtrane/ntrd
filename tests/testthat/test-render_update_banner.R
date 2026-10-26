test_that("render_update_banner returns NULL when no update is available", {
  local_reproducible_output()

  result <- update_result() # default: available = FALSE
  action <- shiny::actionButton("do_update", "Update")

  expect_null(render_update_banner(result, "pkg", action))
})

test_that("render_update_banner returns NULL when result is not an update_result", {
  local_reproducible_output()

  action <- shiny::actionButton("do_update", "Update")

  expect_null(render_update_banner(list(), "pkg", action))
  expect_null(render_update_banner(NULL, "pkg", action))
  expect_null(render_update_banner("not an update_result", "pkg", action))
})

test_that("render_update_banner returns NULL when package is missing", {
  local_reproducible_output()

  result <- update_result(available = TRUE, current = "0.1", latest = "0.2")
  action <- shiny::actionButton("do_update", "Update")

  expect_null(render_update_banner(result, NULL, action))
  expect_null(render_update_banner(result, NA_character_, action))
})

test_that("render_update_banner returns NULL when action is missing", {
  local_reproducible_output()

  result <- update_result(available = TRUE, current = "0.1", latest = "0.2")

  expect_null(render_update_banner(result, "pkg", NULL))
})

test_that("render_update_banner produces expected HTML with NEWS link", {
  local_reproducible_output()

  result <- update_result(
    available = TRUE,
    current = "0.1.0",
    latest = "0.2.0",
    news_url = "https://example.com/NEWS.md"
  )
  action <- shiny::actionButton(
    "do_update",
    "Update",
    class = "btn-sm btn-primary update-banner-action"
  )

  expect_snapshot(
    cat(as.character(render_update_banner(result, "ntrdWisconsin", action)))
  )
})

test_that("render_update_banner omits NEWS link when news_url is NA", {
  local_reproducible_output()

  result <- update_result(
    available = TRUE,
    current = "0.1.0",
    latest = "0.2.0",
    news_url = NA_character_
  )
  action <- shiny::actionButton(
    "do_update",
    "Update",
    class = "btn-sm btn-primary update-banner-action"
  )

  banner_html <- as.character(render_update_banner(
    result,
    "ntrdWisconsin",
    action
  ))
  expect_false(grepl("update-banner-news-link", banner_html, fixed = TRUE))
})
