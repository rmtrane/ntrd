# --- plotModule tests ---

# ---------- plotUI tests ----------

test_that("plotUI returns a tagList for a valid group", {
  ui <- plotUI("General Cognition")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("plotUI contains conditional panels and plotly output", {
  ui <- plotUI("Memory")
  ui_html <- as.character(ui)

  # Should contain the plotly output placeholder

  expect_true(grepl("Memory-plot", ui_html))

  # Should contain conditional panel references
  expect_true(grepl("showPlot", ui_html))

  # Should contain the "no scores" fallback text
  expect_true(grepl("No standardized scores found", ui_html))
})

test_that("plotUI adds tooltip for General Cognition", {
  ui <- plotUI("General Cognition")
  ui_html <- as.character(ui)
  expect_true(grepl("MoCA", ui_html))
  expect_true(grepl("MMSE", ui_html))
})

test_that("plotUI adds tooltip for Attention/Processing", {
  ui <- plotUI("Attention/Processing")
  ui_html <- as.character(ui)
  expect_true(grepl("Digit Span Forward", ui_html))
  expect_true(grepl("Number Span", ui_html))
})

test_that("plotUI adds tooltip for Memory", {
  ui <- plotUI("Memory")
  ui_html <- as.character(ui)
  expect_true(grepl("Logical Memory", ui_html))
  expect_true(grepl("Craft", ui_html))
})

test_that("plotUI adds tooltip for Language", {
  ui <- plotUI("Language")
  ui_html <- as.character(ui)
  expect_true(grepl("Boston Naming", ui_html))
  expect_true(grepl("MINT", ui_html))
})

test_that("plotUI does not add tooltip for groups without one", {
  # Pick a group that doesn't have a tooltip
  groups_with_tooltip <- c(
    "General Cognition", "Attention/Processing", "Memory", "Language"
  )
  other_groups <- setdiff(nacc_groups, groups_with_tooltip)

  if (length(other_groups) > 0) {
    ui <- plotUI(other_groups[[1]])
    ui_html <- as.character(ui)
    # Should not contain data-bs-toggle="tooltip" (bslib tooltip indicator)
    expect_false(grepl("tooltip-inner", ui_html))
  }
})

test_that("plotUI errors for an invalid group id", {
  expect_error(plotUI("Not A Real Group"), "should be one of")
})

# ---------- plotServer tests ----------

test_that("plotServer errors for an invalid group id", {
  expect_error(
    plotServer("Not A Real Group", dat = shiny::reactiveVal(NULL)),
    "should be one of"
  )
})

# ---------- plotApp tests (shinytest2) ----------

test_that("plotApp initializes without error", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  dat <- get_prepared_demo_data()

  app <- shinytest2::AppDriver$new(
    plotApp(dat_input = dat, testing = TRUE),
    name = "plotApp-init",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # App should be running — check that the study ID input exists
  vals <- app$get_values(input = TRUE)$input
  expect_true("studyid" %in% names(vals))
})

test_that("plotApp shows plots after selecting a study ID", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  dat <- get_prepared_demo_data()
  studyids <- unique(dat$NACCID)

  app <- shinytest2::AppDriver$new(
    plotApp(dat_input = dat, studyids = studyids, testing = TRUE),
    name = "plotApp-select-id",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Select the first study ID (wait_ = FALSE since plot rendering can be slow)
  app$set_inputs(studyid = studyids[[1]], wait_ = FALSE)
  app$wait_for_idle(timeout = 30000)

  # current_date input should now have choices
  vals <- app$get_values(input = TRUE)$input
  expect_true("current_date" %in% names(vals))
  expect_false(is.null(vals$current_date))
})

test_that("plotApp UI contains expected components", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  dat <- get_prepared_demo_data()

  app <- shinytest2::AppDriver$new(
    plotApp(dat_input = dat, testing = TRUE),
    name = "plotApp-ui-check",
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Check that the accordion for plots exists
  html <- app$get_html("#main-plot")
  expect_true(grepl("plots-accordion", html))
  expect_true(grepl("Longitudinal Trends", html))
})
