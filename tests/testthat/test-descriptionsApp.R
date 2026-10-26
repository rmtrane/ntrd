test_that("descriptionsApp initializes with correct defaults", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    descriptionsApp(testing = TRUE),
    name = "descriptionsApp-defaults",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Check exported test values
  test_vals <- app$get_values(export = TRUE)$export

  # Should have 7 default description groups
  expect_length(test_vals$descriptions, 7)
  expect_length(test_vals$fill_values, 7)

  # Names should be the default labels
  expect_equal(
    names(test_vals$descriptions),
    c(
      "Impaired",
      "Borderline",
      "Low Average",
      "Average",
      "High Average",
      "Superior",
      "Very Superior"
    )
  )

  # Fill values should be valid colors
  expect_true(check_colors(test_vals$fill_values))
})

test_that("descriptionsApp add_row adds a description and updates exports", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    descriptionsApp(testing = TRUE),
    name = "descriptionsApp-add-row",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Click the "+" button to add a row
  app$click(selector = "#desc-add_row")
  app$wait_for_idle()

  test_vals <- app$get_values(export = TRUE)$export

  # Should now have 8 description groups
  expect_length(test_vals$descriptions, 8)
  expect_true("New Group" %in% names(test_vals$descriptions))
})

test_that("descriptionsApp reset restores defaults", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    descriptionsApp(testing = TRUE),
    name = "descriptionsApp-reset",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Add a row first
  app$click(selector = "#desc-add_row")
  app$wait_for_idle()

  expect_length(app$get_values(export = TRUE)$export$descriptions, 8)

  # Click reset
  app$click(selector = "#desc-reset")
  app$wait_for_idle()

  test_vals <- app$get_values(export = TRUE)$export
  expect_length(test_vals$descriptions, 7)
})

test_that("descriptionsApp works with custom descriptions", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  custom <- c("Low" = 0.25, "Medium" = 0.75, "High" = 1.00)

  app <- shinytest2::AppDriver$new(
    descriptionsApp(default_descriptions = custom, testing = TRUE),
    name = "descriptionsApp-custom",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  test_vals <- app$get_values(export = TRUE)$export

  expect_length(test_vals$descriptions, 3)
  expect_equal(names(test_vals$descriptions), c("Low", "Medium", "High"))
  expect_length(test_vals$fill_values, 3)
})
