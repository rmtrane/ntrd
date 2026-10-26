test_that("base_plot_z_scores returns a plotly htmlwidget", {
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-2.5, 2.5)
  )
  expect_s3_class(result, "plotly")
  expect_s3_class(result, "htmlwidget")
})

test_that("base_plot_z_scores works with shade_descriptions = FALSE", {
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-2.5, 2.5),
    shade_descriptions = FALSE,
    fill_values = setNames(
      calc_fill_colors(7),
      nm = c(
        "Impaired", "Borderline", "Low Average",
        "Average", "High Average", "Superior", "Very Superior"
      )
    )
  )
  expect_s3_class(result, "plotly")
})

test_that("base_plot_z_scores auto-generates fill colors when shade_descriptions = TRUE", {
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-2.5, 2.5),
    shade_descriptions = TRUE
  )
  expect_s3_class(result, "plotly")
})

test_that("base_plot_z_scores accepts custom fill_values", {
  custom_fills <- setNames(
    calc_fill_colors(7),
    nm = c(
      "Impaired", "Borderline", "Low Average",
      "Average", "High Average", "Superior", "Very Superior"
    )
  )
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-2.5, 2.5),
    fill_values = custom_fills
  )
  expect_s3_class(result, "plotly")
})

test_that("base_plot_z_scores new_id patches internal plotly keys", {
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-2.5, 2.5),
    new_id = "my_custom_id"
  )

  expect_equal(result$x$cur_data, "my_custom_id")
  expect_true("my_custom_id" %in% names(result$x$attrs))
})

test_that("base_plot_z_scores configures axes correctly", {
  result <- base_plot_z_scores(
    x_range = as.Date(c("2024-01-01", "2024-12-31")),
    y_range = c(-3, 3)
  )

  # Access layout attributes
  layout_attrs <- result$x$layoutAttrs[[1]]
  expect_equal(layout_attrs$yaxis$minallowed, -3)
  expect_equal(layout_attrs$yaxis$maxallowed, 3)
  expect_equal(layout_attrs$xaxis$title, "Date of Test")
  expect_equal(layout_attrs$yaxis$title, "z-score")
})
