test_that("my_gt_plt_bar_pct errors when input is not gt_tbl", {
  expect_error(
    my_gt_plt_bar_pct(data.frame(val = 1:3), column = "val"),
    "gt_tbl"
  )
})

test_that("my_gt_plt_bar_pct errors when label_cutoff is out of range", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  expect_error(
    my_gt_plt_bar_pct(gt_obj, column = "val", label_cutoff = 1.5),
    "label_cutoff"
  )
  expect_error(
    my_gt_plt_bar_pct(gt_obj, column = "val", label_cutoff = -0.1),
    "label_cutoff"
  )
})

test_that("my_gt_plt_bar_pct errors when font_style is invalid", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  expect_error(
    my_gt_plt_bar_pct(gt_obj, column = "val", font_style = "underline"),
    "font_style"
  )
})

test_that("my_gt_plt_bar_pct returns gt_tbl for valid input", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val")
  expect_s3_class(result, "gt_tbl")
})

test_that("my_gt_plt_bar_pct works with labels = TRUE", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val", labels = TRUE)
  expect_s3_class(result, "gt_tbl")
})

test_that("my_gt_plt_bar_pct works with scaled = TRUE", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val", scaled = TRUE)
  expect_s3_class(result, "gt_tbl")
})

test_that("my_gt_plt_bar_pct handles NA values in column", {
  gt_obj <- gt::gt(data.frame(val = c(10, NA, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val")
  expect_s3_class(result, "gt_tbl")
})

test_that("my_gt_plt_bar_pct bar HTML is generated on render (no labels)", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val")
  html <- gt::as_raw_html(result)
  expect_true(grepl("flex-grow", html))
})

test_that("my_gt_plt_bar_pct renders labels when labels = TRUE and value < cutoff", {
  # 10 is below 0.4 * 90 = 36, so it triggers the label_cutoff branch
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val", labels = TRUE)
  html <- gt::as_raw_html(result)
  expect_true(grepl("position:absolute", html))
})

test_that("my_gt_plt_bar_pct renders labels with scaled = TRUE", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val", labels = TRUE, scaled = TRUE)
  html <- gt::as_raw_html(result)
  expect_true(grepl("%", html))
})

test_that("my_gt_plt_bar_pct renders all-NA column as mdash", {
  gt_obj <- gt::gt(data.frame(val = c(NA_real_, NA_real_)))
  result <- my_gt_plt_bar_pct(gt_obj, column = "val")
  html <- gt::as_raw_html(result)
  expect_true(grepl("mdash", html))
})

test_that("my_gt_plt_bar_pct works with all valid font_style options", {
  gt_obj <- gt::gt(data.frame(val = c(10, 50, 90)))
  for (style in c("bold", "normal", "italic")) {
    result <- my_gt_plt_bar_pct(gt_obj, column = "val", font_style = style)
    expect_s3_class(result, "gt_tbl")
  }
})
