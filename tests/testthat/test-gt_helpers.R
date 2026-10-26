# --- is_hex_col ---

test_that("is_hex_col identifies valid 6-char hex colors", {
  expect_true(is_hex_col("#FF0000"))
  expect_true(is_hex_col("#aabbcc"))
  expect_true(is_hex_col("#000000"))
})

test_that("is_hex_col identifies valid 8-char hex colors (with alpha)", {
  expect_true(is_hex_col("#FF0000FF"))
  expect_true(is_hex_col("#aabbccdd"))
})

test_that("is_hex_col rejects invalid colors", {
  expect_false(is_hex_col("#GG0000"))
  expect_false(is_hex_col("red"))
  expect_false(is_hex_col("#ABC"))
  expect_false(is_hex_col("FF0000"))
})

# --- is_short_hex ---

test_that("is_short_hex identifies 3-char short hex", {
  expect_true(is_short_hex("#ABC"))
  expect_true(is_short_hex("#fff"))
})

test_that("is_short_hex identifies 4-char short hex (with alpha)", {
  expect_true(is_short_hex("#ABCD"))
})

test_that("is_short_hex rejects non-short hex", {
  expect_false(is_short_hex("#AABBCC"))
  expect_false(is_short_hex("red"))
  expect_false(is_short_hex("#AABBCCDD"))
})

# --- expand_short_hex ---

test_that("expand_short_hex expands 3-char hex correctly", {
  expect_equal(expand_short_hex("#ABC"), "#AABBCC")
  expect_equal(expand_short_hex("#fff"), "#FFFFFF")
})

test_that("expand_short_hex expands 4-char hex correctly", {
  expect_equal(expand_short_hex("#ABCD"), "#AABBCCDD")
})

# --- is_rgba_col ---

test_that("is_rgba_col identifies valid RGBA strings", {
  expect_true(is_rgba_col("rgba(255, 0, 0, 1)"))
  expect_true(is_rgba_col("rgba(0,0,0,0.5)"))
})

test_that("is_rgba_col rejects non-RGBA strings", {
  expect_false(is_rgba_col("#FF0000"))
  expect_false(is_rgba_col("red"))
  expect_false(is_rgba_col("rgb(255, 0, 0)"))
})

# --- rgba_to_hex ---

test_that("rgba_to_hex converts RGBA to hex", {
  result <- rgba_to_hex("rgba(255, 0, 0, 1)")
  expect_true(is_hex_col(result))
})

test_that("rgba_to_hex passes hex colors through unchanged", {
  expect_equal(rgba_to_hex("#FF0000"), "#FF0000")
})

test_that("rgba_to_hex handles mixed input", {
  result <- rgba_to_hex(c("#FF0000", "rgba(0, 255, 0, 1)"))
  expect_length(result, 2)
  expect_equal(result[1], "#FF0000")
  expect_true(is_hex_col(result[2]))
})

# --- ideal_fgnd_color ---

test_that("ideal_fgnd_color returns dark text for light backgrounds", {
  result <- ideal_fgnd_color("#FFFFFF")
  expect_equal(result, "#000000")
})

test_that("ideal_fgnd_color returns light text for dark backgrounds", {
  result <- ideal_fgnd_color("#000000")
  expect_equal(result, "#FFFFFF")
})

# --- valid_color_names ---

test_that("valid_color_names includes standard R colors", {
  vnames <- valid_color_names()
  expect_true("red" %in% vnames)
  expect_true("blue" %in% vnames)
  expect_true("transparent" %in% vnames)
})

test_that("valid_color_names includes CSS-exclusive colors", {
  vnames <- valid_color_names()
  css_names <- names(css_exclusive_colors())
  expect_true(all(css_names %in% vnames))
})

# --- gt_index ---

test_that("gt_index extracts column data from gt_tbl", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  gt_obj <- gt::gt(df)

  result <- gt_index(gt_obj, "x")
  expect_equal(result, 1:3)
})

test_that("gt_index returns full data frame when as_vector = FALSE", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  gt_obj <- gt::gt(df)

  result <- gt_index(gt_obj, "x", as_vector = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

# --- html_color ---

test_that("html_color errors when colors contain NA", {
  expect_error(html_color(c("#FF0000", NA)), "NA")
})

test_that("html_color resolves CSS-exclusive color names to hex", {
  css_names <- names(css_exclusive_colors())
  if (length(css_names) > 0) {
    result <- html_color(css_names[1], alpha = 1)
    expect_true(is_hex_col(result) || is_rgba_col(result))
  }
})

# --- check_named_colors ---

test_that("check_named_colors errors on invalid color names", {
  expect_error(check_named_colors("not_a_real_color"), "color names")
})

# --- gt_index ---

test_that("gt_index respects row group ordering", {
  df <- data.frame(group = c("B", "A", "A"), x = c(3, 1, 2))
  gt_obj <- gt::gt(df, groupname_col = "group") |>
    gt::row_group_order(groups = c("A", "B"))
  result <- gt_index(gt_obj, "x")
  expect_equal(result, c(1, 2, 3))
})

test_that("gt_index errors when input is not gt_tbl", {
  expect_error(
    gt_index(data.frame(x = 1:3), "x"),
    "gt_tbl"
  )
})
