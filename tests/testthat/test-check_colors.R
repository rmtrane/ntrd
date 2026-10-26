test_that("check_colors returns TRUE for valid hex colors", {
  expect_true(check_colors("#FF0000"))
  expect_true(check_colors(c("#FF0000", "#00FF00", "#0000FF")))
  expect_true(check_colors("#abcdef"))
  expect_true(check_colors("#ABCDEF"))
  expect_true(check_colors("#aAbBcC"))
})

test_that("check_colors returns FALSE for invalid colors", {
  expect_false(check_colors("red"))
  expect_false(check_colors("not_a_color"))
  expect_false(check_colors("#GG0000"))
  expect_false(check_colors("#FFF"))
  expect_false(check_colors(c("#FF0000", "red")))
})

test_that("check_colors with return_non_colors returns non-color entries", {
  result <- check_colors(c("#FF0000", "red", "#00FF00", "blue"), return_non_colors = TRUE)
  expect_equal(unname(result), c("red", "blue"))
  expect_equal(names(result), c("2", "4"))
})

test_that("check_colors with return_non_colors returns empty for all valid", {
  result <- check_colors(c("#FF0000", "#00FF00"), return_non_colors = TRUE)
  expect_length(result, 0)
})

test_that("check_colors integrates with calc_fill_colors", {
  for (n in 3:11) {
    colors <- calc_fill_colors(n)
    expect_true(check_colors(colors), info = paste("n =", n))
  }
})
