test_that("calc_fill_colors returns correct number of colors", {
  for (n in 3:11) {
    result <- calc_fill_colors(n)
    expect_length(result, n)
  }
})

test_that("calc_fill_colors returns valid hex color strings", {
  for (n in 3:11) {
    result <- calc_fill_colors(n)
    expect_true(
      all(grepl("^#[A-Fa-f0-9]{6}$", result)),
      info = paste("n =", n)
    )
  }
})

test_that("calc_fill_colors errors for n < 3", {
  expect_error(calc_fill_colors(2))
  expect_error(calc_fill_colors(1))
  expect_error(calc_fill_colors(0))
})

test_that("calc_fill_colors errors for n > 11", {
  expect_error(calc_fill_colors(12))
  expect_error(calc_fill_colors(20))
})
