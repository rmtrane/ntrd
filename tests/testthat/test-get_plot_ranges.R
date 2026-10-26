test_that("get_y_range errors when input is not data.table", {
  expect_error(
    get_y_range(data.frame(x = 1:3)),
    "data.table"
  )
})

test_that("get_y_range returns numeric vector of length 2", {
  dt <- data.table::data.table(x = c(-1, 0, 1))
  result <- get_y_range(dt)

  expect_type(result, "double")
  expect_length(result, 2)
})

test_that("get_y_range returns default c(-2.5, 2.5) when all columns are NA", {
  dt <- data.table::data.table(x = NA_real_, y = NA_real_)
  result <- get_y_range(dt)

  expect_equal(result, c(-2.5, 2.5))
})

test_that("get_y_range returns c(-2.5, 2.5) when values are within that range", {
  dt <- data.table::data.table(x = c(-1, 0, 1.5))
  result <- get_y_range(dt)

  expect_equal(result, c(-2.5, 2.5))
})

test_that("get_y_range expands symmetrically for extreme values", {
  dt <- data.table::data.table(x = c(-1, 0, 4))
  result <- get_y_range(dt)

  expect_equal(result[1], -4)
  expect_equal(result[2], 4)
})

test_that("get_y_range converts T-score columns to z-scores", {
  dt <- data.table::data.table(x = c(50, 60, 70))
  attr(dt$x, "method") <- "tscores"

  result <- get_y_range(dt)

  # T=70 → z=2, which is within [-2.5, 2.5]
  expect_equal(result, c(-2.5, 2.5))
})

test_that("get_y_range handles extreme T-scores", {
  dt <- data.table::data.table(x = c(50, 90))
  attr(dt$x, "method") <- "tscores"

  result <- get_y_range(dt)

  # T=90 → z=4, which expands the range
  expect_equal(result[1], -4)
  expect_equal(result[2], 4)
})

test_that("get_y_range handles mix of all-NA and non-NA columns", {
  dt <- data.table::data.table(
    good = c(-1, 0, 1),
    bad  = c(NA_real_, NA_real_, NA_real_)
  )
  result <- get_y_range(dt)
  expect_equal(result, c(-2.5, 2.5))
})

test_that("get_y_range handles mixed T-score and z-score columns", {
  dt <- data.table::data.table(
    z_col = c(0, 1),
    t_col = c(50, 60)
  )
  attr(dt$t_col, "method") <- "tscores"

  result <- get_y_range(dt)

  # z_col max abs = 1, t_col after conversion: 0, 1 → max abs = 1
  expect_equal(result, c(-2.5, 2.5))
})
