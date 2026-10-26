test_that("date_range returns length-2 Date vector", {
  dates <- as.Date(c("2024-03-15", "2024-09-20"))
  result <- date_range(dates)

  expect_length(result, 2)
  expect_s3_class(result, "Date")
})

test_that("date_range pads by approximately one quarter on each side", {
  dates <- as.Date(c("2024-06-15"))
  result <- date_range(dates)

  # min - 3 months floored, max + 3 months ceiled
  expect_true(result[1] < as.Date("2024-06-15"))
  expect_true(result[2] > as.Date("2024-06-15"))
})

test_that("date_range floors and ceils to quarter boundaries", {
  dates <- as.Date(c("2024-05-15", "2024-08-20"))
  result <- date_range(dates)

  # Quarter boundaries are Jan 1, Apr 1, Jul 1, Oct 1
  expect_equal(
    lubridate::month(result[1]) %in% c(1, 4, 7, 10),
    TRUE
  )
  expect_equal(lubridate::day(result[1]), 1)
})

test_that("date_range handles NA values in input", {
  dates <- as.Date(c("2024-03-15", NA, "2024-09-20"))
  result <- date_range(dates)

  expect_length(result, 2)
  expect_s3_class(result, "Date")
  expect_true(result[1] < as.Date("2024-03-15"))
  expect_true(result[2] > as.Date("2024-09-20"))
})

test_that("date_range works with single date", {
  dates <- as.Date("2024-06-15")
  result <- date_range(dates)

  expect_length(result, 2)
  expect_true(result[1] < dates)
  expect_true(result[2] > dates)
})

test_that("date_range works with character input", {
  result <- date_range(c("2024-03-15", "2024-09-20"))

  expect_length(result, 2)
  expect_s3_class(result, "Date")
})
