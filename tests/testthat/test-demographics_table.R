test_that("demographics_table errors when input is not data.table", {
  expect_error(
    demographics_table(data.frame(NACCID = "001")),
    "data.table"
  )
})

test_that("demographics_table errors when multiple NACCID values", {
  dt <- data.table::data.table(
    NACCID = c("001", "002"),
    SEX = c(1, 2),
    EDUC = c(12, 16),
    BIRTHYR = c(1950, 1960)
  )
  expect_error(demographics_table(dt), "one study ID")
})

test_that("demographics_table returns gt_tbl for valid single-patient data", {
  dt <- data.table::data.table(
    NACCID = "001",
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1955,
    RACE = 1
  )
  result <- demographics_table(dt)
  expect_s3_class(result, "gt_tbl")
})

test_that("demographics_table handles missing optional columns", {
  # Without HANDED column
  dt <- data.table::data.table(
    NACCID = "001",
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1955
  )
  result <- demographics_table(dt)
  expect_s3_class(result, "gt_tbl")
})

test_that("demographics_table converts numeric SEX codes to labels", {
  dt <- data.table::data.table(
    NACCID = "001",
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1955
  )
  result <- demographics_table(dt)

  # The rendered data should have a human-readable label, not the numeric code
  tbl_data <- result[["_data"]]
  sex_row <- tbl_data[tbl_data$name == "Gender:", ]
  expect_false(sex_row$value == "1")
})

test_that("demographics_table works with demo_data", {
  # Use first participant from demo_data
  first_id <- demo_data$NACCID[1]
  dt <- demo_data[NACCID == first_id]
  result <- demographics_table(dt)
  expect_s3_class(result, "gt_tbl")
})

test_that("demographics_table adds footnote when values are NA", {
  dt <- data.table::data.table(
    NACCID = "001",
    SEX = NA,
    EDUC = 16,
    BIRTHYR = 1955
  )
  result <- demographics_table(dt)

  # Should have a footnote about missing values
  footnotes <- result[["_footnotes"]]
  expect_true(nrow(footnotes) > 0)
})

test_that("demographics_table handles multi-visit with conflicting values", {
  dt <- data.table::data.table(
    NACCID = c("001", "001"),
    SEX = c(1, 1),
    EDUC = c(12, 16),
    BIRTHYR = c(1955, 1955)
  )
  result <- demographics_table(dt)
  expect_s3_class(result, "gt_tbl")

  # Should have a footnote about varying values
  footnotes <- result[["_footnotes"]]
  expect_true(nrow(footnotes) > 0)
})
