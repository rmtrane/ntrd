# --- data_load() tests for demo_source and csv_source ---

test_that("data_load(demo_source()) returns a data_nacc object", {
  result <- data_load(demo_source())

  expect_true(S7::S7_inherits(result, data_nacc))
})

test_that("data_load(demo_source()) contains derived columns", {
  result <- data_load(demo_source())
  dat <- result@data

  derived_cols <- c("REYTOTAL", "REYAREC", "FAS", "MOCACLOCK")
  for (col in derived_cols) {
    expect_true(
      col %in% colnames(dat),
      label = paste("Expected derived column", col, "in data")
    )
  }
})

test_that("data_load(demo_source()) contains npsych_scores columns", {
  result <- data_load(demo_source())
  dat <- result@data

  npsych_cols <- intersect(colnames(dat), ntrs::list_npsych_scores())
  expect_true(length(npsych_cols) > 0)

  # Verify at least one column has the npsych_scores S7 class
  first_col <- dat[[npsych_cols[1]]]
  expect_true(S7::S7_inherits(first_col, ntrs::npsych_scores))
})

test_that("data_load(csv_source()) loads from CSV file", {
  # Write demo_data to a temp CSV
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)

  data.table::fwrite(demo_data, tmp_file)

  result <- data_load(csv_source(), file_path = tmp_file)

  expect_true(S7::S7_inherits(result, data_nacc))

  # Verify npsych_scores columns are created
  dat <- result@data
  npsych_cols <- intersect(colnames(dat), ntrs::list_npsych_scores())
  expect_true(length(npsych_cols) > 0)
  first_col <- dat[[npsych_cols[1]]]
  expect_true(S7::S7_inherits(first_col, ntrs::npsych_scores))
})
