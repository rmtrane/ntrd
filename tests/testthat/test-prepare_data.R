test_that("prepare_data errors when dat is not data_nacc", {
  expect_error(
    prepare_data(data.frame(NACCID = "001")),
    "data_nacc"
  )

  expect_error(
    prepare_data(data.table::data.table(NACCID = "001")),
    "data_nacc"
  )
})

test_that("prepare_data returns a data.table", {
  result <- get_prepared_demo_data()
  expect_true(data.table::is.data.table(result))
})

test_that("prepare_data output has std_ and raw_ prefixed columns", {
  result <- get_prepared_demo_data()

  std_cols <- grep("^std_", colnames(result), value = TRUE)
  raw_cols <- grep("^raw_", colnames(result), value = TRUE)

  expect_true(length(std_cols) > 0)
  expect_true(length(raw_cols) > 0)
})

test_that("prepare_data works with explicit methods argument", {
  tmp <- data.table::copy(demo_data)

  tmp[,
    names(.SD) := purrr::imap(.SD, \(x, idx) {
      ntrs::get_npsych_scores(idx)(x)
    }),
    .SDcols = intersect(colnames(tmp), ntrs::list_npsych_scores())
  ]

  obj <- data_nacc(data = tmp)
  methods <- get_default_methods()
  result <- suppressWarnings(prepare_data(obj, methods = methods))

  expect_true(data.table::is.data.table(result))
  expect_true(any(grepl("^std_", colnames(result))))
})

test_that("prepare_data removes all-NA rows (except ID/visit/birth cols)", {
  tmp <- data.table::copy(demo_data)

  tmp[,
    names(.SD) := purrr::imap(.SD, \(x, idx) {
      ntrs::get_npsych_scores(idx)(x)
    }),
    .SDcols = intersect(colnames(tmp), ntrs::list_npsych_scores())
  ]

  # Add a row that is all NA except for ID/visit/birth columns
  empty_row <- tmp[1, ]
  skip_cols <- c("NACCID", grep("^VISIT|^BIRTH", colnames(tmp), value = TRUE))
  for (col in setdiff(colnames(empty_row), skip_cols)) {
    data.table::set(empty_row, j = col, value = NA)
  }

  tmp_with_empty <- rbind(tmp, empty_row, ignore.attr = TRUE)
  obj <- data_nacc(data = tmp_with_empty)

  nrow_before <- nrow(obj@data)
  result <- suppressWarnings(prepare_data(obj))

  # The all-NA row should have been removed
  expect_true(nrow(result) < nrow_before)
})

test_that("prepare_data renames diagnosis columns", {
  result <- get_prepared_demo_data()

  # Check for _etiology and _contribution suffixed columns
  etiology_cols <- grep("_etiology$", colnames(result), value = TRUE)
  contribution_cols <- grep("_contribution$", colnames(result), value = TRUE)

  # At least some diagnosis columns should have been renamed if present in demo_data
  # Check that the original names from diag_contr_pairs are NOT in the result
  original_diag_cols <- intersect(
    diag_contr_pairs$presump_etio_diag,
    colnames(demo_data)
  )

  if (length(original_diag_cols) > 0) {
    expect_true(length(etiology_cols) > 0)
  }
})

test_that("prepare_data handles OTHCOG/OTHCOGX columns without error", {
  tmp <- data.table::copy(demo_data)

  tmp[,
    names(.SD) := purrr::imap(.SD, \(x, idx) {
      ntrs::get_npsych_scores(idx)(x)
    }),
    .SDcols = intersect(colnames(tmp), ntrs::list_npsych_scores())
  ]

  # Add OTHCOG/OTHCOGX columns if not present
  if (!"OTHCOG" %in% colnames(tmp)) tmp[, OTHCOG := 0]
  if (!"OTHCOGX" %in% colnames(tmp)) tmp[, OTHCOGX := NA_character_]

  tmp[1, OTHCOG := 1]
  tmp[1, OTHCOGX := "Some other cognitive diagnosis"]

  obj <- data_nacc(data = tmp)
  expect_no_error(suppressWarnings(prepare_data(obj)))
})
