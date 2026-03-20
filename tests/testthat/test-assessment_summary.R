# --- assessment_summary_data() tests ---

test_that("assessment_summary_data returns expected list structure", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  result <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("for_main_table", "fill_values", "footnotes"),
    ignore.order = TRUE
  )
  expect_s3_class(result$for_main_table, "data.table")
  expect_type(result$fill_values, "character")
})

test_that("assessment_summary_data for_main_table has required columns", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  result <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  expected_cols <- c(
    "group",
    "labels",
    "name",
    "raw",
    "raw_suffix",
    "units",
    "std",
    "Percentile",
    "Description",
    "is_error"
  )
  expect_true(all(expected_cols %in% colnames(result$for_main_table)))
})

test_that("assessment_summary_data Percentile is between 0 and 100", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  result <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  pctiles <- result$for_main_table$Percentile
  expect_true(all(pctiles >= 0 & pctiles <= 100, na.rm = TRUE))
})

test_that("assessment_summary_data Description values match description names", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  descriptions <- c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  )

  result <- assessment_summary_data(
    dat = single_row,
    descriptions = descriptions,
    include_caption = FALSE
  )

  desc_vals <- result$for_main_table$Description
  expect_true(all(desc_vals %in% c(names(descriptions), NA)))
})

test_that("assessment_summary_data errors on multi-row data", {
  prepped <- get_prepared_demo_data()
  multi_row <- prepped[1:2, ]

  expect_error(
    assessment_summary_data(dat = multi_row),
    "exactly one row"
  )
})

test_that("assessment_summary_data errors on non-data.table input", {
  prepped <- get_prepared_demo_data()
  df <- as.data.frame(prepped[1, ])

  expect_error(
    assessment_summary_data(dat = df),
    "data.table"
  )
})

test_that("assessment_summary_data errors when id column missing", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]

  expect_error(
    assessment_summary_data(dat = single_row, id = "NONEXISTENT"),
    "id"
  )
})

test_that("assessment_summary_data fill_values has same names as descriptions", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]

  descriptions <- c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  )

  result <- assessment_summary_data(
    dat = single_row,
    descriptions = descriptions,
    include_caption = FALSE
  )

  expect_equal(names(result$fill_values), names(descriptions))
})

test_that("assessment_summary_data custom fill_values are preserved", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]

  descriptions <- c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  )

  custom_fills <- setNames(
    c(
      "#FF0000",
      "#FF6600",
      "#FFCC00",
      "#FFFF00",
      "#99FF00",
      "#33FF00",
      "#00FF00"
    ),
    names(descriptions)
  )

  result <- assessment_summary_data(
    dat = single_row,
    descriptions = descriptions,
    fill_values = custom_fills,
    include_caption = FALSE
  )

  expect_equal(result$fill_values, custom_fills)
})

test_that("assessment_summary_data includes caption when requested", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]

  result <- assessment_summary_data(
    dat = single_row,
    include_caption = TRUE
  )

  expect_true("cap" %in% names(result))
  expect_s3_class(result$cap, "data.table")
})

test_that("assessment_summary_data excludes caption when not requested", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  result <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  expect_false("cap" %in% names(result))
})

test_that("assessment_summary_data footnotes contain logical vectors", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  result <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  expect_true("footnotes" %in% names(result))

  fn <- result$footnotes
  # Footnotes are now keyed by @description strings from std_npsych_scores columns
  expect_true(length(fn) > 0)
  expect_true(all(nchar(names(fn)) > 0))

  n_rows <- nrow(result$for_main_table)
  for (nm in names(fn)) {
    expect_type(fn[[nm]], "logical")
    expect_length(fn[[nm]], n_rows)
  }
})

test_that("assessment_summary_data produces consistent results across rows", {
  prepped <- get_prepared_demo_data()

  result1 <- assessment_summary_data(
    dat = prepped[1, ],
    include_caption = FALSE
  )

  result2 <- assessment_summary_data(
    dat = prepped[2, ],
    include_caption = FALSE
  )

  # Both should have the same column structure
  expect_equal(
    colnames(result1$for_main_table),
    colnames(result2$for_main_table)
  )
})

# --- assessment_summary_table() tests ---

test_that("assessment_summary_table returns a gt_tbl object", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  summary_dat <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  tbl <- assessment_summary_table(summary_dat = summary_dat)

  expect_s3_class(tbl, "gt_tbl")
})

test_that("assessment_summary_table has correct table id", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  summary_dat <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  tbl <- assessment_summary_table(summary_dat = summary_dat)

  table_id_val <- tbl$`_options`$value[tbl$`_options`$parameter == "table_id"]
  expect_equal(table_id_val[[1]], "assessment-summary-table")
})

test_that("assessment_summary_table works with caption", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  summary_dat <- assessment_summary_data(
    dat = single_row,
    include_caption = TRUE
  )

  tbl <- assessment_summary_table(summary_dat = summary_dat)

  expect_s3_class(tbl, "gt_tbl")
  # Caption data should result in a title being set
  expect_true(!is.null(tbl$`_heading`$title))
})

test_that("assessment_summary_table renders to HTML without error", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  summary_dat <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  tbl <- assessment_summary_table(summary_dat = summary_dat)

  html <- gt::as_raw_html(tbl)
  expect_type(html, "character")
  expect_true(nchar(html) > 0)
})

test_that("assessment_summary_table works for multiple different rows", {
  prepped <- get_prepared_demo_data()

  for (i in 1:min(3, nrow(prepped))) {
    summary_dat <- assessment_summary_data(
      dat = prepped[i, ],
      include_caption = FALSE
    )

    tbl <- assessment_summary_table(summary_dat = summary_dat)
    expect_s3_class(tbl, "gt_tbl")
  }
})

test_that("assessment_summary_table bar_height parameter is respected", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  summary_dat <- assessment_summary_data(
    dat = single_row,
    include_caption = FALSE
  )

  # Should not error with different bar heights
  tbl_small <- assessment_summary_table(
    summary_dat = summary_dat,
    bar_height = 8
  )
  tbl_large <- assessment_summary_table(
    summary_dat = summary_dat,
    bar_height = 32
  )

  expect_s3_class(tbl_small, "gt_tbl")
  expect_s3_class(tbl_large, "gt_tbl")
})

# --- assessment_longitudinal_table() tests ---

test_that("assessment_longitudinal_table returns HTML for valid multi-row data", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  # Get data for one individual with multiple visits
  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  result <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods
  )

  expect_s3_class(result, "html")
})

test_that("assessment_longitudinal_table errors on non-data.table", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  df <- as.data.frame(prepped[NACCID == multi_visit_id])

  expect_error(
    assessment_longitudinal_table(dat = df, methods = methods),
    "data.table"
  )
})

test_that("assessment_longitudinal_table errors when id column missing", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  expect_error(
    assessment_longitudinal_table(
      dat = multi_visit_data,
      id = "BADID",
      methods = methods
    ),
    "id"
  )
})

test_that("assessment_longitudinal_table errors when date column missing", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  expect_error(
    assessment_longitudinal_table(
      dat = multi_visit_data,
      date = "BADDATE",
      methods = methods
    ),
    "date"
  )
})

test_that("assessment_longitudinal_table errors for multiple individuals", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  skip_if(
    length(unique(prepped$NACCID)) < 2,
    "Need at least 2 individuals in demo data"
  )

  # The function may hit duplicate dates or multiple individuals check first
  expect_error(
    assessment_longitudinal_table(dat = prepped, methods = methods)
  )
})

test_that("assessment_longitudinal_table custom table_id is used", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  result <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods,
    table_id = "my-custom-id"
  )

  expect_true(grepl("my-custom-id", as.character(result)))
})

test_that("assessment_longitudinal_table show_all_visits parameter works", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  # Both options should work without error
  result_all <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods,
    show_all_visits = TRUE
  )

  result_subset <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods,
    show_all_visits = FALSE
  )

  expect_s3_class(result_all, "html")
  expect_s3_class(result_subset, "html")
})

test_that("assessment_longitudinal_table table_font_size parameter works", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  result <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods,
    table_font_size = 80
  )

  expect_s3_class(result, "html")
})

test_that("assessment_longitudinal_table returns message when no scores found", {
  prepped <- get_prepared_demo_data()

  # Create a minimal data.table with only required columns but no score columns
  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  minimal_dat <- data.table::data.table(
    NACCID = rep(multi_visit_id, 2),
    VISITDATE = as.Date(c("2020-01-01", "2021-01-01"))
  )

  result <- assessment_longitudinal_table(
    dat = minimal_dat,
    methods = list()
  )

  # Should return an h3 "No scores found." message
  expect_true(inherits(result, "shiny.tag"))
})

test_that("assessment_longitudinal_table errors on non-Date date column", {
  prepped <- get_prepared_demo_data()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- data.table::copy(prepped[NACCID == multi_visit_id])
  # Convert date to character to trigger error
  multi_visit_data[, VISITDATE := as.character(VISITDATE)]

  expect_error(
    assessment_longitudinal_table(dat = multi_visit_data, methods = list()),
    "dates"
  )
})

test_that("assessment_longitudinal_table errors on duplicate dates", {
  prepped <- get_prepared_demo_data()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- data.table::copy(prepped[NACCID == multi_visit_id])
  # Set all dates to the same value to trigger duplicate error
  multi_visit_data[, VISITDATE := as.Date("2020-01-01")]

  expect_error(
    assessment_longitudinal_table(dat = multi_visit_data, methods = list()),
    "duplicates"
  )
})

test_that("assessment_longitudinal_table stubhead_label parameter works", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  result <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods,
    stubhead_label = "Test Label"
  )

  expect_s3_class(result, "html")
  expect_true(grepl("Test Label", as.character(result)))
})

test_that("assessment_longitudinal_table crosswalk pairs are merged", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  # Find an individual that has both members of a crosswalk pair with non-NA values
  crosswalk_pairs <- list(
    c("MOCATOTS", "NACCMMSE"),
    c("MINTTOTS", "BOSTON"),
    c("CRAFTURS", "LOGIMEM"),
    c("CRAFTDRE", "MEMUNITS")
  )

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_ids <- names(id_counts[id_counts > 1])

  skip_if(
    length(multi_visit_ids) == 0,
    "No individual with multiple visits in demo data"
  )

  # For each multi-visit individual, check if any crosswalk pair has both
  # members with non-NA std_npsych_scores values
  found <- FALSE
  test_data <- NULL

  for (mid in multi_visit_ids) {
    dat_i <- prepped[NACCID == mid]

    npsych_class_map <- unlist(dat_i[,
      lapply(.SD, \(x) S7::S7_class(x)@name),
      .SDcols = ntrs::is_npsych_scores
    ])

    std_subclass_map <- unlist(dat_i[,
      lapply(.SD, \(x) x@scores_subclass),
      .SDcols = \(x) S7::S7_inherits(x, ntrs::std_npsych_scores)
    ])

    for (pair in crosswalk_pairs) {
      if (all(pair %in% std_subclass_map)) {
        # Check that at least one row has a non-NA value for each member
        cols_for_pair <- names(std_subclass_map)[std_subclass_map %in% pair]
        has_vals <- vapply(cols_for_pair, \(col) any(!is.na(dat_i[[col]])), logical(1))
        if (all(has_vals)) {
          found <- TRUE
          test_data <- dat_i
          break
        }
      }
    }
    if (found) break
  }

  skip_if(!found, "No crosswalk pairs with non-NA data found in demo data")

  result <- assessment_longitudinal_table(
    dat = test_data,
    methods = methods
  )

  # If crosswalk pairs are merged, the combined label uses " /" separator
  # and includes <u><i> markup for the legacy score
  result_str <- paste(as.character(result), collapse = "")
  expect_true(
    grepl("<u><i>", result_str, fixed = TRUE),
    label = "Expected crosswalk pair label with underline/italic markup"
  )
})

test_that("assessment_longitudinal_table source note with methods", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  ids <- prepped$NACCID
  id_counts <- table(ids)
  multi_visit_id <- names(id_counts[id_counts > 1])[1]

  skip_if(
    is.na(multi_visit_id),
    "No individual with multiple visits in demo data"
  )

  multi_visit_data <- prepped[NACCID == multi_visit_id]

  result <- assessment_longitudinal_table(
    dat = multi_visit_data,
    methods = methods
  )

  result_str <- as.character(result)
  expect_true(
    grepl("Standardization Methods", result_str),
    label = "Expected source note with standardization methods tooltip"
  )
})
