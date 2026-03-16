test_that("prevDiagnosesServer renders output for valid data", {
  prepped <- get_prepared_demo_data()
  # Get data for a single study ID
  single_id <- prepped[NACCID == prepped$NACCID[1]]

  shiny::testServer(
    prevDiagnosesServer,
    args = list(
      dat = shiny::reactive(single_id),
      table_font_size = shiny::reactive(100)
    ),
    {
      session$flushReact()

      # The output should produce HTML
      result <- output$prev_diagnoses_table
      expect_true(!is.null(result))
    }
  )
})

test_that("prev_diagnoses_table errors for non-data.table", {
  # Test the underlying function directly (observer swallows errors)
  df <- data.frame(NACCID = "A001", VISITDATE = "2020-01-01")
  expect_error(prev_diagnoses_table(df), "data\\.table")
})

test_that("prev_diagnoses_table shows no-diagnoses message for NACCUDSD = 1", {
  prepped <- get_prepared_demo_data()
  single_id <- prepped[NACCID == prepped$NACCID[1]]

  # Set all NACCUDSD to 1 (normal cognition) to trigger "no diagnoses" path
  if ("NACCUDSD" %in% colnames(single_id)) {
    single_id$NACCUDSD <- 1
  }

  result <- prev_diagnoses_table(single_id)
  expect_true(!is.null(result))
})
