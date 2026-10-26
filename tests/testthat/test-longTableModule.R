# --- longTableModule tests ---

test_that("longTableServer renders HTML output for multi-visit data", {
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

  shiny::testServer(
    longTableServer,
    args = list(
      dat = multi_visit_data,
      methods = methods
    ),
    {
      # Trigger the renderUI
      result <- output$long_table
      expect_true(!is.null(result))
    }
  )
})

test_that("longTableServer wraps non-reactive args in reactiveVal", {
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

  # Pass plain values (not reactives) - should not error
  shiny::testServer(
    longTableServer,
    args = list(
      dat = multi_visit_data,
      methods = methods,
      table_font_size = 80,
      descriptions = c(
        "Impaired" = 0.03,
        "Borderline" = 0.10,
        "Low Average" = 0.26,
        "Average" = 0.76,
        "High Average" = 0.92,
        "Superior" = 0.97,
        "Very Superior" = 1
      )
    ),
    {
      result <- output$long_table
      expect_true(!is.null(result))
    }
  )
})

test_that("longTableUI returns a tagList", {
  ui <- longTableUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})
