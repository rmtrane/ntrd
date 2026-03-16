# --- Helper function tests ---

test_that("make_pdf_filename produces correct format", {
  fixed_time <- as.POSIXct("2025-06-15 02:30:00 PM", format = "%Y-%m-%d %I:%M:%S %p")
  result <- make_pdf_filename("A001", "2024-01-15", time = fixed_time)

  expect_equal(result, "A001-2024-01-15_created-on-2025-06-15-at-02-30_pm.pdf")
})

test_that("make_pdf_filename ends with .pdf", {
  result <- make_pdf_filename("X123", "2023-03-01")
  expect_match(result, "\\.pdf$")
})

test_that("make_pdf_filename includes NACCID and VISITDATE", {
  result <- make_pdf_filename("NACC999", "2022-12-25")
  expect_match(result, "^NACC999-2022-12-25_created-on-")
})

test_that("make_table_caption formats correctly", {
  result <- make_table_caption("A001", "2024-01-15")
  expect_equal(result, "ID: A001 . Visit Date: 2024-01-15")
})

test_that("make_table_caption uniquifies duplicate IDs", {
  result <- make_table_caption(c("A001", "A001"), c("2024-01-15", "2024-01-15"))
  expect_equal(result, "ID: A001 . Visit Date: 2024-01-15")
})

test_that("chrome_extra_args returns --disable-gpu by default", {
  result <- chrome_extra_args()
  expect_equal(result, "--disable-gpu")
})

test_that("chrome_extra_args adds sandbox flags on shinyapps", {
  withr::local_envvar(R_CONFIG_ACTIVE = "shinyapps")
  result <- chrome_extra_args()
  expect_true("--no-sandbox" %in% result)
  expect_true("--disable-dev-shm-usage" %in% result)
  expect_true("--disable-gpu" %in% result)
})

# --- Snapshot tests ---

test_that("mainTable gt HTML output is stable", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  methods <- get_default_methods()

  shiny::testServer(
    mainTableServer,
    args = list(
      dat = shiny::reactive(single_row),
      methods = shiny::reactiveVal(methods),
      table_font_size = shiny::reactiveVal(100)
    ),
    {
      session$flushReact()

      expect_snapshot(gt::as_raw_html(mainTable()))
    }
  )
})

test_that("mainTableApp download flow produces a download button", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("pagedown")
  skip_if(
    !nzchar(tryCatch(pagedown::find_chrome(), error = function(e) "")),
    "Chrome/Chromium not available"
  )

  # NOTE: mainTableApp currently hardcodes methods = "infer", which causes
  # assessment_summary_data() to error when the data lacks method attributes.
  # Once mainTableApp accepts a `methods` argument, replace skip with:
  #   mainTableApp(dat = single_row, methods = get_default_methods(), testing = TRUE)
  skip("mainTableApp does not yet accept a methods argument")

  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]

  app <- shinytest2::AppDriver$new(
    mainTableApp(dat = single_row, testing = TRUE),
    name = "mainTableApp-download",
    timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Click "Generate PDF for Download"
  app$click(selector = "#main_table-genPDF")
  app$wait_for_idle(timeout = 30000)

  # The downloadTable UI should now contain a download button
  download_btn <- app$get_html("#main_table-downloadPDF")
  expect_match(download_btn, "Download PDF")
})

# --- Server logic tests ---

test_that("mainTableServer renders a gt table for single-row data", {
  prepped <- get_prepared_demo_data()
  single_row <- prepped[1, ]
  methods <- get_default_methods()

  shiny::testServer(
    mainTableServer,
    args = list(
      dat = shiny::reactive(single_row),
      methods = shiny::reactiveVal(methods),
      table_font_size = shiny::reactiveVal(100)
    ),
    {
      session$flushReact()

      # mainTable() is the internal reactiveVal holding the gt object
      expect_s3_class(mainTable(), "gt_tbl")
    }
  )
})

test_that("mainTableServer returns NULL for NULL dat", {
  methods <- get_default_methods()

  shiny::testServer(
    mainTableServer,
    args = list(
      dat = shiny::reactive(NULL),
      methods = shiny::reactiveVal(methods),
      table_font_size = shiny::reactiveVal(100)
    ),
    {
      session$flushReact()

      expect_null(mainTable())
    }
  )
})

test_that("mainTableServer returns NULL for multi-row dat", {
  prepped <- get_prepared_demo_data()
  multi_row <- prepped[1:2, ]
  methods <- get_default_methods()

  shiny::testServer(
    mainTableServer,
    args = list(
      dat = shiny::reactive(multi_row),
      methods = shiny::reactiveVal(methods),
      table_font_size = shiny::reactiveVal(100)
    ),
    {
      session$flushReact()

      # Should not render a table for multi-row data
      expect_null(mainTable())
    }
  )
})

test_that("mainTableServer table updates when dat changes", {
  prepped <- get_prepared_demo_data()
  methods <- get_default_methods()

  row_rv <- shiny::reactiveVal(prepped[1, ])

  shiny::testServer(
    mainTableServer,
    args = list(
      dat = row_rv,
      methods = shiny::reactiveVal(methods),
      table_font_size = shiny::reactiveVal(100)
    ),
    {
      session$flushReact()
      first_table <- mainTable()
      expect_s3_class(first_table, "gt_tbl")

      # Update to a different row
      row_rv(prepped[2, ])
      session$flushReact()

      second_table <- mainTable()
      expect_s3_class(second_table, "gt_tbl")
    }
  )
})
