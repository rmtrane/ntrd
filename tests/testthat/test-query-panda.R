test_that("adrc_ptid not string", {
  expect_error(
    get_biomarker_data(
      adrc_ptid = 12345,
      api_key = "NOT-A-REAL-API-KEY"
    ),
    regexp = "`adrc_ptid` must be a string"
  )

  expect_error(
    get_biomarker_data(
      adrc_ptid = c("adrc00031", "adrc00032"),
      api_key = "NOT-A-REAL-API-KEY"
    ),
    regexp = "`adrc_ptid` must be a string"
  )
})

test_that("packages missing", {
  with_mocked_bindings(
    {
      expect_error(
        get_biomarker_data(
          adrc_ptid = "adrc00031",
          api_key = "NOT-A-REAL-API-KEY"
        ),
        regexp = "Please install "
      )
    },
    is_installed = function(pkg, quietly = TRUE) {
      if (pkg %in% c("httr2", "jsonlite")) {
        return(FALSE)
      }
      rlang::is_installed(pkg)
    },
    .package = "rlang"
  )
})

test_that("Quering Panda when server unaccessible", {
  ## Skip if panda access
  skip_if(panda_access)

  panda_returns <- get_biomarker_data(
    adrc_ptid = "adrc00031",
    api_key = "NOT-A-REAL-API-KEY"
  )

  ## Check table names
  check_table_names(panda_returns)

  ## Check that all returns are try-errors
  purrr::walk(
    panda_returns,
    \(x) expect_s3_class(x, "try-error")
  )

  ## Check status codes
  purrr::walk(
    panda_returns,
    \(x) expect_equal(httr2::resp_status(attr(x, "condition")$resp), 403L)
  )
})

test_that("Querying Panda when accessible", {
  ## Skip if no panda access
  skip_if_not(panda_access)
  skip_if(is.null(getOption("panda_api_key")))

  bio_dat <- get_biomarker_data(
    adrc_ptid = "adrc00031",
    api_key = getOption("panda_api_key"),
    base_query_file = system.file(
      "json/panda_template.json",
      package = "ntrd"
    )
  )

  # expected_names <- c(
  #   "Local Roche CSF - Sarstedt freeze 2, cleaned",
  #   "Local Roche CSF - Sarstedt freeze 3",
  #   "Local Roche CSF - Sarstedt freeze, cleaned",
  #   "NTK MultiObs - CSF analytes",
  #   "NTK2 MultiObs - CSF, 20230311",
  #   "MK6240_NFT_Rating",
  #   "NAV4694 Visual Ratings",
  #   "PIB Visual Rating 20180126",
  #   "HDX Plasma - pTau217",
  #   "Amprion - CSF a-Synuclein"
  # )

  check_table_names(bio_dat)

  # expect_equal(
  #   names(bio_dat),
  #   expected_names
  # )

  ## Check that all returns are data.table's or NULL
  expect_true(
    all(purrr::map_lgl(bio_dat, \(x) {
      is.null(x) || data.table::is.data.table(x) || inherits(x, "error-message")
    }))
  )

  ## Check that all tables have expected column names
  bio_dat |>
    purrr::discard(is.null) |>
    purrr::discard(\(x) inherits(x, "error-message")) |>
    purrr::iwalk(check_table_colnames)

  for_gt <- bio_dat |>
    purrr::discard(is.null) |>
    purrr::discard(\(x) inherits(x, "error-message")) |>
    purrr::map(
      \(x) {
        x[,
          names(.SD) := lapply(.SD, \(y) round(y, digits = 4)),
          .SDcols = grep("_raw$", colnames(x), value = T)
        ]

        bio_tab_for_gt(x, return = "both")
      }
    )
  # lapply(bio_tab_for_gt)

  ## Make sure all tables are data.tables
  purrr::walk(
    for_gt,
    \(x) expect_true(data.table::is.data.table(x))
  )

  ## Snapshot all tables
  purrr::walk(
    for_gt,
    \(x) expect_snapshot_value(as.list(x), style = "json2")
  )

  all_values <- get_all_values()

  all_densities <- get_all_densities(all_values)
  all_cuts <- get_all_cuts(all_values)

  ## Snapshot the HTML output
  expect_snapshot(
    bslib::page(
      bio_tab_to_html_table(
        for_gt,
        densities = all_densities,
        cuts = all_cuts
      )
    )
  )

  ## We perform the same checks for a different patient ID to make sure we hit all tables
  bio_dat <- get_biomarker_data(
    adrc_ptid = "adrc01261",
    api_key = getOption("panda_api_key")
  )

  # expect_equal(
  #   names(bio_dat),
  #   expected_names
  # )
  check_table_names(bio_dat)

  # Check that all returns are data.table's or NULL
  expect_true(
    all(purrr::map_lgl(bio_dat, \(x) {
      is.null(x) | data.table::is.data.table(x) | inherits(x, "error-message")
    }))
  )

  # Check that all tables have expected column names
  bio_dat |>
    purrr::discard(is.null) |>
    purrr::discard(\(x) inherits(x, "error-message")) |>
    purrr::iwalk(check_table_colnames)
})
