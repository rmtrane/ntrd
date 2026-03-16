# Helper: create prepared demo data for module tests that need it.
# This is loaded automatically by testthat before tests run.
get_prepared_demo_data <- function() {
  tmp <- data.table::copy(demo_data)

  tmp[,
    names(.SD) := purrr::imap(.SD, \(x, idx) {
      ntrs::get_npsych_scores(idx)(x)
    }),
    .SDcols = intersect(colnames(tmp), ntrs::list_npsych_scores())
  ]

  tmp[,
    c("REYTOTAL", "REYAREC", "FAS", "MOCACLOCK") := list(
      ntrs::calc_REYTOTAL(REY1REC, REY2REC, REY3REC, REY4REC, REY5REC),
      ntrs::calc_REYAREC(REYTCOR, REYFPOS),
      ntrs::calc_FAS(
        BILLS, TAXES, SHOPPING, GAMES, STOVE,
        MEALPREP, EVENTS, PAYATTN, REMDATES, TRAVEL
      ),
      ntrs::calc_MOCACLOCK(MOCACLOC, MOCACLON, MOCACLOH)
    )
  ]

  obj <- data_nacc(data = tmp)
  suppressWarnings(prepare_data(obj))
}

# Helper: get default standardization methods from the demo data
get_default_methods <- function() {
  lapply(
    setNames(ntrs::list_npsych_scores(), ntrs::list_npsych_scores()),
    \(x) {
      ntrs::get_std_defaults(ntrs::get_npsych_scores(x)())
    }
  ) |>
    purrr::discard(is.null)
}
