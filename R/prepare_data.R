#' Wrapper to prepare raw data
#'
#' This is a wrapper function that returns only the desired columns, adds
#' variables that can be derived to the data, and adds standardized scores.
#'
#' @param dat data set similar to the NACC data. For an example, see
#'   `?demo_data`.
#' @inheritParams ntrs::std_data
#'
#'
#' @export
prepare_data <- function(
  dat,
  methods = NULL
) {
  # Avoid R CMD check notes
  NACCAGE <- SEX <- EDUC <- RACE <- MEMTIME <- NULL

  if (!S7::S7_inherits(dat, data_nacc)) {
    cli::cli_abort(
      "{.arg dat} must be of class {.cls data_nacc}, but is {.cls {class(dat)}}."
    )
  }

  # Extract data slot
  dat <- dat@data

  if (is.null(methods)) {
    # Get defaults by examining the npsych_scores objects
    methods <- dat[0, .SD, .SDcols = ntrs::is_npsych_scores] |> # NpsychBatteryNorms::default_methods
      purrr::map(\(x) suppressMessages(get_std_defaults(x))) |>
      purrr::discard(purrr::is_null)
  }

  ## Remove 'empty' rows, i.e. rows with no NACCID, VISIT*, BIRTH* available. This is an edge case, that shouldn't really happen.
  dat <- dat[
    rowSums(
      !is.na(dat[,
        setdiff(
          colnames(dat),
          c(
            "NACCID",
            data.table::patterns("^VISIT|^BIRTH", cols = colnames(dat))
          )
        ),
        with = F
      ])
    ) >
      0
  ]

  if (all(c("OTHCOG", "OTHCOGX") %in% colnames(dat))) {
    dat$OTHCOG <- with(
      dat,
      ifelse(OTHCOG == 1, as.character(OTHCOGX), as.character(OTHCOG))
    )
  }

  if (all(c("OTHPSY", "OTHPSYX") %in% colnames(dat))) {
    dat$OTHPSY <- with(
      dat,
      ifelse(OTHPSY == 1, as.character(OTHPSYX), as.character(OTHPSY))
    )
  }

  if (all(c("COGOTH", "COGOTHX") %in% colnames(dat))) {
    dat$COGOTH <- with(
      dat,
      ifelse(COGOTH == 1, as.character(COGOTHX), as.character(COGOTH))
    )
  }

  if (all(c("COGOTH2", "COGOTH2X") %in% colnames(dat))) {
    dat$COGOTH2 <- with(
      dat,
      ifelse(
        COGOTH2 == 1,
        as.character(COGOTH2X),
        as.character(COGOTH2)
      )
    )
  }

  if (all(c("COGOTH3", "COGOTH3X") %in% colnames(dat))) {
    dat$COGOTH3 <- with(
      dat,
      ifelse(
        COGOTH3 == 1,
        as.character(COGOTH3X),
        as.character(COGOTH3)
      )
    )
  }

  diag_renames <- c(
    setNames(
      paste0(diag_contr_pairs$presump_etio_diag, "_etiology"),
      diag_contr_pairs$presump_etio_diag
    ),
    setNames(
      paste0(diag_contr_pairs$presump_etio_diag, "_contribution"),
      nm = diag_contr_pairs$contribution
    )
  )

  colnames(dat)[
    colnames(dat) %in% names(diag_renames)
  ] <- diag_renames[colnames(dat)[colnames(dat) %in% names(diag_renames)]]

  ntrs::std_data(
    dat,
    age = NACCAGE,
    sex = SEX,
    educ = EDUC,
    race = RACE,
    delay = if ("MEMTIME" %in% colnames(dat)) MEMTIME else NULL,
    methods = methods,
    prefix_std = "std_",
    prefix_raw = "raw_",
    .cols = names(methods)
  )
}
