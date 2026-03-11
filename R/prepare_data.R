#' Wrapper to prepare raw data
#'
#' This is a wrapper function that returns only the desired columns, adds
#' variables that can be derived to the data, and adds standardized scores.
#'
#' @param dat data set similar to the NACC data. For an example, see
#'   `?demo_data`.
#' @param selected_cols vector with columns to keep. If named, the names must
#'   correspond to NACC variable names and entries columns in the data set `dat`.
#' @param with_diags logical (default `TRUE`); should diagnoses be included?
#'
#' @inheritParams NpsychBatteryNorms::add_standardized_scores
#'
#' @examples prepare_data(demo_data)
#'
#' @export
prepare_data <- function(
  dat,
  # selected_cols = c(
  #   "RACE",
  #   "CDRGLOB",
  #   "MOCATOTS",
  #   "MOCBTOTS",
  #   "TRAILA",
  #   "TRAILARR",
  #   "TRAILALI",
  #   "OTRAILA",
  #   "OTRLARR",
  #   "DIGFORCT",
  #   "DIGFORSL",
  #   "DIGBACCT",
  #   "DIGBACLS",
  #   "WAIS",
  #   "MINTTOTS",
  #   "ANIMALS",
  #   "VEG",
  #   "UDSVERTN",
  #   "UDSVERFC",
  #   "UDSVERLC",
  #   "UDSBENTC",
  #   "UDSBENTD",
  #   "CRAFTVRS",
  #   "CRAFTURS",
  #   "CRAFTDVR",
  #   "CRAFTDRE",
  #   "REY1REC",
  #   "REY2REC",
  #   "REY3REC",
  #   "REY4REC",
  #   "REY5REC",
  #   "REY6REC",
  #   "REYDREC",
  #   "REYTCOR",
  #   "TRAILB",
  #   "TRAILBLI",
  #   "TRAILBRR",
  #   "MOCACLOC",
  #   "MOCACLOH",
  #   "MOCACLON",
  #   "OTRAILB",
  #   "OTRLBRR",
  #   "OTRLBLI",
  #   "NACCGDS",
  #   "CDRSUM",
  #   "UDSBENRS",
  #   "NACCID",
  #   "SEX",
  #   "EDUC",
  #   "HANDED",
  #   "NACCAGE",
  #   "BIRTHYR",
  #   "VISITYR",
  #   "VISITMO",
  #   "VISITDAY",
  #   "NACCUDSD",
  #   "NACCMMSE",
  #   "BOSTON",
  #   "LOGIMEM",
  #   "MEMUNITS",
  #   "MEMTIME",
  #   "DIGIF",
  #   "DIGIFLEN",
  #   "DIGIB",
  #   "DIGIBLEN"
  # ),
  methods = NULL,
  print_messages = F,
  with_diags = T
) {
  if (!S7::S7_inherits(dat, data_nacc)) {
    cli::cli_abort(
      "{.arg dat} must be of class {.cls data_nacc}, but is {.cls {class(dat)}}."
    )
  }

  # Extract data slot
  dat <- dat@data

  # other_cols <- intersect(
  #   c(
  #     with(
  #       diag_contr_pairs,
  #       sort(c(presump_etio_diag, contribution, na.omit(other)))
  #     ),
  #     unlist(lapply(
  #       list(
  #         # NpsychBatteryNorms::calculate_fas,
  #         ntrs::calc_FAS,
  #         # NpsychBatteryNorms::calculate_mocaclock,
  #         ntrs::calc_MOCACLOCK,
  #         # NpsychBatteryNorms::calculate_reyarec,
  #         ntrs::calc_REYAREC,
  #         var_labels
  #       ),
  #       formalArgs
  #     ))
  #   ),
  #   colnames(dat)
  # )

  # ## Remove other_cols that were included in selected_cols
  # other_cols <- setdiff(other_cols, selected_cols)

  # ## Final vector of columns to select WITH NAMES!!!
  # cols_to_select <- c(selected_cols, other_cols)

  # dat <- dat[, cols_to_select, with = F]

  # ## Fix missing names
  # names(cols_to_select) <- ifelse(
  #   names(cols_to_select) == "",
  #   cols_to_select,
  #   names(cols_to_select)
  # )

  # ## Change column names
  # colnames(dat) <- setNames(names(cols_to_select), cols_to_select)[colnames(
  #   dat
  # )]

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

  # if (!"VISITDATE" %in% colnames(dat)) {
  #   dat$VISITDATE <- as.Date(ifelse(
  #     test = is.na(dat$VISITYR) | is.na(dat$VISITMO) | is.na(dat$VISITDAY),
  #     yes = NA,
  #     no = paste(dat$VISITYR, dat$VISITMO, dat$VISITDAY, sep = "-")
  #   ))
  # }

  # if (!"NACCAGE" %in% colnames(dat)) {
  #   dat$NACCAGE <- lubridate::time_length(
  #     dat$VISITDATE -
  #       as.Date(ifelse(
  #         test = is.na(dat$BIRTHYR) | is.na(dat$BIRTHMO),
  #         yes = NA,
  #         no = paste(dat$BIRTHYR, dat$BIRTHMO, 15, sep = "-")
  #       )),
  #     unit = "years"
  #   )
  # }

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
