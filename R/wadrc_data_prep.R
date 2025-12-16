#' Function to Prepare WADRC UDS-3 Data
#'
#' @param adrc_data Data as gotten from WADRC REDCap database.
#'  Could be a complete download to .csv file, then read into R,
#'  or pulled directly from REDCap using the `REDCapR` package.
#' @param uds Spefify if data are from UDS-2, UDS-3 or UDS-4 database.
#'
#' @keywords internal
wadrc_data_prep <- function(
  adrc_data,
  uds = c("uds2", "uds3", "uds4")
) {
  ## Due to NSE notes in R CMD check:
  # fmt: skip
  NACCID <-
    VISITDATE <-
    NACCUDSD <-
    contributions_character <-
    var <-
    val <-
    for_tab <-
    visityr <-
    visitmo <-
    visitday <-
    ptid <-
    race <-
    racewhite <-
    raceblack <-
    raceaian <-
    racenhpi <-
    raceasian <-
    racemena <-
    raceunkn <- 
    ethispanic <- 
      NULL

  if (!data.table::is.data.table(adrc_data)) {
    cli::cli_abort(
      "The {.arg adrc_data} must be a {.cls data.table} object, but is a {.cls {class(adrc_data)}}."
    )
  }

  if ("redcap_event_name" %in% colnames(adrc_data)) {
    out <- adrc_data[
      !grepl(pattern = "biomarker", x = adrc_data$redcap_event_name)
    ]
  } else {
    out <- adrc_data
  }

  if (uds == "uds2") {
    data.table::setnames(
      out,
      old = nacc_to_wadrc_uds2[c("NACCID", "VISITDAY", "VISITMO", "VISITYR")],
      new = c("ptid", "visitday", "visitmo", "visityr")
    )
  }

  if (uds == "uds3") {
    prefixes <- c(
      "",
      "fu_",
      "tcog_",
      "tcog_wadrc_c2_",
      "wadrc_c2_",
      "tcog_wadrc_c22_",
      "wadrc_c22_",
      "tip_",
      "tele_"
    )

    suffixes <- c(
      "",
      "",
      "",
      "_v1_1",
      "_v1_1",
      "",
      "",
      "",
      ""
    )

    for_diagnosis <- c(
      "normcog",
      "demented",
      "mciamem",
      "mciaplus",
      "mcinon1",
      "mcinon2",
      "impnomci"
    )

    # colnames(out)[grepl("education", colnames(out))] <- gsub(
    #   pattern = "education",
    #   replacement = "educ",
    #   x = colnames(out)[grepl("education", colnames(out))]
    # )

    out <- out[,
      intersect(
        colnames(out),
        unique(c(
          "ptid",
          "redcap_event_name",
          data.table::patterns("^visit", cols = colnames(out)),
          paste0(
            prefixes,
            rep(
              c(
                nacc_to_wadrc_uds3[
                  -which(names(nacc_to_wadrc_uds3) %in% c("EDUC", "RACE"))
                ], # Treat EDUC and RACE separately
                for_diagnosis
              ),
              each = length(prefixes)
            ),
            suffixes
          ),
          "educ",
          "tip_educ",
          "race",
          "tip_race"
        ))
      ),
      with = F
    ]

    rm_cols <- colnames(out)[grepl(pattern = "_visit", colnames(out))]

    if (length(rm_cols) > 0) {
      out[, (rm_cols) := NULL]
    }

    ## Combine columns that all relate to same variables
    for (var in setdiff(
      c(nacc_to_wadrc_uds3, for_diagnosis),
      c(
        "ptid",
        "redcap_event_name",
        data.table::patterns("visit", cols = colnames(out))
      )
    )) {
      alt_cols <- setdiff(
        colnames(out)[grepl(
          pattern = paste0(
            c("^", "_", "^", "_"),
            var,
            c("$", "$", "_", "_"),
            collapse = "|"
          ),
          x = colnames(out)
        )],
        var
      )

      out[,
        (var) := apply(.SD, 1, return_single, if_multiple = -1),
        .SDcols = intersect(c(var, alt_cols), colnames(out))
      ]
      out[, (alt_cols) := NULL]
    }
  }

  if (uds == "uds4") {
    # Keep only visits related to uds4
    out <- out[
      # !grepl(pattern = "biomarker", x = out$redcap_event_name)
      !grepl(pattern = "uds3", x = out$redcap_event_name)
    ]
    # Create visityr, visitmo, visitday
    out$visityr = lubridate::year(out$visitdate)
    out$visitmo = lubridate::month(out$visitdate)
    out$visitday = lubridate::day(out$visitdate)

    # Rename 'birthsex' to 'sex'
    colnames(out)[colnames(out) == "birthsex"] <- "sex"

    # Create 'race' column
    out[,
      race := data.table::fcase(
        racewhite == 1                  ,  1 ,
        raceblack == 1                  ,  2 ,
        raceaian == 1                   ,  3 ,
        racenhpi == 1                   ,  4 ,
        raceasian == 1                  ,  5 ,
        racemena == 1 | ethispanic == 1 , 50 ,
        raceunkn == 1                   , 99 ,
        default = NA
      )
    ]

    out$visitdate <- NULL

    # Make sure birthyr is numeric
    out$birthyr <- as.numeric(out$birthyr)
  }

  ## Remove redcap_event_name
  if ("redcap_event_name" %in% colnames(out)) {
    out$redcap_event_name <- NULL # out[, redcap_event_name := NULL]
  }

  out <- unique(out)

  ## Create diagnosis variable udsd
  out$udsd <- NA

  if (uds == "uds2") {
    out$udsd[out$normcog == 1] <- 1
    out$udsd[out$impnomci == 1] <- 2
    out$udsd[
      out$mciamem == 1 |
        out$mciaplus == 1 |
        out$mcinon1 == 1 |
        out$mcinon2 == 1
    ] <- 3
    out$udsd[out$demented == 1] <- 4
  }

  if (uds == "uds3") {
    out$udsd[out$normcog == 1] <- 1
    out$udsd[out$impnomci == 1] <- 2
    out$udsd[
      out$mciamem == 1 |
        out$mciaplus == 1 |
        out$mcinon1 == 1 |
        out$mcinon2 == 1
    ] <- 3
    out$udsd[out$demented == 1] <- 4
  }

  if (uds == "uds4") {
    out$udsd[out$normcog == 1] <- 1
    out$udsd[out$impnomci == 1] <- 2
    out$udsd[out$mci == 1] <- 3
    out$udsd[out$demented == 1] <- 4
  }

  out <- fill_data_downup(out)

  ## Rename all columns to match NACC naming scheme. To do so, create
  ## character vector of the form c("new_name" = "old_name", ...)

  ## For UDS2, exactly nacc_to_wadrc_uds2
  if (uds == "uds2") {
    cols_wanted <- c(
      "NACCID" = "ptid",
      "VISITYR" = "visityr",
      "VISITMO" = "visitmo",
      "VISITDAY" = "visitday",
      nacc_to_wadrc_uds2[
        !names(nacc_to_wadrc_uds2) %in%
          c("NACCID", "VISITYR", "VISITMO", "VISITDAY")
      ]
    )
  }

  ## For UDS3, this is exactly nacc_to_wadrc_uds3
  if (uds == "uds3") {
    cols_wanted <- nacc_to_wadrc_uds3
  }

  ## For UDS4, need to add visityr, visitmo, visitday, and sex, while removing
  ## visitdate and birthsex.
  if (uds == "uds4") {
    cols_wanted <- c(
      "VISITYR" = "visityr",
      "VISITMO" = "visitmo",
      "VISITDAY" = "visitday",
      "SEX" = "sex",
      nacc_to_wadrc_uds4[!nacc_to_wadrc_uds4 %in% c("birthsex", "visitdate")]
    )
  }

  out <- setNames(
    out[, cols_wanted, with = F],
    nm = names(cols_wanted)
  )

  out
}


return_single <- function(x, if_multiple = NA) {
  if (all(is.na(x))) {
    return(NA)
  }

  no_nas <- unique(na.omit(x))

  if (length(no_nas) > 1) {
    if (!is.null(if_multiple)) {
      return(if_multiple)
    }

    return(x)
  }

  no_nas
}


fill_data_downup <- function(
  out,
  ptid = "ptid",
  visityr = "visityr",
  visitmo = "visitmo",
  visitday = "visitday",
  educ = "educ",
  constant_across_visits = c("sex", "race", "birthyr", "birthmo", "handed")
) {
  data.table::setnames(
    out,
    old = c(ptid, visityr, visitmo, visitday, educ),
    new = c("ptid", "visityr", "visitmo", "visitday", "educ")
  )
  # remove_after <- c()
  # if (ptid != "ptid") {
  #   out$ptid <- out[[ptid]]
  #   remove_after <- c(remove_after, "ptid")
  # }
  # if (visityr != "visityr") {
  #   out$visityr <- out[[visityr]]
  #   remove_after <- c(remove_after, "visityr")
  # }
  # if (visitmo != "visitmo") {
  #   out$visitmo <- out[[visitmo]]
  #   remove_after <- c(remove_after, "visitmo")
  # }
  # if (visitday != "visitday") {
  #   out$visitday <- out[[visitday]]
  #   remove_after <- c(remove_after, "visitday")
  # }
  # if (educ != "educ") {
  #   out$educ <- out[[educ]]
  #   remove_after <- c(remove_after, "educ")
  # }

  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[, names(.SD) := lapply(.SD, as.numeric), .SDcols = is.logical]

  ## Fill NA within visits downup (i.e. first fill down, then fill up).
  ## For numeric values, use data.table::nafill directly. However, only
  ## do this for columns with missing data.
  num_cols <- lapply(out, \(x) is.numeric(x) && sum(is.na(x) > 0))

  out[,
    names(.SD) := lapply(
      .SD,
      \(x) {
        # If x of length 1, simply return x (there is nothing to fill...)
        if (length(x) == 1) {
          return(x)
        }

        data.table::nafill(x, type = "locf") |>
          data.table::nafill(type = "nocb")
      }
    ),
    .SDcols = names(num_cols[unlist(num_cols)]),
    by = c("ptid", "visityr", "visitmo", "visitday")
  ]

  ## For character columns, need workaround. First, make factor
  char_cols <- setdiff(
    colnames(out)[sapply(out, is.character)],
    c("ptid", "visityr", "visitmo", "visitday")
  )

  if (length(char_cols) > 0) {
    out[, (char_cols) := lapply(.SD, factor), .SDcols = char_cols]
    ## Next, get levels of factors (for later refill)
    lev = lapply(char_cols, function(x) levels(out[[x]])) |>
      setNames(char_cols)
    ## Now, make integer...
    out[, (char_cols) := lapply(.SD, as.integer), .SDcols = char_cols]
    ## ... then fill
    out[,
      (char_cols) := lapply(
        .SD,
        \(x) {
          if (length(x) == 1) {
            return(x)
          }

          data.table::nafill(x, 'locf') |>
            data.table::nafill(type = "nocb")
        }
      ),
      by = c("ptid", "visityr", "visitmo", "visitday"),
      .SDcols = char_cols
    ]

    ## Now, refill with levels to return to characters
    for (col in char_cols) {
      data.table::set(out, NULL, col, lev[[col]][out[[col]]])
    }
  }

  ## Remove duplicate rows if any
  out <- unique(out)

  ## Next we want to fill in sex, educ, race, handed, birthyr, birthmo by ptid
  ## First, make sure data.table is sorted appropriately
  out <- out[order(ptid, visityr, visitmo, visitday)]

  out[,
    names(.SD) := lapply(.SD, \(x) {
      if (length(x) == 1) {
        return(x)
      }

      if (all(is.na(x))) {
        return(x)
      }

      data.table::nafill(x, type = "locf") |>
        data.table::nafill(type = "nocb")
    }),
    .SDcols = intersect(colnames(out), constant_across_visits),
    # .SDcols = data.table::patterns(
    #   c("sex", "race", "birthyr", "birthmo", "handed"),
    #   cols = colnames(out)
    # ),
    by = "ptid"
  ]

  ## Remove duplicate rows if any
  out <- unique(out)

  ## Finally, if only a single non-missing educ value is present,
  ## copy to all entries. Otherwise, fill downup.
  out[,
    educ := {
      if (!all(is.na(educ))) {
        nonas <- na.omit(unique(educ))

        if (length(nonas) == 1) {
          educ <- rep(nonas, length(educ))
        }

        data.table::nafill(educ, type = "locf") |>
          data.table::nafill(type = "nocb")
      } else {
        educ
      }
    },
    by = "ptid"
  ]

  ## Rename columns back to original names
  data.table::setnames(
    out,
    old = c("ptid", "visityr", "visitmo", "visitday", "educ"),
    new = c(ptid, visityr, visitmo, visitday, educ)
  )

  return(out)
}
