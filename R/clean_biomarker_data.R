#' Clean biomarker data
#'
#' @description
#' Clean up biomarker data from Panda.
#'
#' @param as_df A data.table of biomarker data. Should be the result of `jsonlite::fromJSON` applied to the result of API call.
#' @param table_name A single string indicating the name of the table from which the data originated.
#'
#' @returns
#' A cleaned `data.table` of biomarker data. The function may return an
#' error message (a string with class `"error-message"`) if no visits
#' could be matched to dates or if no non-missing biomarker data is found.
#'
#' @export
clean_biomarker_data <- function(
  as_df,
  table_name,
  query
) {
  ## Patterns to replace in column names
  replace_in_colnames <- c(
    "_1_[^2]" = "_",
    "_xw" = "_crosswalk",
    "_derived" = "",
    "Abeta" = "ABeta",
    "ABeta_42" = "ABeta42"
  )

  ## Remove view_participants_adrcnum if present
  if ("view_participants_adrcnum" %in% names(as_df)) {
    as_df$view_participants_adrcnum <- NULL
  }

  if (table_name == "Local Roche CSF - Sarstedt freeze 3") {
    if (!any(grepl("pTau181", colnames(as_df)))) {
      colnames(as_df) <- gsub("pTau", "pTau181", colnames(as_df))
    }
  }

  age_cols <- grep("age", colnames(as_df), value = TRUE)

  if (length(age_cols) > 1) {
    as_df <- as_df[which(as_df[[age_cols[1]]] == as_df[[age_cols[2]]])]
    as_df[[age_cols[1]]] <- NULL
  }

  ## Fix binary variables to 0/1
  as_df[,
    names(.SD) := lapply(.SD, \(y) {
      y[y %in% c("TRUE", "true", "True", "Y")] <- 1
      y[y %in% c("FALSE", "false", "False", "N")] <- 0

      y[!y %in% c(0, 1)] <- NA

      as.numeric(y)
    }),
    .SDcols = grep(pattern = "_bin$|braak", x = names(as_df), value = T)
  ]

  ## Fix character columns that should be numeric
  as_df[,
    names(.SD) := lapply(.SD, \(y) {
      y[y %in% c("NA", "nan")] <- NA

      if (all(grepl("^\\d*\\.?\\d*$", y) | is.na(y))) {
        y <- as.numeric(y)
      }

      y
    }),
    .SDcols = is.character
  ]

  ## Fix column names. Get columns from query
  table_cols <- subset(query$query$tables, name == table_name)$columns[[
    1
  ]]$name

  ## Find prefix that we want to remove
  col_prefix <- table(gsub(
    pattern = paste0("_", table_cols, "$", collapse = "|"),
    replacement = "",
    x = names(as_df)
  ))

  col_prefix <- names(which.max(col_prefix))

  data.table::setnames(
    as_df,
    names(as_df),
    new = gsub(
      pattern = paste0(col_prefix, "_"),
      replacement = "",
      x = names(as_df)
    )
  )

  if (nrow(as_df) == 0) {
    out <- "Error: visits could not be matched to dates."

    class(out) <- "error-message"

    return(out)
  }

  for (i in seq_along(replace_in_colnames)) {
    # For i = 1, we want to match on expression, but replace different expression. Hence the extra gsub in new.
    data.table::setnames(
      as_df,
      old = grep(
        pattern = names(replace_in_colnames)[i],
        x = names(as_df),
        value = TRUE
      ),
      new = gsub(
        gsub("[^2]", "", names(replace_in_colnames)[i], fixed = T),
        replace_in_colnames[i],
        grep(
          pattern = names(replace_in_colnames)[i],
          x = names(as_df),
          value = TRUE
        )
      )
    )
  }

  for (i in colnames(as_df)) {
    if (paste0(i, "_bin") %in% colnames(as_df)) {
      colnames(as_df)[colnames(as_df) == i] <- paste0(i, "_raw")
    }
  }

  ##
  ## Table specific adjustments
  ##

  ## If Amprion - CSF
  if (table_name == "Amprion - CSF a-Synuclein") {
    data.table::setnames(
      as_df,
      c("Result", "date_of_collection"),
      c("AlphaSyn-SAA_cat", "obtained_date"),
      skip_absent = TRUE
    )
  }

  ## If we are dealing with HDX Plasma
  if (table_name == "HDX Plasma - pTau217") {
    data.table::setnames(
      as_df,
      old = "mean_conc",
      new = "pTau217_raw"
    )

    as_df$pTau217_cat <- categorize_ratio(
      as_df$pTau217_raw,
      thresholds = biomarker_thresholds[[table_name]]$pTau217$thresholds,
      out = "bin"
    )
  }

  ## If Lumipulse CSF
  if (table_name == "Lumipulse CSF - ABeta") {
    as_df <- data.table::dcast(
      as_df[!is.na(analyte)],
      ... ~ analyte,
      value.var = "concentration"
    )

    data.table::setnames(
      as_df,
      old = c("AB40", "AB42"),
      new = c("AB40_raw", "AB42_raw")
    )

    as_df$ABeta42_ABeta40_raw <- lumipulse_ABeta42_ABeta40_ratio(
      as_df$AB40,
      as_df$AB42
    )
    as_df$ABeta42_ABeta40_cat <- categorize_ratio(
      as_df$ABeta42_ABeta40_raw,
      thresholds = biomarker_thresholds[[
        table_name
      ]]$ABeta42_ABeta40$thresholds,
      out = "bin"
    )

    as_df[, c("AB40_raw", "AB42_raw")] <- NULL
  }

  ## If Lumipulse Plasma
  if (table_name == "Lumipulse Plasma - pTau217") {
    as_df <- data.table::dcast(
      as_df[!is.na(analyte)],
      ... ~ analyte,
      value.var = "concentration",
      fill = NA
    )

    data.table::setnames(
      as_df,
      old = c("AB40", "AB42", "pTau217"),
      new = c("AB40_raw", "AB42_raw", "pTau217_raw")
    )

    as_df$pTau217_ABeta42_raw <- lumipulse_pTau217_ABeta42_ratio(
      pTau217 = as_df$pTau217_raw,
      ABeta42 = as_df$AB42_raw
    )

    as_df$pTau217_ABeta42_cat <- categorize_ratio(
      as_df$pTau217_ABeta42_raw,
      thresholds = biomarker_thresholds[[
        table_name
      ]]$pTau217_ABeta42$thresholds,
      out = "bin"
    )

    as_df[, c("AB40_raw", "AB42_raw", "pTau217_raw")] <- NULL
  }

  ## If NTK MultiObs - CSF
  # if (table_name == "NTK MultiObs - CSF analytes") {
  #   as_df
  # }

  ## Check that there are at least some non-missing biomarker data. If not, return NULL
  if (
    all(is.na(
      as_df[,
        !grepl(pattern = "age_|_date", colnames(as_df)),
        with = F
      ]
    ))
  ) {
    out <- "Error: no non-missing biomarker data found."
    class(out) <- "error-message"

    return(out)
  }

  as_df
}
