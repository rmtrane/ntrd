#' Get biomarker data
#'
#' @description
#' Queries the [Panda](https://panda.medicine.wisc.edu) database to get biomarker data for
#' ADRC participants.
#'
#' @param adrc_ptid A single string. ADRC participant id for which we want to pull biomarker data.
#' @param api_key A single string. API key for panda database.
#' @param base_query_file A single string. Optional.
#'
#' @returns
#' A list of `data.table`s containing biomarker data, with names corresponding to the tables from
#' Panda that was queried.
#'
#' @keywords internal
get_biomarker_data <- function(
  adrc_ptid = "adrc00006",
  api_key,
  base_query_file = system.file(
    "json/panda_template.json",
    package = "NpsychAssessmentTool"
  )
) {
  name <- NULL # for devtools::check()

  if (!is.character(adrc_ptid) || length(adrc_ptid) != 1) {
    cli::cli_abort(
      "{.arg adrc_ptid} must be a string, but is of class {.cls {class(adrc_ptid)}}"
    )
  }

  missing_pkgs <- c("httr2", "jsonlite")[c(
    !rlang::is_installed("httr2"),
    !rlang::is_installed("jsonlite")
  )]

  if (length(missing_pkgs) > 0) {
    cli::cli_abort(
      "Please install {.pkg {missing_pkgs}} to pull data from Panda. You can use {.code install.packages(c(\"{paste0(missing_pkgs, collapse = '\", \"')}\"))} to do so."
    )
  }

  ## Patterns to replace in column names
  replace_in_colnames <- c(
    "_1_[^2]" = "_",
    # "_xw" = "",
    "_derived" = "",
    "Abeta" = "ABeta",
    "ABeta_42" = "ABeta42"
  )

  ## Read the base query file and replace the participant id
  my_query <- gsub(
    x = readLines(base_query_file),
    pattern = "adrc_ptid",
    replacement = paste0(
      "'",
      adrc_ptid,
      "'"
    )
  ) |> #head(n = 25) |> cat(sep = "\n")
    jsonlite::fromJSON()

  ## Build the request. First, set base URL
  my_request <- httr2::request(
    base_url = 'https://panda.medicine.wisc.edu/api/search/search'
  ) |>
    # Next, add authorization piece (this is where the API key is needed)
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
    ) |>
    # Finally, specify request method
    httr2::req_method("POST")

  ## We perform the request once for each table except for the Participants table.
  ## This is included in all queries.
  tables <- my_query$query$tables

  tables <- tables[
    !tables$name %in%
      c(
        "MRI Appointments",
        "PET Appointments",
        "LP Appointments",
        "Participants"
      ),
    "name"
  ]

  all_responses <- lapply(tables, \(tab) {
    cur_query <- my_query

    cur_query$query$tables <- my_query$query$tables[
      my_query$query$tables$name %in% c(tab, "Participants"),
    ]

    ## If no date is in any table, add PET Appointments, and make table join "inner"
    all_names <- unlist(lapply(cur_query$query$tables$columns, `[[`, "name"))

    if (!any(grepl("date", tolower(all_names)))) {
      if (any(grepl("^age_lp$", tolower(all_names)))) {
        extra_table <- "LP Appointments"
      } else {
        extra_table <- "PET Appointments"
      }
      cur_query$query$tables <- my_query$query$tables[
        my_query$query$tables$name %in%
          c(tab, extra_table, "Participants"),
      ]
    }

    cur_query$query$tables$join[cur_query$query$tables$name == tab] <- "inner"

    cur_req <- my_request |>
      # Add the JSON to the body of the request.
      httr2::req_body_json(
        data = cur_query
      )

    cur_resp <- try(httr2::req_perform(cur_req), TRUE)

    if (inherits(cur_resp, "try-error")) {
      return(cur_resp)
    }

    if (cur_resp$status != 200) {
      return(cur_resp$status)
    }

    httr2::resp_body_json(cur_resp)$data
  }) |>
    setNames(tables)

  purrr::imap(all_responses, \(x, idx) {
    if (is.na(x) | inherits(x, "try-error")) {
      return(x)
    }

    if (x == "[]") {
      out <- "No values found"
      class(out) <- "error-message"

      return(out)
    }

    as_df <- data.table::data.table(jsonlite::fromJSON(x))
    as_df$view_participants_adrcnum <- NULL

    if (idx == "Local Roche CSF - Sarstedt freeze 3") {
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
    table_cols <- subset(my_query$query$tables, name == idx)$columns[[1]]$name

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

    if (col_prefix == "cg_amprion_csf_alpha_synuclein") {
      data.table::setnames(
        as_df,
        c("Result", "date_of_collection"),
        c("AlphaSyn-SAA_cat", "obtained_date")
      )
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

    ## If we are dealing with the plasma biomarker, a few extra adjustments
    if (grepl(pattern = "Plasma", x = idx)) {
      data.table::setnames(
        as_df,
        old = "mean_conc",
        new = "pTau217_plasma_raw"
      )

      as_df$pTau217_plasma_cat <- findInterval(
        as_df$pTau217_plasma_raw,
        vec = c(0.4, 0.63)
      )
    }

    ## Check that there are at least some non-missing biomarker data. If not, return NULL
    if (
      all(
        is.na(as_df[,
          !grepl(pattern = "age_|_date", colnames(as_df)),
          with = F
        ])
      )
    ) {
      out <- "Error: no non-missing biomarker data found."
      class(out) <- "error-message"

      return(out)
    }

    as_df
  })
}


#' Format a table for gt
#'
#' @description
#' Takes a table as those in the entries of the list provided by `get_biomarker_data`,
#' and formats this for "pretty printing" using `gt::gt`.
#'
#' @param tab A `data.table`.
#'
#' @returns
#' A `data.table` with modified column names and values, formatted for use with `gt::gt`.
#' Returns `NULL` if the input `tab` is `NULL`.
#'
#' @keywords internal
bio_tab_for_gt <- function(
  tab,
  return = c("binary", "raw", "both")
) {
  if (inherits(tab, "try-error")) {
    return(tab)
  }

  if (is.null(tab)) {
    return(data.table::data.table(name = "No values found"))
  }

  if (inherits(tab, "error-message")) {
    return(data.table::data.table(name = unclass(tab)))
  }

  if (!inherits(tab, "data.table")) {
    cli::cli_abort(
      "{.arg tab} must be of class {.cls data.table}, but is of class {.cls {class(tab)}}"
    )
  }

  colnames(tab)[grepl(pattern = "date", colnames(tab))] <- "date"
  colnames(tab)[grepl(pattern = "age", colnames(tab))] <- "Age_raw"

  # To avoid "no visible binding for global variable" in devtools::check()
  name <- value.name <- bin <- variable <- value <- NULL

  # Remove missing dates
  tab <- tab[!is.na(date)]
  tab[, date := as.Date(date)]

  if (!"date" %in% colnames(tab)) {
    tab$date <- paste("Visit", seq_along(tab$age_raw))
  }

  if (any(grepl("_bin$", x = names(tab)))) {
    ## In rare cases, entries might be present with no raw or bin values. We remove these
    rows_include <- rowSums(
      !(tab[,
        lapply(.SD, is.na),
        .SDcols = colnames(tab)[
          grepl("_raw$|_bin$", colnames(tab)) & !grepl("Age", colnames(tab))
        ]
      ])
    ) >
      0

    tab <- data.table::melt(
      tab[rows_include],
      id.vars = c("date"),
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|bin)"
      )
    )

    if (return == "binary") {
      tab[,
        c("name", "raw", "bin") := list(
          factor(name, levels = unique(name)),
          ifelse(
            name == "Age",
            raw,
            ifelse(
              bin,
              list(
                icon = '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>',
                text = 'Positive'
              ),
              list(
                icon = '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i>',
                text = 'Negative'
              )
            )
          ),
          NULL
        )
      ]
    }

    if (return == "both") {
      tab[,
        c("name", "raw", "bin") := list(
          factor(name, levels = unique(name)),
          lapply(
            data.table::transpose(.SD),
            \(x) {
              c(
                list(raw = x[1])[!is.na(x[1])],
                list(
                  list(
                    icon = '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i>',
                    text = 'Negative'
                  ),
                  list(
                    icon = '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>',
                    text = 'Positive'
                  )
                )[[x[2] + 1]]
              )
            }
          ),
          NULL
        ),
        .SDcols = c("raw", "bin")
      ]
    }

    tab <- data.table::dcast(
      tab,
      name ~ date,
      value.var = "raw"
    )

    tab$name <- as.character(tab$name)
  }

  if ("rating_0_1_2_3" %in% names(tab)) {
    tab <- data.table::melt(
      tab,
      id.vars = c("date"),
      measure.vars = c("Age_raw", "rating_0_1_2_3")
    )

    tab[,
      c("variable", "value") := list(
        gsub(
          "rating_0_1_2_3",
          "PiB Visual Rating",
          x = gsub("_raw", "", x = variable)
        ),
        # fmt: skip
        data.table::fcase(
          variable == "Age_raw", as.list(value),
          value < 5, list(
            "0" = list(
              icon = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i>',
              text = "Clearly negative (0)"
            ),
            "1" = list(
              icon = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i>',
              text = "Clearly negative (1)"
            ),
            "2" = list(
              text = 'Ambiguous/Indeterminate'
            ),
            "3" = list(
              icon = '<i class="glyphicon glyphicon-plus-sign" style="color:red;"></i>',
              text = "Positive"
            ),
            "5" = list()
          )[pmin(as.character(value), 5, na.rm = T)]
        )
      )
    ]

    tab <- data.table::dcast(
      tab,
      variable ~ date,
      value.var = "value"
    )

    data.table::setnames(
      tab,
      "variable",
      "name"
    )
  }

  if ("braak_1" %in% colnames(tab)) {
    tab <- data.table::melt(
      tab,
      id.vars = "date"
    )

    tab[,
      c("variable", "value") := list(
        c(
          "Age",
          "Comment",
          "Stage I",
          "Stage II",
          "Stage III",
          "Stage IV",
          "Stage V",
          "Stage VI"
        )[match(
          tab$variable,
          c(
            "Age_raw",
            "comment",
            "braak_1",
            "braak_2",
            "braak_3",
            "braak_4",
            "braak_5",
            "braak_6"
          )
        )],
        # fmt: skip
        value = ifelse(
          variable %in% c("Age_raw", "comment"), 
          as.list(value),
          list(
            list(
              icon = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i>',
              text = 'Negative'
            ),
            list(
              icon = '<i class="glyphicon glyphicon-plus-sign" style="color:red;"></i>',
              text = 'Positive'
            )
          )[value + 1]
        )
      )
    ]

    tab <- data.table::dcast(
      tab,
      variable ~ date,
      value.var = "value"
    )

    data.table::setnames(
      tab,
      "variable",
      "name"
    )
  }

  if (any(grepl("AlphaSyn-SAA", names(tab)))) {
    tab <- data.table::melt(
      tab,
      id.vars = c("date"),
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|cat)"
      )
    )

    if (return == "both") {
      tab[,
        c("name", "raw", "cat") := list(
          factor(name, levels = unique(name)),
          lapply(
            data.table::transpose(.SD),
            \(x) {
              icon <- data.table::fcase(
                x[2] == "Not Detected"                  , '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i>' ,
                x[2] %in% c("Detected-1", "Detected-2") , '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>'    ,
                default = NA
              )

              c(
                list(raw = as.numeric(x[1])),
                # list(
                list(
                  icon = if (!is.na(icon)) icon,
                  text = x[2]
                  # )
                  # list(
                  #   icon = '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i>',
                  #   text = 'Not Detected'
                  # ),
                  # list(
                  #   icon = '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>',
                  #   text = 'Detected-1'
                  # ),
                  # list(
                  #   icon = '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>',
                  #   text = 'Detected-2'
                  # ),
                  # list(
                  #   text = "QND"
                  # )
                ) #[[match(x[2], c("Not Detected", "Detected-1", "Detected-2"))]]
              )
            }
          ),
          NULL
        ),
        .SDcols = c("raw", "cat")
      ]
    }

    tab <- data.table::dcast(
      tab,
      name ~ date,
      value.var = "raw"
    )

    tab$name <- as.character(tab$name)
  }

  if (any(grepl("_cat$", x = names(tab)))) {
    ## In rare cases, entries might be present with no raw or bin values.
    rows_include <- rowSums(
      !(tab[,
        lapply(.SD, is.na),
        .SDcols = colnames(tab)[
          grepl("_raw$|_bin$", colnames(tab)) & !grepl("Age", colnames(tab))
        ]
      ])
    ) >
      0

    tab <- data.table::melt(
      tab[rows_include],
      id.vars = "date",
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|cat)"
      )
    )

    if (return == "binary") {
      tab[,
        c("name", "raw", "cat") := list(
          factor(name, levels = unique(name)),
          ifelse(
            name == "Age",
            raw,
            c(
              '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i> Low',
              'Medium',
              '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i> High'
            )[cat + 1]
          ),
          NULL
        )
      ]
    }

    if (return == "both") {
      tab[,
        c("name", "raw", "cat") := list(
          factor(name, levels = unique(name)),
          lapply(
            data.table::transpose(.SD),
            \(x) {
              c(
                list(raw = x[1]),
                list(
                  list(
                    icon = '<i class="glyphicon glyphicon-minus-sign" style="color: green;"></i>',
                    text = 'Low'
                  ),
                  list(
                    text = 'Medium'
                  ),
                  list(
                    icon = '<i class="glyphicon glyphicon-plus-sign" style="color: red;"></i>',
                    text = 'High'
                  )
                )[[x[2] + 1]]
              )
            }
          ),
          NULL
        ),
        .SDcols = c("raw", "cat")
      ]
    }

    tab <- data.table::dcast(
      tab,
      name ~ date,
      value.var = "raw"
    )

    tab$name <- as.character(tab$name)
  }

  tab
}

#' Transform a table to a gt object
#'
#' @description
#' A short description...
#'
#' @param tab_for_gt A `data.table` or a `list` of `data.table`s.
#'
#' @returns
#' A `gt::gt` table. If `tab_for_gt` is a list, the names of the list are used as
#' grouping variable for table.
#'
#' @keywords internal
bio_tab_to_gt <- function(tab_for_gt) {
  if (is.list(tab_for_gt) & !inherits(tab_for_gt, "data.table")) {
    tab_for_gt <- tab_for_gt[!unlist(lapply(tab_for_gt, is.null))]

    tab_for_gt <- lapply(tab_for_gt, \(x) {
      if (inherits(x, "try-error")) {
        x <- data.table::data.table(
          name = x[1]
        )
      }

      x
    })

    if (!all(sapply(tab_for_gt, data.table::is.data.table))) {
      non_dts <- tab_for_gt[!sapply(tab_for_gt, data.table::is.data.table)]

      cli::cli_abort(
        "When {.arg tab_for_gt} is of class {.cls list}, all elements must be of class {.cls data.table}, but {.val {names(non_dts)}} {?is/are} of class {.cls {unlist(sapply(non_dts, class))}}"
      )
    }

    tab_for_gt <- data.table::rbindlist(
      tab_for_gt,
      fill = TRUE,
      idcol = "table"
    )

    visit_dates <- setdiff(names(tab_for_gt), c("name", "table"))

    tab_for_gt <- tab_for_gt[, c("table", "name", sort(visit_dates)), with = F]
  }

  group <- if ("table" %in% names(tab_for_gt)) "table"

  if (!is.null(group)) {
    age_rows <- tab_for_gt[tab_for_gt$name == "Age"]

    age_rows$table <- NULL
    age_rows <- unique(age_rows[,
      c("table", names(.SD)) := c(
        "",
        lapply(.SD, \(x) unique(na.omit(x)))
      )
    ])

    tab_for_gt <- data.table::rbindlist(
      list(age_rows, tab_for_gt[tab_for_gt$name != "Age"]),
      fill = TRUE
    )
  }

  gt::gt(
    id = "biomarker-table",
    tab_for_gt,
    rowname_col = "name",
    groupname_col = group
  ) |>
    gt::tab_stub_indent(
      rows = 1:nrow(tab_for_gt),
      indent = if (!is.null(group)) 4
    ) |>
    gt::fmt(
      fns = \(x) sapply(x, gt::html),
      rows = tab_for_gt$name != "Age"
    ) |>
    gt::fmt(
      fns = \(x) paste(floor(as.numeric(x)), "years"),
      rows = tab_for_gt$name == "Age",
      columns = -"name"
    ) |>
    gt::cols_align(align = "left") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "right",
          style = "hidden"
        ),
        gt::cell_text(align = "left")
      ),
      locations = gt::cells_stub()
    ) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::sub_missing()
}


get_all_values <- function(
  api_key = getOption("panda_api_key"),
  base_query_file = system.file(
    "json/panda_template.json",
    package = "NpsychAssessmentTool"
  )
) {
  # To avoid notes in R CMD check
  name <- NULL

  base_query <- readLines(base_query_file) |>
    jsonlite::fromJSON()

  base_request <- httr2::request(
    base_url = 'https://panda.medicine.wisc.edu/api/search/search'
  ) |>
    # Next, add authorization piece (this is where the API key is needed)
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
    ) |>
    # Finally, specify request method
    httr2::req_method("POST")

  all_tables <- base_query$query$tables

  ## Tables to work with (remove participants, appointments, visual rating)
  all_tables_names <- all_tables$name[
    # !stringr::str_detect(
    !grepl(
      "Participants|Appointments|Visual Rating",
      x = all_tables$name
    )
  ]

  # For each table, get everything

  all_values <- lapply(
    setNames(all_tables_names, all_tables_names),
    \(cur_table_name) {
      cur_query <- base_query

      cur_query$query$tables <- subset(
        all_tables,
        name == cur_table_name
      )

      cur_query$query$tables$columns[[1]]$constraints <- NULL

      cur_query$query$tables$columns[[1]] <-
        cur_query$query$tables$columns[[1]][
          !grepl(
            "enumber|date|age",
            x = cur_query$query$tables$columns[[1]]$name
          ),
        ]

      cur_req <- base_request |>
        httr2::req_body_json(
          data = cur_query
        )

      cur_resp <- try(httr2::req_perform(cur_req), TRUE)

      if (inherits(cur_resp, "try-error")) {
        return(cur_resp)
      }

      if (cur_resp$status != 200) {
        return(cur_resp$status)
      }

      httr2::resp_body_json(cur_resp)$data

      # jsonlite::fromJSON(
      #   httr2::resp_body_json(cur_resp)$data
      # )
    }
  )

  replace_in_colnames <- c(
    "_1_[^2]" = "_",
    # "_xw" = "",
    "_derived" = "",
    "Abeta" = "ABeta",
    "ABeta_42" = "ABeta42"
  )

  purrr::imap(all_values, \(x, idx) {
    if (is.na(x) | inherits(x, "try-error")) {
      return(x)
    }

    if (x == "[]" | inherits(x, "error-message")) {
      return(NULL)
    }

    as_df <- data.table::data.table(jsonlite::fromJSON(x))

    if (idx == "Local Roche CSF - Sarstedt freeze 3") {
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
    table_cols <- subset(base_query$query$tables, name == idx)$columns[[1]]$name
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

    ## If we are dealing with the plasma biomarker, a few extra adjustments
    if (grepl(pattern = "Plasma", x = idx)) {
      data.table::setnames(
        as_df,
        old = "mean_conc",
        new = "pTau217_plasma_raw"
      )

      as_df$pTau217_plasma_cat <- findInterval(
        as_df$pTau217_plasma_raw,
        vec = c(0.4, 0.63)
      )
    }
    as_df
  })
}


get_all_cuts <- function(all_values) {
  # To avoid notes in R CMD check
  name <- NULL
  value.name <- NULL
  bin <- NULL
  min_obs <- NULL
  max_obs <- NULL

  lapply(all_values, \(x) {
    if (any(grep("_cat$", colnames(x)))) {
      colnames(x) <- gsub("_cat", "_bin", colnames(x))
    }

    if (!any(grepl("_(raw|bin)", colnames(x)))) {
      return()
    }

    data.table::melt(
      x[complete.cases(x)],
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|bin)"
      )
    )[
      !is.na(bin),
      list(
        # cut = ifelse(bin == 0, max(raw, na.rm = T), min(raw, na.rm = T))
        # .N,
        min_obs = min(raw, na.rm = T),
        max_obs = max(raw, na.rm = T) #,
        # range = list(range(raw, na.rm = T))
      ),
      by = c("name", "bin")
    ][
      order(name, min_obs)
    ][,
      list(
        bin = bin / max(bin),
        min_obs = ((min_obs + data.table::shift(max_obs)) / 2) |>
          data.table::nafill(type = "const", fill = 0),
        max_obs = ((data.table::shift(min_obs, n = -1) + max_obs) / 2) |>
          data.table::nafill(type = "const", fill = Inf)
      ),
      by = "name"
    ][,
      list(
        name = name,
        bin = bin,
        # fmt: skip
        color = data.table::fcase(
          bin == 0, "rgba(0, 100, 0, alpha)",
          bin == 1, "rgba(139, 0, 0, alpha)",
          bin == 0.5, "rgba(216, 216, 216, 0.5)"
        ),
        min_obs = min_obs,
        max_obs = max_obs
      )
    ]
  })
}


get_all_densities <- function(all_values) {
  purrr::map(all_values, \(x) {
    if (is.null(x) | nrow(x) == 0 | inherits(x, "try-error")) {
      return(NULL)
    }

    x <- x[, grepl(pattern = "_raw$", names(x)), with = F]

    if (ncol(x) == 0) {
      return(NULL)
    }

    dens <- purrr::imap(x, \(y, idy) {
      if (!grepl(pattern = "_raw$", idy)) {
        return(NULL)
      }

      y <- na.omit(y)

      density(
        y,
        from = 0,
        kernel = "gaussian",
        bw = "SJ-ste",
        adjust = ifelse(length(y) > 500, 1, 1.5),
        na.rm = T
      )
    })

    dens[!unlist(lapply(dens, is.null))]
  }) |>
    purrr::discard(purrr::is_null)
}


check_connection <- function(
  api_key = getOption("panda_api_key"),
  timeout = 5
) {
  # Try to make a simple request to the URL
  tryCatch(
    {
      response <- httr2::request(
        base_url = 'https://panda.medicine.wisc.edu/api/search/search'
      ) |>
        httr2::req_timeout(timeout) |>
        # Next, add authorization piece (this is where the API key is needed)
        httr2::req_headers(
          Authorization = paste("Bearer", api_key)
        ) |>
        # Finally, specify request method
        httr2::req_method("POST") |>
        httr2::req_body_json(
          jsonlite::fromJSON(
            '{"query":{"tables":[{"name":"Participants","id":679,"class":"cg_tn","join":"left","columns":[{"name":"Adrc Number","id":690,"class":"cg_tn_cn","show":true,"constraints":[{"operator":"=","values":["\'adrc00001\'"]}]}]}],"offset":0,"limit":-1,"format":"csv","row_format":"participant_centric","name":null,"save":null}}'
          )
        ) |>
        httr2::req_perform()

      # If we get here, connection succeeded
      return(list(
        connected = TRUE,
        status = httr2::resp_status(response),
        message = "Connection successful"
      ))
    },
    error = function(e) {
      # Connection failed
      return(list(
        connected = FALSE,
        status = NA,
        message = paste("Connection failed:", e$message)
      ))
    }
  )
}
