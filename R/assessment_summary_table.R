#' Create NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' This function create a summary table of measures from the NACC T-Cog Neuropsychological
#' Assessment.
#'
#' @param dat Data to use. Must have exactly one row (data from one participant and one visit).
#' @param id Column containing ID.
#' @param descriptions A named vector giving cutoffs to use for creating descriptions.
#' @param fill_values (optional) A named vector of same length (with same names)
#'   as `descriptions` with hex color values to use. If `NULL`, no colorcoding used.
#'   By default, evenly spread out colors from red through yellow to green are used.
#' @param methods (optional) either list of named entries specifying which model was used
#'   for standardizing cognitive scores, or the character string "infer". For the
#'   latter, methods are infered from the `dat` object using
#'   `NpsychBatteryNorms::methods_from_std_data`. If specified, footnotes are added
#'   to indicate the methods used.
#' @param bar_height In pixels. Height of the percentile bars. Default: 16
#' @param include_caption Logical; should a caption be included above the table
#'   with age and IQCODEs at current visit?
#'
#' @returns An object of class `gt::gt_tbl`
#'
#' @export
assessment_summary_table <- function(
  summary_dat,
  bar_height = 16
) {
  # if (missingArg(fill_values)) fill_values <- NULL

  # stopifnot(
  #   "Exactly one of 'dat' or 'summary_dat' must be provided" = missingArg(dat) +
  #     missingArg(summary_dat) ==
  #     1
  # )

  # if (
  #   unique(dat$NACCID) == "adrc000178" & unique(dat$VISITDATE == "2020-10-12")
  # ) {
  #   browser()
  # }

  # if (missingArg(summary_dat)) {
  # summary_dat <- assessment_summary_data(
  #   dat = dat,
  #   id = id,
  #   descriptions = descriptions,
  #   fill_values = fill_values,
  #   methods = methods,
  #   include_caption = include_caption
  # )
  # }

  for_main_table <- summary_dat$for_main_table

  fill_values <- summary_dat$fill_values

  out <- for_main_table |>
    gt::gt(
      rowname_col = "labels",
      groupname_col = "group",
      id = "assessment-summary-table"
    ) |>
    gt::cols_hide(columns = c("name", "is_error")) |>
    ## Make parts of the table bold
    gt::tab_options(
      table.font.names = "Arial",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    ) |>
    gt::fmt(
      columns = "raw_suffix",
      fns = gt::md
    ) |>
    gt::fmt_number(
      columns = "std"
    ) |>
    gt::tab_style(
      style = gt::css(
        "white-space" = "nowrap"
      ),
      locations = gt::cells_stub()
    ) |>
    gt::tab_stub_indent(
      rows = T,
      indent = 5
    ) |>
    my_gt_plt_bar_pct(
      "Percentile",
      scaled = T,
      labels = T,
      height = bar_height
    ) |>
    gt::sub_missing() |>
    gt::cols_align(
      align = "left",
      columns = c("labels", "raw_suffix", "units")
    ) |>
    gt::cols_align(
      align = "right",
      columns = "raw"
    ) |>
    gt::cols_align(
      align = "center",
      columns = "Percentile"
    ) |>
    ## Make raw, raw_suffix, and units look pretty
    gt::tab_style(
      style = "padding-right:0px; padding-left:0px;",
      locations = gt::cells_column_labels(columns = c("raw", "raw_suffix"))
    ) |>
    gt::tab_style(
      style = "padding-left:0px;",
      locations = gt::cells_column_labels("units")
    ) |>
    gt::tab_style(
      style = "padding-right:0px;",
      locations = gt::cells_body(
        columns = "raw"
      )
    ) |>
    gt::tab_style(
      style = paste0(
        "padding-left:0px; padding-right:0px; color:",
        grDevices::rgb(0, 0, 0, 0.3),
        ";"
      ),
      # glue::glue("padding-left:0px; padding-right:0px; color: {grDevices::rgb(0, 0, 0, 0.3)};"),
      locations = list(
        gt::cells_body(
          columns = "raw_suffix"
        )
      )
    )

  if (any(for_main_table$is_error)) {
    out <- out |>
      gt::tab_style(
        style = gt::css(
          "padding-left" = gt::px(0),
          "padding-right" = gt::px(0),
          "color" = grDevices::rgb(0.8, 0, 0, 0.75),
          "text-decoration-line" = "underline",
          "text-decoration-style" = "dotted"
        ),
        locations = list(
          gt::cells_body(
            columns = "raw",
            rows = for_main_table$is_error
          )
        )
      ) |>
      gt::tab_style(
        style = gt::css(
          "padding-left" = gt::px(0),
          "padding-right" = gt::px(0),
          color = "black"
        ),
        locations = list(
          gt::cells_body(
            columns = "raw_suffix",
            rows = for_main_table$is_error
          )
        )
      ) |>
      gt::tab_footnote(
        footnote = "Raw scores highlighted in red are not valid scores, but error codes. Hover for details.",
        placement = "left"
      )
  }

  out <- out |>
    gt::tab_style(
      style = "padding-left:0px;",
      locations = gt::cells_body(
        columns = "units"
      )
    ) |>
    gt::fmt(
      columns = "units",
      fns = gt::md
    ) |>
    gt::fmt(
      columns = "raw",
      fns = \(x) lapply(x, gt::html)
    ) |>
    gt::cols_label(
      "raw" = "R",
      "raw_suffix" = "aw",
      "units" = "",
      "std" = gt::html(as.character(bslib::tooltip(
        "Std.",
        "Results of standardizing raw scores"
      )))
      # gtExtras::with_tooltip("Standardized", "Results of standardizing raw scores")
    ) |>
    gt::tab_style(
      style = gt::css(
        "text-decoration-line" = "underline",
        "text-decoration-style" = "dashed"
      ),
      locations = gt::cells_column_labels("std")
    ) |>
    gt::tab_spanner(
      columns = c("raw", "raw_suffix", "units", "std"),
      label = "Scores"
    )

  if ("footnotes" %in% names(summary_dat)) {
    out <- out |>
      gt::tab_footnote(
        footnote = "Z-scores based on regression models adjusting for age, sex, race, and years of education.",
        locations = gt::cells_body(
          columns = "std",
          rows = summary_dat$footnotes$regression_rows_w_updated
        ),
        placement = "right"
      ) |>
      gt::tab_footnote(
        footnote = "Z-scores based on regression models adjusting for age, sex, and years of education.",
        locations = gt::cells_body(
          columns = "std",
          rows = summary_dat$footnotes$regression_rows_w_nacc
        ),
        placement = "right"
      ) |>
      gt::tab_footnote(
        footnote = "Z-scores based on regression models adjusting for age, sex, years of education, and delay interval.",
        locations = gt::cells_body(
          columns = "std",
          rows = summary_dat$footnotes$regression_rows_w_delay
        ),
        placement = "right"
      ) |>
      gt::tab_footnote(
        footnote = "T-scores adjusting for age, sex, and years of education.",
        locations = gt::cells_body(
          columns = "std",
          rows = summary_dat$footnotes$t_score_rows
        ),
        placement = "right"
      ) |>
      gt::tab_footnote(
        footnote = "Z-scores obtained from standardizing using group specific means and standard deviations. Groups based on age, sex, and years of education.",
        locations = gt::cells_body(
          columns = "std",
          rows = summary_dat$footnotes$norm_rows
        ),
        placement = "right"
      )
  }

  ## Add colors to "Description" column
  lapply(names(fill_values), \(desc) {
    out <<- out |>
      gt::tab_style(
        style = gt::cell_fill(color = fill_values[[desc]]),
        locations = gt::cells_body(
          columns = "Description",
          rows = which(out$`_data`$Description == desc)
        )
      )
  })

  if ("cap" %in% names(summary_dat)) {
    cap <- summary_dat$cap |>
      gt::gt(
        id = "summary-table-caption"
      ) |>
      gt::fmt_markdown() |>
      gt::tab_options(
        column_labels.hidden = TRUE,
        table.margin.left = gt::px(0),
        table.margin.right = gt::px(0),
        data_row.padding = gt::px(3)
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(v_align = "top"),
          gt::cell_borders(style = "hidden")
        ),
        locations = list(
          gt::cells_body()
        )
      )

    out <- out |>
      gt::tab_header(
        title = gt::as_raw_html(cap)
      ) |>
      gt::tab_style(
        style = list(gt::cell_text(align = "left")),
        locations = list(gt::cells_title())
      ) |>
      gt::tab_style(
        style = list(gt::cell_borders(sides = "top", style = "hidden")),
        locations = list(gt::cells_title())
      )
  }

  out |>
    gt::opt_footnote_marks(marks = "standard")
}

#' @rdname assessment_summary_table
#'
#' @keywords internal
assessment_summary_data <- function(
  dat,
  id = "NACCID",
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values = NULL,
  methods = "infer",
  include_caption = TRUE
) {
  stopifnot(
    "'id' must be a character string specifying a column in the data frame 'dat'" = is.character(
      id
    ) &&
      id %in% colnames(dat)
  )

  stopifnot(
    "'dat' must be of class 'data.table'" = data.table::is.data.table(dat)
  )

  if (is.null(fill_values)) {
    fill_values <- setNames(
      calc_fill_colors(length(descriptions)),
      nm = names(descriptions)
    )
  }

  base::stopifnot(
    "The names of fill_values and descriptions must be the same" = names(
      fill_values
    ) ==
      names(descriptions)
  )

  t_scores <- NULL

  # if (length(methods) == 1 && methods == "infer") {
  #   methods <- NpsychBatteryNorms::methods_from_std_data(std_data = dat)
  # }

  base::stopifnot(
    "'methods' must either be given or included as attributes from 'dat'" = exists(
      "methods"
    )
  )

  t_scores <- names(methods)[sapply(methods, `[[`, "method") == "tscores"]

  base::stopifnot("Data provided must have exactly one row" = nrow(dat) == 1)

  ## Rename id column for easier coding.
  colnames(dat)[colnames(dat) == id] <- "id"

  ## Create labels to be used.
  # First, get subclass names
  subclasses <- unlist(dat[,
    lapply(.SD, \(x) S7::S7_class(x)@name),
    .SDcols = is_npsych_scores
  ])

  # Create named list with arguments for var_labels()
  for_labels <- lapply(
    setNames(formalArgs(var_labels), formalArgs(var_labels)),
    \(x) {
      # if (x %in% colnames(dat)) {
      #   return(dat[[x]])
      # }

      # if (paste("raw", x, sep = "_") %in% colnames(dat)) {
      #   return(dat[[paste("raw", x, sep = "_")]])
      # }

      if (x %in% subclasses) {
        return(
          #  ntrs::remove_error_codes(
          dat[[names(which(subclasses == x))]]
          # )
        )
      }

      NA
    }
  )

  ## Don't want a data.frame, but named vector. Hence, unlist
  cur_labels <- unlist(do.call(var_labels, for_labels))

  ## Some raw_ columns might not have special labels, so make sure we do not
  ## miss any npsych_scores.
  std_cols <- grep(pattern = "^std_", x = colnames(dat), value = TRUE)

  ## Get rid of columns not needed
  for_main_table <- dat[,
    grep(
      pattern = paste(
        unique(c("id", names(cur_labels), gsub("^std_", "", std_cols))),
        collapse = "|"
      ),
      x = colnames(dat)
    ),
    with = F
  ]

  ## Rename columns that we do not standardize to still include prefix raw_
  data.table::setnames(
    for_main_table,
    old = names(cur_labels),
    new = paste("raw", names(cur_labels), sep = "_"),
    skip_absent = TRUE
  )

  # colnames(for_main_table)[
  #   colnames(for_main_table) %in% names(cur_labels)
  # ] <- paste0(
  #   "raw_",
  #   colnames(for_main_table)[colnames(for_main_table) %in% names(cur_labels)]
  # )

  ## Replace negative error codes with NA. Get raw cols, then replace
  raw_cols <- grep(
    pattern = "^raw_",
    x = colnames(for_main_table[, .SD, .SDcols = ntrs::is_npsych_scores]),
    value = T
  )

  for_main_table[,
    names(.SD) := lapply(.SD, \(x) {
      neg_err_codes <- x@codes[x@codes < 0]
      if (as.numeric(x) %in% neg_err_codes) {
        return(NA_real_)
      }

      as.double(x)
    }),
    .SDcols = raw_cols
  ]

  for_main_table[,
    names(.SD) := lapply(.SD, as.double),
    .SDcols = data.table::patterns("^std_")
  ]

  ## Create long data with one row per variable. To do so, we go through a data set with two rows per variable, one for raw and one for std.
  for_main_table <- data.table::melt(
    for_main_table,
    measure.vars = data.table::measure(
      value.name,
      name,
      sep = "_"
    ),
    variable.factor = FALSE,
    value.factor = FALSE
  )[!is.na(raw)]

  # Get all npsych_scores labels
  all_labels <- unlist(dat[,
    setNames(lapply(.SD, \(x) x@label), gsub("^raw_", "", names(.SD))),
    .SDcols = is_npsych_scores
  ])

  # Overwrite if special label necessary
  all_labels[names(cur_labels)] <- cur_labels

  ## Add additional columns. Avoid tidyverse verbs for devtools::check (? not sure why this throws a warning here... EDIT: fixed by importing .data from rlang. Could rewrite back to use tidyverse syntax...)
  for_main_table[,
    c(
      "group",
      "labels",
      "for_percentile",
      "raw_suffix",
      "units",
      "is_error"
    ) := list(
      unlist(sapply(name, \(x) {
        tmp <- ntrs::get_npsych_scores(x)()@domain
        if (length(tmp) != 0) {
          return(tmp)
        }

        NA
      })),
      factor(
        all_labels[name],
        levels = all_labels[order(match(
          names(all_labels),
          names(cur_labels),
          nomatch = 99
        ))]
      ),
      ifelse(name %in% t_scores, (std - 50) / 10, std),
      purrr::map2_chr(name, raw, \(x, y) {
        if (is.na(y)) {
          return("")
        }

        if (x %in% ntrs::list_npsych_scores()) {
          x <- ntrs::get_npsych_scores(x)()
        } else {
          return("")
        }

        if (is.null(x@range)) {
          return("")
        }

        if (y %in% x@codes) {
          return("")
        }

        paste0("/", x@range[2])
      }),
      purrr::map2_chr(name, raw, \(x, y) {
        if (is.na(y)) {
          return("")
        }

        if (x %in% ntrs::list_npsych_scores()) {
          x <- ntrs::get_npsych_scores(x)()
        } else {
          return("")
        }

        if (y %in% x@codes) {
          return("")
        }

        base::ifelse(
          S7::S7_class(x)@name %in% c("TRAILA", "TRAILB", "OTRAILA", "OTRAILB"),
          "&nbspsec",
          ""
        )
      }),
      purrr::map2_lgl(name, raw, \(x, y) {
        if (is.na(y)) {
          return(FALSE)
        }

        if (x %in% ntrs::list_npsych_scores()) {
          x <- ntrs::get_npsych_scores(x)()
        } else {
          return(FALSE)
        }

        if (y %in% x@codes & y > x@range[2]) {
          return(TRUE)
        }

        FALSE
      })
    )
  ][,
    c("Percentile", "Description") := list(
      pnorm(for_percentile) * 100,
      names(descriptions)[
        findInterval(
          pnorm(for_percentile),
          vec = descriptions,
          rightmost.closed = T
        ) +
          1
      ]
    )
  ]

  # for_main_table$group <- with(
  #   for_main_table,
  #   nacc_var_groups[name]
  # )

  # for_main_table$labels <- with(
  #   for_main_table,
  #   factor(
  #     cur_labels[name],
  #     levels = unname(cur_labels)
  #   )
  # )

  # for_main_table <- subset(for_main_table, !is.na(raw))

  # for_main_table$for_percentile <- with(
  #   for_main_table,
  #   ifelse(name %in% t_scores, (std - 50) / 10, std)
  # )

  # for_main_table$Percentile <- with(
  #   for_main_table,
  #   pnorm(for_percentile) * 100
  # )

  # for_main_table$Description <- names(descriptions)[
  #   findInterval(
  #     for_main_table$Percentile / 100,
  #     vec = descriptions,
  #     rightmost.closed = T
  #   ) +
  #     1
  # ]

  # for_main_table$raw_suffix <- with(
  #   for_main_table,
  #   purrr::map2_chr(name, raw, \(x, y) {
  #     x_fun <- match.fun(x)

  #     if (is.na(y)) {
  #       return("")
  #     }

  #     cur_range <- purrr::pluck(rdd, x)$range

  #     if (is.null(cur_range)) {
  #       return("")
  #     }

  #     if (y %in% purrr::pluck(rdd, x)$codes) {
  #       return("")
  #     }

  #     paste0("/", cur_range[2])
  #   })
  # )

  # for_main_table$units <- with(
  #   for_main_table,
  #   purrr::map2_chr(name, raw, \(x, y) {
  #     if (is.na(y)) {
  #       return("")
  #     }

  #     if (y %in% purrr::pluck(rdd, x)$codes) {
  #       return("")
  #     }

  #     base::ifelse(
  #       x %in% c("TRAILA", "TRAILB", "OTRAILA", "OTRAILB"),
  #       "&nbspsec",
  #       ""
  #     )
  #   })
  # )

  for_main_table <- for_main_table[
    order(labels),
    c(
      "group",
      "labels",
      "name",
      "raw",
      "raw_suffix",
      "units",
      "std",
      "Percentile",
      "Description",
      "is_error"
    ),
    with = F
  ][
    !is.na(group)
  ]

  for_main_table[,
    raw := purrr::map2_chr(name, raw, \(x, y) {
      if (x == "CDRGLOB") {
        return(sprintf("%.1f", y))
      }

      if (x %in% ntrs::list_npsych_scores()) {
        x <- ntrs::get_npsych_scores(x)()
      } else {
        return("")
      }

      codes <- x@codes

      if (y %in% codes) {
        return(as.character(bslib::tooltip(y, names(codes)[which(codes == y)])))
      }

      sprintf("%.0f", y)
    })
  ]

  # for_main_table$is_error <- with(
  #   for_main_table,
  #   purrr::map2_lgl(name, raw, \(x, y) {
  #     codes <- rdd[[x]]$codes

  #     if (y %in% codes & y > 10) {
  #       return(TRUE)
  #     }

  #     FALSE
  #   })
  # )

  # for_main_table$raw <- with(
  #   for_main_table,
  #   purrr::map2_chr(name, raw, \(x, y) {
  #     # purrr::map2(name, raw, \(x, y) {
  #     if (x == "CDRGLOB") {
  #       return(sprintf("%.1f", y))
  #     }

  #     codes <- rdd[[x]]$codes

  #     if (y %in% codes) {
  #       return(as.character(bslib::tooltip(y, names(codes)[which(codes == y)])))
  #     }

  #     sprintf("%.0f", y)
  #   })
  # )

  out <- list(
    for_main_table = for_main_table,
    fill_values = fill_values
  )

  if (!missingArg(methods)) {
    regression_rows_w_nacc <- for_main_table$name %in%
      names(which(unlist(lapply(methods, \(x) {
        x[["method"]] == "regression" && grepl("^nacc", x[["version"]])
      })))) &
      !for_main_table$is_error &
      for_main_table$name != "MEMUNITS"

    regression_rows_w_updated <- for_main_table$name %in%
      names(which(unlist(lapply(methods, \(x) {
        x[["method"]] == "regression" && !grepl("^nacc", x[["version"]])
      })))) &
      !for_main_table$is_error &
      for_main_table$name != "MEMUNITS"

    regression_rows_w_delay <- for_main_table$name %in%
      names(methods)[lapply(methods, `[[`, "method") == "regression"] &
      !for_main_table$is_error &
      for_main_table$name == "MEMUNITS"

    t_score_rows <- for_main_table$name %in%
      names(methods)[lapply(methods, `[[`, "method") == "tscores"] &
      !for_main_table$is_error

    norm_rows <- for_main_table$name %in%
      names(methods)[lapply(methods, `[[`, "method") == "norms"] &
      !for_main_table$is_error

    out$footnotes <- list(
      regression_rows_w_nacc = regression_rows_w_nacc,
      regression_rows_w_updated = regression_rows_w_updated,
      regression_rows_w_delay = regression_rows_w_delay,
      t_score_rows = t_score_rows,
      norm_rows = norm_rows
    )
  }

  if (include_caption) {
    for_cap <- dat[,
      c("NACCAGE", "FAS", "IQCODESELF", "IQCODEINFORM")[
        c("NACCAGE", "FAS", "IQCODESELF", "IQCODEINFORM") %in% colnames(dat)
      ],
      with = F
    ]

    for_cap$NACCAGE <- floor(for_cap$NACCAGE)

    for_cap$IQCODESELF <- ifelse(
      is.null(for_cap$IQCODESELF) || is.na(for_cap$IQCODESELF),
      "&mdash;",
      sprintf("%.2f", for_cap$IQCODESELF)
    )

    for_cap$IQCODEINFORM <- ifelse(
      is.null(for_cap$IQCODEINFORM) || is.na(for_cap$IQCODEINFORM),
      "&mdash;",
      sprintf("%.2f", for_cap$IQCODEINFORM)
    )

    for_cap$FAS <- ifelse(
      is.null(for_cap$FAS) || is.na(for_cap$FAS),
      "&mdash;",
      for_cap$FAS
    )

    out$cap <- data.table::data.table(
      X1 = "**Age**:",
      X2 = paste0(
        "<u>&nbsp;&nbsp;",
        floor(for_cap$NACCAGE),
        "&nbsp;&nbsp;</u>&nbsp;."
      ),
      X3 = "**FAS**:",
      X4 = paste0("<u>&nbsp;&nbsp;", for_cap$FAS, "&nbsp;&nbsp;</u>&nbsp;."),
      X5 = "**IQCODE** (self/informant):",
      X6 = paste0(
        "<u>&nbsp;&nbsp;",
        for_cap$IQCODESELF,
        "&nbsp;/&nbsp;",
        for_cap$IQCODEINFORM,
        "&nbsp;&nbsp;</u>&nbsp;."
      )
    )
  }

  out
}
