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
#' @param summary_dat A list as returned by `assessment_summary_data()`.
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
    for (fn_text in names(summary_dat$footnotes)) {
      out <- out |>
        gt::tab_footnote(
          footnote = fn_text,
          locations = gt::cells_body(
            columns = "std",
            rows = summary_dat$footnotes[[fn_text]]
          ),
          placement = "right"
        )
    }
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
  include_caption = TRUE
) {
  # To avoid R CMD check NOTE:
  value.name <- name <- for_percentile <- group <- NULL

  if (!data.table::is.data.table(dat)) {
    cli::cli_abort("{.arg dat} must be of class {.cls data.table}.")
  }

  dat <- data.table::copy(dat)

  if (!is.character(id) || !id %in% colnames(dat)) {
    cli::cli_abort(
      "{.arg id} must be a character string specifying a column in {.arg dat}."
    )
  }

  data.table::setnames(dat, old = id, new = "id")

  if (nrow(dat) != 1) {
    cli::cli_abort(
      "Data provided must have exactly one row, but has {nrow(dat)}."
    )
  }

  if (is.null(fill_values)) {
    fill_values <- setNames(
      calc_fill_colors(length(descriptions)),
      nm = names(descriptions)
    )
  }

  if (!identical(names(fill_values), names(descriptions))) {
    cli::cli_abort(
      "The names of {.arg fill_values} and {.arg descriptions} must be the same."
    )
  }

  ## Get npsych_scores columns: map column name -> class name
  npsych_class_map <- unlist(dat[,
    lapply(.SD, \(x) S7::S7_class(x)@name),
    .SDcols = ntrs::is_npsych_scores
  ])

  ## Get domains from npsych_scores columns: map column name -> domain
  domain_map <- unlist(
    lapply(
      dat[, names(npsych_class_map), with = F],
      \(x) x@domain
    )
  )

  ## Get std_npsych_scores columns: map column name -> scores_subclass
  std_subclass_map <- unlist(dat[,
    lapply(.SD, \(x) x@scores_subclass),
    .SDcols = \(x) S7::S7_inherits(x, ntrs::std_npsych_scores)
  ])

  ## Get all methods
  std_col_methods <- lapply(
    dat[, names(std_subclass_map), with = F],
    \(x) {
      x@method
    }
  )
  ## Identify t-scores
  t_scores <- unname(std_subclass_map[unlist(std_col_methods) == "tscores"])

  ## Create labels to be used.
  # Match var_labels formals to npsych_scores class names
  for_labels <- lapply(
    setNames(formalArgs(var_labels), formalArgs(var_labels)),
    \(x) {
      if (x %in% npsych_class_map) {
        return(
          dat[[names(which(npsych_class_map == x))]]
        )
      }

      NA
    }
  )

  ## Don't want a data.frame, but named vector. Hence, unlist
  cur_labels <- unlist(do.call(var_labels, for_labels))

  ## Select columns by class name (npsych_scores) and scores_subclass
  ## (std_npsych_scores), but remove those without domain.
  desired_scores <- setdiff(
    unique(c(names(cur_labels), unname(std_subclass_map))),
    names(which(is.na(domain_map)))
  )
  raw_to_keep <- names(npsych_class_map)[npsych_class_map %in% desired_scores]
  std_to_keep <- names(std_subclass_map)[std_subclass_map %in% desired_scores]
  for_main_table <- dat[, c("id", raw_to_keep, std_to_keep), with = FALSE]

  ## Rename raw columns using class name, std columns using scores_subclass
  data.table::setnames(
    for_main_table,
    old = raw_to_keep,
    new = paste0("raw_", npsych_class_map[raw_to_keep])
  )
  data.table::setnames(
    for_main_table,
    old = std_to_keep,
    new = paste0("std_", std_subclass_map[std_to_keep])
  )

  ## Rename domain_map to use npsych_scores subclass instead
  domain_map <- setNames(
    domain_map[names(npsych_class_map)],
    unname(npsych_class_map)
  )

  ## Replace negative error codes with NA for raw cols, and convert to numeric.
  raw_cols <- paste0("raw_", npsych_class_map[raw_to_keep])

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

  ## For std cols, just convert to numeric.
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

  # Get all npsych_scores labels, keyed by class name
  all_labels <- unlist(dat[,
    setNames(lapply(.SD, \(x) x@label), npsych_class_map[names(.SD)]),
    .SDcols = ntrs::is_npsych_scores
  ])

  # Overwrite if special label necessary
  all_labels[names(cur_labels)] <- cur_labels

  ## Add additional columns.
  for_main_table[,
    c(
      "group",
      "labels",
      "for_percentile",
      "raw_suffix",
      "units",
      "is_error"
    ) := list(
      if (length(name) == 0) {
        return(character())
      } else {
        factor(
          domain_map[name],
          levels = unique(c(nacc_groups, na.omit(domain_map)))
        )
      },
      factor(
        all_labels[name],
        levels = all_labels[order(match(
          names(all_labels),
          names(cur_labels),
          nomatch = 99
        ))]
      ),
      as.numeric(ifelse(name %in% t_scores, (std - 50) / 10, std)),
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

        base::ifelse(
          x %in% c("TRAILA", "TRAILB", "OTRAILA", "OTRAILB"),
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
      stats::pnorm(as.numeric(for_percentile)) * 100, # for some reason, for_main_table$for_percentile is character if nrow(for_main_table) == 0.
      names(descriptions)[
        findInterval(
          stats::pnorm(as.numeric(for_percentile)),
          vec = descriptions,
          rightmost.closed = T
        ) +
          1
      ]
    )
  ]

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

  ## Build footnotes from @description of std_npsych_scores columns
  std_descriptions <- dat[, lapply(.SD, \(x) x@description), .SDcols = \(x) {
    S7::S7_inherits(x, ntrs::std_npsych_scores)
  }]
  # Map std column names to score names using scores_subclass
  names(std_descriptions) <- std_subclass_map[names(std_descriptions)]

  unique_descs <- unique(unlist(std_descriptions))
  out$footnotes <- lapply(setNames(unique_descs, unique_descs), \(desc) {
    scores_with_desc <- names(std_descriptions)[
      unlist(std_descriptions) == desc
    ]
    for_main_table$name %in% scores_with_desc & !for_main_table$is_error
  })

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
