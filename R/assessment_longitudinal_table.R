#' Assessment Longitudinal Table
#'
#' @description
#' Create a table of raw scores across multiple visits colored by the corresponding standardized scores.
#'
#' @param dat A data.table object.
#' @param id A character string specifying a column in the data frame `dat`. Default is `"NACCID"`.
#' @param date A character string specifying a column in the data frame `dat`. Default is `"VISITDATE"`.
#' @param descriptions A named numeric vector. Default has names `"Impaired"`, `"Borderline"`, `"Low Average"`, `"Average"`, `"High Average"`, `"Superior"`, and `"Very Superior"` with corresponding values `0.03`, `0.10`, `0.26`, `0.76`, `0.92`, `0.97`, and `1`.
#' @param fill_values Optional.
#' @param methods Either a list containing methods used for standardization (each a character vector with named entried `method` and `version`), or `"infer"` (default). If `"infer"`, then methods are pulled from attributes of standardized columns.
#' @param table_font_size A numeric value passed to `gt::tab_options(table.font.size = gt::pct(table_font_size))`. Defaults to 100.
#' @param table_id Optional. ID given to the table. If not provided, random string assigned.
#' @param show_all_visits A boolean. If `TRUE` (default), all visits present in the data are shown in the table. If `FALSE`, only visits with at least one standardized score are presented.
#' @param stubhead_label Optional. Passed to `gt::tab_stubhead(label = stubhead_label)`.
#'
#' @returns
#' A `shiny::HTML` table created from a `gt::gt` object.
#'
#' @export
assessment_longitudinal_table <- function(
  dat,
  id = "NACCID",
  date = "VISITDATE",
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values,
  methods = list(), # "infer",
  table_font_size = 100,
  table_id = NULL,
  show_all_visits = TRUE,
  stubhead_label = NULL
) {
  stopifnot(
    "'id' must be a character string specifying a column in the data frame 'dat'" = is.character(
      id
    ) &&
      id %in% colnames(dat)
  )
  stopifnot(
    "'date' must be a character string specifying a column in the data frame 'dat'" = is.character(
      date
    ) &&
      date %in% colnames(dat)
  )

  stopifnot(
    "'dat' must be a data.table object" = data.table::is.data.table(dat)
  )

  colnames(dat)[colnames(dat) == id] <- "id"
  colnames(dat)[colnames(dat) == date] <- "date"

  stopifnot(
    "'date' must be a column containing dates" = lubridate::is.Date(dat$date)
  )
  stopifnot(
    "'date' should not contain duplicates" = length(unique(dat$date)) ==
      nrow(dat)
  )

  if (is.null(table_id)) {
    table_id <- paste(sample(letters, size = 10, replace = T), collapse = "")
  }

  if (missingArg(fill_values) || is.null(fill_values)) {
    fill_values <- setNames(
      calc_fill_colors(length(descriptions)),
      nm = names(descriptions)
    )
  }

  stopifnot(
    "Data provided must pertain to only one individual" = length(unique(
      dat$id
    )) ==
      1
  )

  npsych_scores_cols <- colnames(dat[0, .SD, .SDcols = is_npsych_scores])
  npsych_scores_cols <- gsub("^raw_", "", npsych_scores_cols)

  for_table_std <- dat[
    ## Remove rows without dates
    !is.na(date),
    ## Only return columns that are non-empty...
    setNames(
      ## New names
      nm = gsub("^std_", "", names(.SD)),
      ## New columns
      object = lapply(.SD, \(x) if (any(!is.na(x))) x)
    ),
    ## ... among the columns "date" and standardized cols that match npsych_scores cols
    .SDcols = c(
      "date",
      intersect(colnames(dat), paste("std", npsych_scores_cols, sep = "_"))
    )
  ][
    ## Order by date
    order(date)
  ]

  if (ncol(for_table_std) == 1) {
    return(shiny::h3("No scores found."))
  }

  for_table <- dat[
    ## Remove rows without dates
    !is.na(date),
    ## Only return columns that are non-empty...
    setNames(
      ## Remove prefix raw_ when present
      nm = gsub("^raw_", "", names(.SD)),
      ## New columns
      object = purrr::imap(.SD, \(x, idx) {
        if (idx == "date") {
          return(x)
        }

        x <- ntrs::remove_error_codes(x)

        if (any(!is.na(x))) x
      })
    ),
    ## ... among the columns "date" and standardized cols that match nacc_var_labels
    .SDcols = c(
      "date",
      intersect(colnames(dat), paste("raw", names(for_table_std), sep = "_"))
    )
  ][
    ## Order by date
    order(date)
  ]

  ## Get methods for variables left
  # if (length(methods) == 1 && methods == "infer") {
  #   methods <- NpsychBatteryNorms::methods_from_std_data(
  #     std_data = for_table_std
  #   )
  # } else {

  ## Get methods for variables left
  vars_present <- colnames(for_table_std)[grepl(
    pattern = paste0("^", names(methods), "$", collapse = "|"), # nacc_var_labels
    x = colnames(for_table_std)
  )]

  methods <- methods[vars_present]

  # }

  if (!is.list(methods)) {
    cli::cli_abort(
      "{.arg methods} must either be given or included as attributes from {.arg dat}."
    )
  }

  ## Get names of scores standardized using t-scores. Needed for adjusting before
  ## adding color codes to table
  t_scores <- names(methods)[sapply(methods, `[[`, "method") == "tscores"]

  ## Combine crosswalk pairs into one column
  crosswalk_pairs <- list(
    c("MOCATOTS", "NACCMMSE"),
    c("DIGFORCT", "DIGIF"),
    c("DIGFORSL", "DIGIFLEN"),
    c("DIGBACCT", "DIGIB"),
    c("DIGBACLS", "DIGIBLEN"),
    c("MINTTOTS", "BOSTON"),
    c("CRAFTURS", "LOGIMEM"),
    c("CRAFTDRE", "MEMUNITS")
  )

  # for_table_orig <- for_table

  # invisible({
  #   lapply(crosswalk_pairs, \(x) {
  for (x in crosswalk_pairs) {
    if (all(x %in% colnames(for_table))) {
      for_table[[paste(x, collapse = "--")]] <- ifelse(
        !is.na(for_table[[x[1]]]),
        sprintf(fmt = "%.2f", for_table[[x[1]]]),
        ifelse(
          !is.na(for_table[[x[2]]]),
          paste0(
            "<u><i>",
            sprintf(fmt = "%.2f", for_table[[x[2]]]),
            "</i></u>"
          ),
          NA
        )
      )

      for_table <- for_table[, -which(colnames(for_table) %in% x), with = F]

      for_table_std[[paste(x, collapse = "--")]] <- ifelse(
        !is.na(for_table_std[[x[1]]]),
        for_table_std[[x[1]]],
        ifelse(
          !is.na(for_table_std[[x[2]]]),
          for_table_std[[x[2]]],
          NA
        )
      )

      for_table_std <- for_table_std[,
        -which(colnames(for_table_std) %in% x),
        with = F
      ]
    }
  }

  ## Transpose
  for_table <- data.table::transpose(
    for_table,
    make.names = "date",
    keep.names = "name"
  )

  for_table_std <- data.table::transpose(
    for_table_std,
    make.names = "date",
    keep.names = "name"
  )

  data.table::setnames(
    for_table_std,
    old = setdiff(colnames(for_table_std), "name"),
    new = paste(setdiff(colnames(for_table_std), "name"), "std", sep = "_")
  )

  for_table <- for_table[
    for_table_std,
    on = "name"
  ]

  if (!show_all_visits) {
    empty_cols <- colSums(!is.na(for_table)) == 0

    if (any(empty_cols)) {
      for_table <- for_table[, !empty_cols, with = F]
      # for_table_std <- for_table_std[, !empty_cols, with = F]
    }
  }

  ## Add labels
  for_table$labels <- unlist(lapply(
    for_table$name,
    \(x) {
      # if (x %in% names(nacc_var_labels)) {
      #   return(nacc_var_labels[x])
      # }
      if (!grepl("--", x)) {
        return(S7::prop(match.fun(x)(), "label"))
      }

      # to_combine <- nacc_var_labels[unlist(strsplit(x, split = "--"))]

      to_combine <- unlist(lapply(unlist(strsplit(x, split = "--")), \(x) {
        S7::prop(match.fun(x)(), "label")
      }))

      to_combine[2] <- gsub(
        pattern = " Span (Forward|Backward) - (Span Length|Total)",
        replacement = "",
        x = to_combine[2]
      )

      to_combine[2] <- paste0(" <u><i>", to_combine[2], "</i></u>")

      paste(to_combine, collapse = " /")
    }
  ))

  # for_table_std$labels <- unlist(lapply(
  #   for_table_std$name,
  #   \(x) {
  #     if (x %in% names(nacc_var_labels)) {
  #       return(nacc_var_labels[x])
  #     }

  #     to_combine <- nacc_var_labels[unlist(strsplit(x, split = "--"))]
  #     to_combine[2] <- gsub(
  #       pattern = " Span (Forward|Backward) - (Span Length|Total)",
  #       replacement = "",
  #       x = to_combine[2]
  #     )

  #     to_combine[2] <- paste0(" <u><i>", to_combine[2], "</i></u>")

  #     paste(to_combine, collapse = " /")
  #   }
  # ))

  ## Remove (--legacy) from name column
  for_table$name <- gsub(
    pattern = "--(.+)",
    replacement = "",
    x = for_table$name
  )
  # for_table_std$name <- gsub(
  #   pattern = "--(.+)",
  #   replacement = "",
  #   x = for_table_std$name
  # )

  ## Add column indicating groups
  for_table$group <- # unname(nacc_var_groups[for_table$name])
    purrr::map_chr(for_table$name, \(x) S7::prop(match.fun(x)(), "domain"))

  # for_table_std$group <- unname(nacc_var_groups[for_table_std$name])

  all_grp_names <- unique(c(nacc_groups), unique(for_table$group))
  all_var_names <- unique(c(names(nacc_var_labels), for_table$name))

  ## Make sure both tables are in right order. Remove "--(legacy)" from names involving
  ## two scores
  for_table <- for_table[
    order(
      # match(for_table$name, names(nacc_var_labels)),
      match(group, all_grp_names),
      # match(for_table$name, names(nacc_var_labels))
      match(name, all_var_names)
    )
  ]

  ## Create gt
  out <- gt::gt(
    for_table,
    id = table_id,
    rowname_col = "labels",
    groupname_col = "group"
  ) |>
    # gt::row_group_order(
    #   groups = c(
    #     nacc_groups[nacc_groups %in% for_table$group],
    #     setdiff(for_table$group, nacc_groups)
    #   )
    # ) |>
    gt::cols_hide(
      columns = c(
        "name",
        dplyr::ends_with("_std")
      )
    ) |>
    gt::cols_align("right", columns = -c("group", "labels")) |>
    gt::tab_style(
      style = gt::css(
        "white-space" = "nowrap",
        "padding-right" = "10px",
        "padding-left" = "10px"
      ),
      locations = gt::cells_column_labels()
    ) |>
    ## Make parts of the table bold
    gt::tab_options(
      table.font.names = "Arial",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      data_row.padding = gt::px(2),
      row_group.padding = gt::px(4),
      table.font.size = gt::pct(table_font_size)
    ) |>
    ## Some formatting
    gt::fmt(
      columns = -"name", #  -c("name", "group"),
      fns = gt::md
    ) |>
    gt::tab_stub_indent(
      rows = T,
      indent = 4
    ) |>
    gt::sub_missing()

  if (!is.null(stubhead_label)) {
    out <- gt::tab_stubhead(
      out,
      label = stubhead_label
    )
  }

  z_scores_from_percentiles <- qnorm(c(0, descriptions))

  for (cur_col in colnames(out$`_data`)[grepl(
    "^\\d{4}-\\d{2}-\\d{2}$",
    x = colnames(out$`_data`)
  )]) {
    cur_vals <- # out$`_data`[[cur_col]]
      for_table[[paste(cur_col, "std", sep = "_")]]

    if (any(grepl(pattern = "<u><i>", cur_vals))) {
      matches <- regexpr(pattern = "[0-9\\.]+", text = cur_vals)
      cur_vals <- substr(
        x = cur_vals,
        start = matches,
        stop = matches + attr(matches, "match.length") - 1
      )
    }

    cur_vals <- ## Need to transform T-scores to N(0,1) from N(50, 10)
      (as.numeric(cur_vals) - 50 * (out$`_data`$name %in% t_scores)) /
      (1 + 9 * (out$`_data`$name %in% t_scores))

    for (i in seq_along(fill_values)) {
      out <- gt::tab_style(
        out,
        style = gt::cell_fill(fill_values[[i]]),
        locations = gt::cells_body(
          columns = {{ cur_col }},
          rows = z_scores_from_percentiles[[i]] <= cur_vals &
            cur_vals < z_scores_from_percentiles[[i + 1]]
        )
      )
    }
  }

  if (is.list(methods) && any(names(methods) %in% out$`_data`$name)) {
    methods_used <- lapply(methods, \(x) data.table::setDT(as.list(x))) |>
      data.table::rbindlist(fill = TRUE, idcol = "variable")

    methods_used$var_labels <- with(
      out[["_data"]],
      unname(setNames(labels, name)[methods_used$variable])
    )

    # methods_used$var_labels <- nacc_var_labels[methods_used$variable]

    method <- NULL # due to NSE notes in R CMD check
    methods_used <- methods_used[,
      list(
        out = paste0(
          unique(method),
          " (",
          unique(version),
          "):<br>&nbsp;&nbsp;- ",
          paste0(var_labels, collapse = "<br>&nbsp;&nbsp;- ")
        )
      ),
      by = c("method", "version")
    ]

    methods_used <- gsub(
      pattern = " \\(NA\\)",
      replacement = "",
      x = methods_used$out
    ) |>
      paste0(collapse = "<br><br>")

    out <- out |>
      gt::tab_source_note(
        # footnote = bslib::tooltip(
        source_note = shiny::HTML(as.character(bslib::tooltip(
          shiny::HTML(
            paste0(
              "<span style=\"text-decoraction-line: underline; text-decoration-style: dotted;\">",
              # as.character(bsicons::bs_icon("info-circle", title = "Standardization Methods")),
              # Below is equivalent to the above line, but we avoid the dependency on bsicons
              '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-info-circle " style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-label="Standardization Methods" role="img">
                <title>Standardization Methods</title>
                <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"></path>
                <path d="m8.93 6.588-2.29.287-.082.38.45.083c.294.07.352.176.288.469l-.738 3.468c-.194.897.105 1.319.808 1.319.545 0 1.178-.252 1.465-.598l.088-.416c-.2.176-.492.246-.686.246-.275 0-.375-.193-.304-.533L8.93 6.588zM9 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0z">
                </path>
              </svg>',
              " Standardization Methods",
              "</span>"
            )
          ),
          shiny::HTML(methods_used),
          options = list(customClass = "my-tooltip")
        )))
      )
  }

  ## Fix rownames and sourcenotes when scrolling horizontally
  out <- out |>
    gt::tab_style(
      style = list(
        gt::css(
          position = "sticky",
          "white-space" = "nowrap",
          "padding-right" = "10px",
          left = 0
        )
      ),
      locations = list(
        gt::cells_stub(),
        gt::cells_stubhead()
      )
    ) |>
    gt::opt_css(
      # glue::glue("
      css = paste0(
        "#",
        table_id,
        " .gt_table {
            border-collapse: separate;
            border-spacing: 0;
          }",
        "#",
        table_id,
        " .gt_group_heading {
            position: sticky;
            left: 0;
            border-bottom-style: hidden;
            white-space: nowrap;
          }",
        "#",
        table_id,
        " .gt_sourcenote {
            position: sticky;
            left: 0;
            white-space: nowrap;
            border-top-style: solid;
            border-top-width: 2px;
            border-top-color: #D3D3D3;
            text-decoration-line: underline;
            text-decoration-style: dotted;
          }"
      )
    )

  out_html <- gt::as_raw_html(out, inline_css = T)

  ## Make row group headings sticky. Need to move the colspan attribute.
  ## First, turn html string into character vector with one line per entry
  out_html <- gsub(
    pattern = "^ +|+$ ",
    replacement = "",
    x = strsplit(as.character(out_html), split = "\n", fixed = T)[[1]]
  )

  ## Next, remove any instance of colspan="1"
  out_html <- gsub(pattern = "colspan=\"1\"", replacement = "", x = out_html)

  colspan_gt_sourcenote <- ""

  ## Now, for all lines with colspan="[0-9]+", move this outside of tag within
  ## its own <td> tag, or (if dealing with sourcenote) remove entirely. If
  ## moved to its own tag, style with border top.
  out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)] <- unlist(
    lapply(
      out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)],
      \(x) {
        ## If this is the source note...
        if (grepl(pattern = "gt_sourcenote", x)) {
          ## get and save colspan part outside of lapply
          colspan_match <- gregexpr(pattern = "colspan=\"[0-9]+\"", x)[[1]]
          colspan_gt_sourcenote <<- substr(
            x = x,
            start = colspan_match,
            stop = colspan_match + attr(colspan_match, "match.length") - 1
          )

          return(gsub(pattern = "colspan=\"[0-9]+\"", replacement = T, x))
        }

        ## Extract colspan part
        colspan_match <- gregexpr(pattern = "colspan=\"[0-9]+\"", x)[[1]]
        colspan_expr <- substr(
          x = x,
          start = colspan_match,
          stop = colspan_match + attr(colspan_match, "match.length") - 1
        )

        ## Remove from original line, add after.
        paste0(
          gsub(pattern = colspan_expr, replacement = "", x),
          paste(
            "<td",
            colspan_expr,
            "style=\"border-top-style: solid; border-top-color: #D3D3D3; border-top-width: 2px;\"></td>"
          )
        )
      }
    )
  )

  ## Finally, add an extra <td> with colspan in gt_sourcenote
  sourcenote_start <- which(grepl("class=\"gt_sourcenote\"", out_html))
  sourcenote_end <- which(grepl("</tr>", out_html))[
    which(grepl("</tr>", out_html)) > sourcenote_start
  ]

  out_html[sourcenote_end] <- paste0(
    "<td ",
    colspan_gt_sourcenote,
    " style=\"border-top-style: solid; border-top-color: #D3D3D3; border-top-width: 2px;\"></td>",
    out_html[sourcenote_end]
  )

  ## Done!
  shiny::HTML(out_html, .noWS = "outside")
}


if (FALSE) {
  bslib::page(
    assessment_longitudinal_table(
      dat = nacc_data_prep[NACCID == "NACC000162"],
      methods = "infer"
    )
  )
}
