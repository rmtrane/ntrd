#' Table With Previous Diagnoses
#'
#' @param dat Data to use. Should only refer to a single participant
#' @param table_font_size Table font size as a percent. Default: 100
#'
#' @export
prev_diagnoses_table <- function(dat, table_font_size = 100) {
  # due to NSE notes in R CMD check:
  var <- val <- for_tab <- contribution <- disease <- etiology <- nacc_name <- NACCID <- NACCUDSD <- VISITDATE <- CDRGLOB <- CDRSUM <- MOCATOTS <- NACCMMSE <- contribution_character <- etiologies <- variable <- NULL

  if (!data.table::is.data.table(dat)) {
    cli::cli_abort("The {.var dat} object must be a {.cls data.table}.")
  }

  diagnosis_table <- data.table::copy(dat[,
    .SD,
    .SDcol = unlist(data.table::patterns(
      paste(
        "_etiology$",
        "_contribution$",
        "^NACCID$",
        "^VISITDATE$",
        "^NACCUDSD$",
        "^raw_MOCATOTS",
        "^raw_NACCMMSE",
        "^CDR",
        "^FAS",
        sep = "|"
      ),
      cols = colnames(dat)
    ))
  ])

  diagnosis_table[,
    names(.SD) := lapply(.SD, as.character),
    .SDcol = unlist(data.table::patterns(
      "_etiology$|_contribution$",
      cols = colnames(dat)
    ))
  ]

  diagnosis_table <- data.table::melt(
    diagnosis_table,
    measure.vars = unlist(data.table::patterns(
      "_etiology$|_contribution",
      cols = colnames(diagnosis_table)
    ))
  )[,
    `:=`(
      nacc_name = gsub(
        pattern = "_etiology$|_contribution$",
        replacement = "",
        x = variable
      ),
      variable = gsub(
        pattern = "^[A-Z0-9]+_",
        replacement = "",
        x = variable
      )
    )
  ]

  # diagnosis_table$nacc_name <- gsub(
  #   pattern = "_etiology$|_contribution$",
  #   replacement = "",
  #   x = diagnosis_table$variable
  # )

  # diagnosis_table$variable <- gsub(
  #   pattern = "^[A-Z0-9]+_",
  #   replacement = "",
  #   x = diagnosis_table$variable
  # )

  diagnosis_table <- data.table::dcast(
    diagnosis_table,
    ... ~ variable,
    value.var = "value"
  )[
    order(NACCID, VISITDATE)
  ][,
    `:=`(
      contribution_character = c(
        "1" = "Primary",
        "2" = "Contributing",
        "3" = "Non-contributing"
      )[contribution],
      disease = setNames(
        diag_contr_pairs$disease,
        diag_contr_pairs$presump_etio_diag
      )[nacc_name],
      cdr = paste0(CDRGLOB, " (", CDRSUM, ")")
    )
  ][,
    disease := ifelse(
      grepl(pattern = "^Other", x = disease),
      paste0(disease, ": ", etiology),
      disease
    )
  ][,
    names(.SD) := lapply(.SD, ntrs::remove_error_codes),
    .SDcols = data.table::patterns("raw_[MOCATOTS|NACCMMSE]")
  ]

  data.table::setnames(
    diagnosis_table,
    old = colnames(diagnosis_table),
    new = gsub("^raw_", "", colnames(diagnosis_table))
  )

  diagnosis_table <- diagnosis_table[,
    list(
      etiologies = list(disease)
    ),
    by = intersect(
      c(
        "NACCID",
        "VISITDATE",
        "NACCUDSD",
        "MOCATOTS",
        "NACCMMSE",
        "cdr",
        "FAS",
        "contribution_character"
      ),
      colnames(diagnosis_table)
    ) #,
    # .SDcols = "disease"
  ][
    is.na(NACCUDSD) | (NACCUDSD == 1),
    etiologies := list(NA)
  ]

  if (!"MOCATOTS" %in% colnames(diagnosis_table)) {
    diagnosis_table$MOCATOTS <- NA_real_
  }

  if (!"NACCMMSE" %in% colnames(diagnosis_table)) {
    diagnosis_table$NACCMMSE <- NA_real_
  }

  diagnosis_table[,
    MOCATOTS := data.table::fcoalesce(MOCATOTS, NACCMMSE)
  ]

  which_mmse <- diagnosis_table[
    !is.na(diagnosis_table$NACCMMSE) & diagnosis_table$NACCMMSE > 0
  ]$VISITDATE

  diagnosis_table[, NACCMMSE := NULL]

  for_out <- diagnosis_table[
    NACCUDSD %in% 1:4 | !is.na(contribution_character)
  ][,
    NACCUDSD := names(ntrs::rdd$NACCUDSD$codes)[match(
      as.numeric(NACCUDSD),
      ntrs::rdd$NACCUDSD$codes
    )]
  ]

  if (nrow(for_out) == 0) {
    cli::cli_alert_info("No diagnoses to display.")

    out <- gt::gt(data = data.frame(x = "No previous diagnoses found.")) |>
      gt::cols_label(x = "") |>
      gt::tab_style(
        style = gt::cell_borders(style = "hidden"),
        locations = list(gt::cells_column_labels(), gt::cells_body())
      )

    return(out)
  }

  for_out <- for_out[,
    list(
      "for_tab" = list(
        data.table::setDT(data.table::data.table(
          "var" = factor(
            c(
              "Global Cognition",
              "CDR Global (SOB)",
              "FAS",
              "Diagnosis",
              .SD$contribution_character
            ),
            levels = c(
              "Global Cognition",
              "CDR Global (SOB)",
              "FAS",
              "Diagnosis",
              "Primary",
              "Contributing",
              "Non-contributing"
            )
          ),
          "val" = c(
            unique(.SD$MOCATOTS),
            unique(.SD$cdr),
            unique(.SD$FAS),
            # names(ntrs::rdd$NACCUDSD$codes)[match(
            #   as.numeric(unique(.SD$NACCUDSD)),
            #   ntrs::rdd$NACCUDSD$codes
            # )],
            unique(.SD$NACCUDSD),
            unname(.SD$etiologies)
          )
        ))[
          ## We're joining on a data.table with all levels of var.
          data.table::CJ(
            "var" = factor(
              c(
                "Global Cognition",
                "CDR Global (SOB)",
                "FAS",
                "Diagnosis",
                "Primary",
                "Contributing",
                "Non-contributing"
              ),
              levels = c(
                "Global Cognition",
                "CDR Global (SOB)",
                "FAS",
                "Diagnosis",
                "Primary",
                "Contributing",
                "Non-contributing"
              )
            ),
            unique = T
          ),
          on = list(var)
        ][,
          list(
            "var" = var,
            "val" = lapply(val, \(x) ifelse(is.null(x), NA, x))
          )
        ]
      )
    ),
    by = c(
      "NACCID",
      "VISITDATE"
    )
  ]

  for_out <- for_out[,
    list(
      "var" = unlist(lapply(for_tab, `[[`, "var"), recursive = F),
      "val" = unlist(lapply(for_tab, `[[`, "val"), recursive = F)
    ),
    by = "VISITDATE"
  ]

  for_out <- for_out[order(VISITDATE)]

  for_out <- data.table::dcast(
    for_out,
    var ~ VISITDATE,
    value.var = "val"
  )[order(var)]

  for_out[,
    c("var", names(.SD)) := c(
      list(factor(
        gsub(pattern = "\\ ", "&nbsp;", var),
        levels = gsub(pattern = "\\ ", "&nbsp;", levels(var))
      )),
      lapply(
        .SD,
        \(x) {
          lapply(x, \(y) {
            if (length(y) == 0 | all(is.na(y))) {
              return(NA)
            }

            paste(
              gsub(pattern = "\\ ", "&nbsp;", y),
              collapse = '<p style="margin:7px;"></p>'
            )
          })
        }
      )
    ),
    .SDcols = is.list #setdiff("VISITDATE", colnames(for_out))
  ]

  out_gt <- gt::gt(
    for_out,
    id = "diagnosis-table",
    rowname_col = "var"
  ) |>
    gt::fmt_markdown() |>
    gt::sub_missing() |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          style = "italic",
          decorate = "underline"
        )
      ),
      locations = gt::cells_body(
        columns = which(colnames(for_out) %in% which_mmse),
        rows = 1
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          v_align = "top",
          weight = "bold"
        ),
        gt::css(position = "sticky", left = 0)
      ),
      locations = list(
        gt::cells_stub(),
        gt::cells_stubhead()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "right",
        weight = gt::px(2),
        color = "#00000033" # rgb(0,0,0, 0.2)
      ),
      locations = gt::cells_stubhead()
    ) |>
    gt::tab_style(
      style = gt::cell_text(v_align = "top"),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::css(
        "white-space" = "nowrap"
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::cols_align(
      align = "left"
    ) |>
    gt::tab_options(
      column_labels.font.weight = "bold",
      table.font.size = gt::pct(table_font_size)
    ) |>
    gt::opt_css(
      css = "
      #diagnosis-table .gt_table {
        border-collapse: separate;
        border-spacing: 0;
      }

      #diagnosis-table .gt_footnote {
        position: sticky;
        left: 0;
      }
    "
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top",
        weight = "2px",
        color = "#D3D3D3"
      ),
      locations = gt::cells_footnotes()
    ) |>
    gt::tab_footnote(
      footnote = gt::html(paste(
        "MoCA",
        if (length(which_mmse) > 0) "or <u><i>MMSE</u></i>"
      )),
      locations = gt::cells_stub(
        rows = grepl("Cognition", .data$var)
      )
    )

  out_html <- gt::as_raw_html(out_gt)

  ## Make footnote sticky. Need to move the colspan attribute. Also, add line.
  ## First, turn html string into character vector with one line per entry
  out_html <- gsub(
    pattern = "^ +|+$ ",
    replacement = "",
    x = strsplit(as.character(out_html), split = "\n", fixed = T)[[1]]
  )

  ## Next, remove any instance of colspan="1"
  out_html <- gsub(pattern = "colspan=\"1\"", replacement = "", x = out_html)

  ## Now, for all lines with colspan="[0-9]+", move this outside of tag within
  ## its own <td> tag, or (if dealing with sourcenote) remove entirely. If
  ## moved to its own tag, style with border top.
  out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)] <- unlist(
    lapply(
      out_html[grepl(pattern = "colspan=\"[0-9]+\"", out_html)],
      \(x) {
        ## If this is the source note, simply remove colspan part of the line
        if (grepl(pattern = "gt_sourcenote", x)) {
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

  shiny::HTML(out_html, .noWS = "outside")
}
