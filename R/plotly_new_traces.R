#' New Traces to add to Plot
#'
#' @description
#' A short description...
#'
#' @param new_dat A data frame.
#' @param visibility A named list, where the names correspond to variables in
#' `new_dat` and the values are one of `"TRUE"`, `"FALSE"`, or `"legendonly"`.
#' @param legend_names Legend names. Will be returned with added legends if any
#' new ones were created. This only happens if a pair of crosswalk variables
#' are both present.
#' @param vars_colors Named vector. Entries give colors to use for markers and liens
#' for variable corresponding to the name of the entry.
#'
#' @returns
#' A list containing new traces (a list of lines and markers), legend names, the new
#' x range, and the new y range. If there are no new traces to add (i.e. no standardized
#' scores in the data), the return is empty.
#'
#' @export
plotly_new_traces <- function(
  new_dat,
  visibility = visibility_defaults,
  legend_names,
  vars_colors
) {
  new_dat <- data.table::copy(new_dat[,
    lapply(.SD, \(x) if (any(!is.na(x))) x)
  ])

  ## Check if any standardized values
  any_left <- sum(grepl(pattern = "^std_", x = colnames(new_dat)))

  if (is.null(any_left) || any_left == 0) {
    return()
  }

  ## Adjust T-scores
  t_score_cols <- names(which(unlist(lapply(
    new_dat,
    \(x) attributes(x)$method == "tscores"
  ))))

  for (col in t_score_cols) {
    new_dat[[col]] <- (new_dat[[col]] - 50) / 10
  }

  ## Due to NSE notes in R CMD check:
  name <- value.name <- NULL

  new_dat[, names(.SD) := lapply(.SD, as.numeric), .SDcols = \(x) {
    S7::S7_inherits(x, ntrs::std_npsych_scores)
  }]

  new_dat <- data.table::melt(
    new_dat,
    id.vars = "VISITDATE",
    measure.vars = data.table::measure(
      ## special keyword value.name is similar to .value in pivot_longer
      value.name,
      name,
      pattern = "(raw|std)_(.*)"
    )
  )

  new_dat$label <- unname(nacc_var_labels[new_dat$name])

  new_dat$name[new_dat$name %in% names(crosswalk_translations)] <-
    crosswalk_translations[new_dat$name[
      new_dat$name %in% names(crosswalk_translations)
    ]]

  new_dat <- new_dat[!is.na(new_dat$std), ]

  new_traces <- lapply(unique(new_dat$name), \(nm) {
    tmp_dat <- new_dat[new_dat$name == nm]
    tmp_dat <- tmp_dat[order(tmp_dat$VISITDATE)]

    vis <- visibility[[nm]]

    # if (paste(nm, "visibility", sep = "_") %in% names(input)) {
    #   vis <- input[[paste(nm, "visibility", sep = "_")]]
    # } else {
    #   vis <- visibility_defaults[[nm]]
    # }

    ## If multiple labels, i.e. if dealing with crosswalk scores,
    ## combine to one label.
    if (length(unique(tmp_dat$label)) > 1) {
      legend_name <- paste(unique(tmp_dat$label), collapse = "/")

      if (
        grepl(
          pattern = "\\ \\-\\ Total|\\ \\- Span\\ Length",
          x = legend_name
        )
      ) {
        if (grepl(pattern = "\\ \\-\\ Total", x = legend_name)) {
          legend_name <- paste0(
            gsub(
              pattern = "\\ \\-\\ Total",
              replacement = "",
              x = legend_name
            ),
            " - Total"
          )
        }

        if (grepl(pattern = "\\ \\-\\ Span Length", x = legend_name)) {
          legend_name <- paste0(
            gsub(
              pattern = "\\ \\-\\ Span\\ Length",
              replacement = "",
              x = legend_name
            ),
            " - Span Length"
          )
        }
      }
    } else {
      legend_name <- unique(tmp_dat$label)
    }

    legend_names <-
      unique(c(legend_names, legend_name))

    new_lines <- list(
      type = "scatter",
      mode = "lines",
      x = as.list(tmp_dat$VISITDATE),
      y = as.list(tmp_dat$std),
      text = I(tmp_dat$label),
      line = list(
        color = vars_colors[[nm]],
        line = list(color = vars_colors[[nm]])
      ),
      name = legend_name,
      visible = vis,
      legendgroup = legend_name,
      customdata = nm,
      hovertemplate = paste0(
        '<span style="font-weight:bold;">%{text}</span><br>',
        "Visit Date: %{x}<br>",
        "Scores:<br>",
        " - raw: ",
        tmp_dat$raw,
        "<br>",
        " - std: %{y:.2f}",
        "<extra><br>",
        "</extra>"
      )
    )

    if (length(unique(tmp_dat$label)) > 1) {
      symbs <- as.list(c("diamond", "cross")[as.numeric(factor(
        tmp_dat$label
      ))])
    } else {
      symbs <- "o"
    }

    new_markers <- list(
      type = "scatter",
      mode = "markers",
      x = as.list(tmp_dat$VISITDATE),
      y = as.list(tmp_dat$std),
      text = I(tmp_dat$label),
      marker = list(
        color = vars_colors[[nm]],
        symbol = symbs,
        size = 8
      ),
      name = legend_name,
      visible = vis,
      showlegend = F,
      legendgroup = legend_name,
      customdata = nm,
      hovertemplate = paste0(
        '<span style="font-weight:bold">%{text}</span><br>',
        "Visit Date: %{x}<br>",
        "Scores:<br>",
        " - raw: ",
        tmp_dat$raw,
        "<br>",
        " - std: %{y:.2f}",
        "<extra><br>",
        "</extra>"
      )
    )

    list(new_lines, new_markers)
  })

  return(
    list(
      new_traces = unlist(new_traces, recursive = FALSE),
      legend_names = legend_names,
      new_x_range = date_range(new_dat$VISITDATE),
      new_y_range = range(as.numeric(new_dat$std), na.rm = T)
    )
  )
}
