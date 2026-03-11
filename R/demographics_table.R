#' Summary Table with Demographic Information
#'
#'
#' @param dat data.table object
#'
#' @examples
#' demographics_table(demo_data[1, ])
#'
#' @export

demographics_table <- function(
  dat
) {
  if (!data.table::is.data.table(dat)) {
    cli::cli_abort(
      c(
        "The {.var dat} argument must be a {.cls data.table} object.",
        "i" = "Instead, it is a {.obj_type_friendly {dat}}."
      )
    )
  }

  if (length(unique(dat$NACCID)) != 1) {
    cli::cli_abort(
      "There should only be one study ID in data, not {length(unique(dat$NACCID))}."
    )
  }

  for (cur_var in c("SEX", "RACE", "HANDED")) {
    dat[[cur_var]] <- unlist(lapply(dat[[cur_var]], \(x) {
      if (is.numeric(x)) {
        names(ntrs::rdd[[cur_var]]$codes)[match(
          x,
          ntrs::rdd[[cur_var]]$codes
        )]
      } else {
        x
      }
      # if (is.numeric(x)) NpsychBatteryNorms::values_to_labels(x, cur_var) else x
    }))
  }

  # dat$SEX <- unlist(lapply(
  #   dat$SEX,
  #   \(x) {
  #     if (is.numeric(x)) NpsychBatteryNorms::values_to_labels(x, "SEX") else x
  #   }
  # ))

  # dat$RACE <- unlist(lapply(
  #   dat$RACE,
  #   \(x) {
  #     if (is.numeric(x)) NpsychBatteryNorms::values_to_labels(x, "RACE") else x
  #   }
  # ))

  # dat$RACE <- unlist(lapply(
  #   dat$RACE,
  #   \(x) {
  #     if (is.numeric(x)) NpsychBatteryNorms::values_to_labels(x, "RACE") else x
  #   }
  # ))

  cols_selected <- c(
    "Study ID:" = "NACCID",
    "Education (years):" = "EDUC",
    "BIRTHYR" = "BIRTHYR",
    "Gender:" = "SEX",
    "Handedness:" = "HANDED",
    "Race:" = "RACE"
  )

  cols_selected <- cols_selected[cols_selected %in% colnames(dat)]
  cur_pt_dat <- unique(dat[, cols_selected, with = F])
  names(cur_pt_dat) <- names(cols_selected)

  all_unique <- nrow(cur_pt_dat) == 1

  if (!all_unique) {
    cur_pt_dat[, names(.SD) := lapply(.SD, as.character)]

    cur_pt_dat[,
      names(.SD) := lapply(.SD, \(x) {
        uniq <- unique(x)
        if (length(uniq) > 1) {
          return(paste(x, collapse = "/"))
        }

        as.character(uniq)
      })
    ]

    cur_pt_dat <- unique(cur_pt_dat)
  }

  any_missing <- any(is.na(cur_pt_dat))

  if ("BIRTHYR" %in% colnames(cur_pt_dat)) {
    colnames(cur_pt_dat)[colnames(cur_pt_dat) == "BIRTHYR"] <- "Year of Birth:"
  }

  out <- data.table::melt(
    cur_pt_dat[, names(.SD) := lapply(.SD, as.character)],
    measure.vars = colnames(cur_pt_dat),
    variable.name = "name"
  ) |>
    gt::gt(
      id = "demographics-table"
    ) |>
    gt::cols_align(
      "right",
      .data$name
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        whitespace = "nowrap"
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold" # ,
        # size = px(16)
      ),
      locations = gt::cells_body(
        rows = .data$name == "Study ID:"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold",
        color = "red"
      ),
      locations = gt::cells_body(
        columns = .data$value,
        rows = is.na(.data$value)
      )
    ) |>
    gt::tab_options(
      column_labels.hidden = T,
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      table.font.size = gt::px(16)
    )

  if (!all_unique | any_missing) {
    out <- out |>
      # out |>
      gt::tab_footnote(
        footnote = paste(
          "Note: One or more variables vary between visits or are missing. These have been highlighted in red."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          whitespace = "normal"
        ),
        locations = gt::cells_footnotes()
      ) |>
      gt::sub_missing(
        missing_text = "(missing)"
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "red",
          weight = "bold"
        ),
        locations = gt::cells_body(
          rows = grepl(pattern = "\\/", x = .data$value) | is.na(.data$value)
        )
      )
  }

  out
}
