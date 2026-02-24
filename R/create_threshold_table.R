#' Count decimal places
#'
#' @description
#' Get number of decimal places in a number.
#'
#' @param x A numeric value.
#'
#' @returns
#' A single integer representing the number of decimal places in `x`.
#'
#' @export
count_decimal_places <- function(x) {
  # Convert to character and remove scientific notation
  x_str <- format(x, scientific = FALSE, trim = TRUE)

  # Remove leading/trailing whitespace
  x_str <- trimws(x_str)

  # Remove decimal point and leading zeros
  x_str <- gsub("^-?0*\\.", "", x_str)

  # Count remaining digits
  nchar(gsub("[^0-9]", "", x_str))
}

#' Create thresholds table
#'
#' @description
#' Create HTML table to include in tooltip for info column of bio_tab_to_html_table.
#'
#' @param thres A data frame or tibble with `min`, `max`, and `label` columns.
#'
#' @returns
#' A `shiny::HTML` object representing an HTML table of the thresholds.
#'
#' @export
create_thresholds_table <- function(thres) {
  sig_digits <- max(unlist(lapply(
    c(thres$min, thres$max),
    count_decimal_places
  )))
  sprintf_fmt <- paste0("%.", sig_digits, "f")

  thres$min <- ifelse(
    thres$min == 0,
    "",
    paste(sprintf(sprintf_fmt, thres$min), "<")
  )
  thres$max <- ifelse(
    thres$max < Inf,
    paste("&le;", sprintf(sprintf_fmt, thres$max)),
    ""
  )
  with(
    thres,
    shiny::HTML(paste0(
      "<table>",
      paste0(
        "<tr><td>",
        label,
        "</td><td>if</td><td>",
        min,
        "</td><td>value</td><td>",
        max,
        "</td></tr>",
        collapse = ""
      ),
      "</table>"
    ))
  )
}
