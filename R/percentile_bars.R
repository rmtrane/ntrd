#' Build inline percentile bars as HTML.
#'
#' Replaces the gt-coupled `my_gt_plt_bar_pct()`: returns a list of
#' `shiny::HTML()` values, one per input element, suitable for storing
#' directly in a `data.table` column that `html_table` then passes through
#' verbatim.
#'
#' The visual is the same as gt's: a fixed-width grey track containing a
#' colored bar sized to `value`. With `labels = TRUE`, a small percentage
#' label is overlaid — inside the bar if the bar is long enough, otherwise
#' outside it. Missing values render as an em-dash, matching the original.
#'
#' @param values Numeric vector. Percentiles in 0–100 when `scaled = TRUE`,
#'   or arbitrary nonnegative numbers when `scaled = FALSE` (in which case
#'   they're normalized against `max(values, na.rm = TRUE)`).
#' @param scaled If `TRUE`, treat `values` as already on a 0–100 scale.
#'   Otherwise rescale to the column maximum.
#' @param labels If `TRUE`, overlay a percentage label.
#' @param label_cutoff Fraction (0–1) of the bar length below which the
#'   label is rendered *outside* the bar rather than centered inside it.
#' @param decimals Decimal places shown in the label.
#' @param height Bar height in pixels.
#' @param width Track width in pixels.
#' @param fill Bar fill color.
#' @param background Track background color.
#' @param font_style Label `font-weight` (`"bold"`, `"normal"`, `"italic"`).
#' @param font_size Label `font-size`, as a CSS length string.
#'
#' @returns A list of `shiny::HTML()` values, length `length(values)`.
#'
#' @keywords internal
percentile_bars <- function(
  values,
  scaled = TRUE,
  labels = TRUE,
  label_cutoff = 0.4,
  decimals = 1,
  height = 16,
  width = 100,
  fill = "purple",
  background = "#e1e1e1",
  font_style = "bold",
  font_size = "10px"
) {
  stopifnot(
    "`label_cutoff` must be between 0 and 1" = label_cutoff >= 0 &&
      label_cutoff <= 1,
    "`font_style` must be 'bold', 'normal', or 'italic'" = font_style %in%
      c("bold", "normal", "italic")
  )

  numeric_vals <- suppressWarnings(as.double(values))
  finite_vals <- numeric_vals[!is.na(numeric_vals)]
  max_x <- if (length(finite_vals)) max(finite_vals) else 0

  fg_on_fill <- ideal_fgnd_color(fill)
  fg_on_bg <- ideal_fgnd_color(background)

  lapply(numeric_vals, function(x) {
    if (is.na(x)) {
      return(shiny::HTML("&mdash;"))
    }

    scaled_value <- if (scaled) {
      x
    } else if (max_x > 0) {
      x / max_x * 100
    } else {
      0
    }

    inner <- if (labels) {
      label_value <- if (scaled) x else x / max_x * 100
      label <- paste0(round(label_value, decimals), "%")

      if (x < label_cutoff * max_x) {
        # Bar too short to hold the label — place the label to the right
        # of the bar, absolutely positioned at the bar's end.
        bar_css <- paste0(
          "background:",
          fill,
          ";",
          "width:",
          scaled_value,
          "%;",
          "height:",
          height,
          "px;",
          "display:flex;align-items:center;",
          "justify-content:flex-start;position:relative;"
        )
        label_css <- paste0(
          "color:",
          fg_on_bg,
          ";",
          "position:absolute;left:0%;",
          "margin-left:",
          scaled_value * width / 100,
          "px;",
          "font-weight:",
          font_style,
          ";",
          "font-size:",
          font_size,
          ";"
        )
        paste0(
          "<div style='",
          bar_css,
          "'>",
          "<span style='",
          label_css,
          "'>",
          label,
          "</span>",
          "</div>"
        )
      } else {
        # Bar long enough — center the label inside it.
        bar_css <- paste0(
          "background:",
          fill,
          ";",
          "width:",
          scaled_value,
          "%;",
          "height:",
          height,
          "px;",
          "display:flex;align-items:center;justify-content:center;",
          "color:",
          fg_on_fill,
          ";",
          "font-weight:",
          font_style,
          ";",
          "font-size:",
          font_size,
          ";",
          "position:relative;"
        )
        label_css <- paste0(
          "color:",
          fg_on_fill,
          ";",
          "position:absolute;left:0px;margin-left:5px;",
          "font-weight:",
          font_style,
          ";",
          "font-size:",
          font_size,
          ";"
        )
        paste0(
          "<div style='",
          bar_css,
          "'>",
          "<span style='",
          label_css,
          "'>",
          label,
          "</span>",
          "</div>"
        )
      }
    } else {
      paste0(
        "<div style='background:",
        fill,
        ";",
        "width:",
        scaled_value,
        "%;",
        "height:",
        height,
        "px;'></div>"
      )
    }

    # Outer track. `width` is fixed so the bar always sits in the same
    # horizontal space regardless of value.
    shiny::HTML(paste0(
      "<div style='",
      "width:",
      width,
      "px;",
      "background:",
      background,
      ";",
      "'>",
      inner,
      "</div>"
    ))
  })
}


# Pick black or white text for legible contrast against a given background.
# Lifted (with minor tidy-up) from gtExtras::ideal_fgnd_color so this file
# stays self-contained.
ideal_fgnd_color <- function(bg, light = "#FFFFFF", dark = "#000000") {
  rgb <- grDevices::col2rgb(bg) / 255
  # Relative luminance per WCAG (sRGB; gamma-adjusted channels).
  to_linear <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  }
  lum <- 0.2126 *
    to_linear(rgb[1L, ]) +
    0.7152 * to_linear(rgb[2L, ]) +
    0.0722 * to_linear(rgb[3L, ])
  unname(ifelse(lum > 0.179, dark, light))
}
