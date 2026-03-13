#' Create base plotly figure
#'
#' Create a base plot where traces can later be added.
#'
#' @param x_range vector with two elements giving the smallest and largest values to use on x-axis (in order)
#' @param y_range vector with two elements giving the smallest and largest values to use on y-axis (in order)
#' @param shade_descriptions logical; should ranges for descriptions be shaded?
#' @param fill_alpha opacity value for shaded areas
#' @param source passed to `plotly::plot_ly`
#'
#' @param new_id Optional. String to use as the plotly source ID. If `NULL`
#'   (default), a random string is assigned.
#' @inheritParams assessment_summary_data
#'
#' @keywords internal
#'
#' @export
base_plot_z_scores <- function(
  x_range,
  y_range,
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
  shade_descriptions = T,
  fill_alpha = 0.2,
  source = "A",
  new_id = NULL
) {
  if (
    shade_descriptions &
      (missing(fill_values) ||
        identical(fill_values, quote(expr = )) ||
        is.null(fill_values))
  ) {
    fill_values <- setNames(
      calc_fill_colors(length(descriptions)),
      nm = names(descriptions)
    )
  }

  ## Get min and max values for y-axis
  y_min <- y_range[1]
  y_max <- y_range[2]

  z_scores_from_percentiles <- qnorm(descriptions[descriptions < 1])

  # tiles <- data.table::data.table(
  #   ymin = c(y_min, z_scores_from_percentiles),
  #   ymax = c(z_scores_from_percentiles, y_max),
  #   descrs = names(descriptions)
  # )

  # print(class(dat$VISITDATE))
  # print(date_range(as.Date(dat$VISITDATE)))

  p <- plotly::plot_ly(
    type = "scatter",
    mode = "none",
    x = x_range,
    showlegend = FALSE,
    hoverinfo = "none",
    source = source
  ) |>
    plotly::layout(
      xaxis = list(
        title = "Date of Test",
        range = x_range,
        minallowed = x_range[1],
        maxallowed = x_range[2]
      ),
      yaxis = list(
        title = "z-score",
        # range = list(y_min, y_max),
        maxallowed = y_max,
        minallowed = y_min,
        showgrid = F
      ),
      legend = list(
        orientation = "h"
      )
    )

  if (!is.null(new_id)) {
    names(p$x$visdat) <- new_id
    p$x$cur_data <- new_id

    names(p$x$attrs) <- new_id
    names(p$x$layoutAttrs) <- new_id
  }

  ## Add horizontal lines and fill colors according to descriptions
  for (i in seq_along(fill_values)) {
    # seq_along(z_scores_from_percentiles)) {
    p <- p |>
      ## horizontal lines
      plotly::add_trace(
        y = z_scores_from_percentiles[min(
          i,
          length(z_scores_from_percentiles)
        )],
        type = "scatter",
        color = I(grDevices::rgb(0, 0, 0, 0.3)),
        alpha = I(as.numeric(i != length(fill_values))),
        mode = "lines",
        linetype = I("dashed"),
        line = list(
          width = 1
        ),
        hoverinfo = "none"
      ) |>
      ## fill
      plotly::add_trace(
        y = c(
          y_min,
          z_scores_from_percentiles[-length(z_scores_from_percentiles)],
          y_max
        )[i],
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = do.call(
          grDevices::rgb,
          as.list(c(
            grDevices::col2rgb(fill_values[i])[, 1] / 255,
            alpha = fill_alpha
          ))
        ),
        name = names(fill_values)[i],
        visible = shade_descriptions,
        line = list(
          width = 0
        ),
        hoverinfo = "none"
      )
  }

  p
}

#' @keywords internal
date_range <- function(dates) {
  dates <- as.Date(dates)

  c(
    lubridate::floor_date(
      min(dates, na.rm = T) - months(3),
      unit = "quarter"
    ),
    lubridate::ceiling_date(
      max(dates, na.rm = T) + months(3),
      unit = "quarter"
    )
  )
}
