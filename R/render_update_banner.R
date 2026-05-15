#' Render the update-available banner
#'
#' @param result An `update_result` S7 object.
#' @param package The package name the result is about (used in the
#'   button's input id so the appServer observer can dispatch correctly).
#' @return A `shiny::tagList` if an update is available, or NULL otherwise.
#' @keywords internal
#' @noRd
render_update_banner <- function(result, package, action = NULL) {
  if (!S7::S7_inherits(result, update_result) || !result@available) {
    return(NULL)
  }

  # Only render when we have both a package label and a click target —
  # showing "update available" with no way to act on it is a worse UX
  # than showing nothing.
  if (is.null(package) || is.null(action)) {
    return(NULL)
  }

  news_link <- if (!is.na(result@news_url)) {
    shiny::tags$a(
      "What's new?",
      href = result@news_url,
      target = "_blank",
      rel = "noopener noreferrer",
      class = "update-banner-news-link"
    )
  }

  shiny::tags$div(
    class = "update-banner",
    role = "status", # accessibility: announces non-urgent updates
    shiny::tags$span(
      class = "update-banner-icon",
      bsicons::bs_icon("arrow-up-circle")
    ),
    shiny::tags$span(
      class = "update-banner-text",
      sprintf(
        "Update available for %s: %s \u2192 %s.",
        package,
        result@current,
        result@latest
      )
    ),
    news_link,
    action
  )
}
