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
  if (is.null(package) || is.na(package) || is.null(action)) {
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
      # bsicons::bs_icon("arrow-up-circle")
      shiny::HTML(
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-arrow-up-circle " style="height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;" aria-hidden="true" role="img" ><path fill-rule="evenodd" d="M1 8a7 7 0 1 0 14 0A7 7 0 0 0 1 8zm15 0A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-7.5 3.5a.5.5 0 0 1-1 0V5.707L5.354 7.854a.5.5 0 1 1-.708-.708l3-3a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 5.707V11.5z"></path></svg>'
      )
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
