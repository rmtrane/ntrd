#' Shiny app header
#'
#' @returns
#' A `shiny::tagList` used to add `www/scripts.js` and `www/styles.css` to the head
#' of the app, and adding the spinner layers.
#'
#' @export
shinyApp_header <- function() {
  shiny::tagList(
    # shiny::useBusyIndicators(),
    shiny::tags$head(
      shiny::tags$script(
        src = "www/scripts.js"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    shiny::tags$div(id = "spinner", class = "loader"),
    shiny::tags$div(id = "spinner_overlay", class = "loader_overlay")
  )
}
