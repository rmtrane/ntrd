#' Run Shiny App
#'
#' @description
#' Wrapper that runs the Shiny application.
#'
#' @param testing Logical: is the app run for testing purposes?
#'
#' @returns
#' Runs the Shiny app.
#'
#' @export
shinyDashboard <- function(
  testing = FALSE
) {
  options(
    shiny.maxRequestSize = 1000 * 1024^2,
    shiny.autoload.r = FALSE
  )

  ## Add resources
  shinyAddResources(development = testing)

  ## Stop daemons if they are running
  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  ## Start single daemon for asynchronously loading biomarker data
  mirai::daemons(1)
  shiny::onStop(\(x) mirai::daemons(0))

  shiny::shinyApp(
    ui = appUI,
    server = appServer,
    options = list(
      port = 5556,
      launch.browser = TRUE,
      test.mode = testing
    )
  )
}
