#' Shiny Module to Display Previous Diagnoses
#'
#' @rdname prevDiagnosesModule
#'
#' @param id An ID string to match module UI and module server
#'
#' @export
prevDiagnosesUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "prev_diagnoses_table"))
  )
}

#' @rdname prevDiagnosesModule
#'
#' @inheritParams prev_diagnoses_table
#' @param print_updating logical (default `FALSE`); should a message be printed to indicate that the table is being updated? For debugging.
#'
#' @export
prevDiagnosesServer <- function(
  id,
  dat,
  print_updating = F,
  table_font_size = shiny::reactive(100)
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$prev_diagnoses_table <- shiny::renderUI({
      if (print_updating) {
        print("Updating previous diagnoses table...")
      }

      prev_diagnoses_table(
        dat(),
        table_font_size = table_font_size()
      )
    })
  })
}

#' @rdname prevDiagnosesModule
#'
#' @inheritParams prevDiagnosesServer
prevDiagnosesApp <- function(
  dat,
  print_updating = F,
  table_font_size = shiny::reactive(100)
) {
  ui <- bslib::page_fillable(
    prevDiagnosesUI("prev_diagnoses_table")
  )

  server <- function(input, output, session) {
    prevDiagnosesServer(
      "prev_diagnoses_table",
      shiny::reactive(dat),
      print_updating,
      table_font_size
    )
  }

  shiny::shinyApp(ui, server)
}
