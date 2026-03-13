#' Shiny Module to Display Longitudinal Trends in Table
#'
#' Table version of the plotCogVarModule.
#'
#' @param id string to tie UI to Server
#' @inheritParams assessment_longitudinal_table
#'
#' @rdname longTableModule
#'
#' @export
longTableUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "long_table"))
  )
}

#' @rdname longTableModule
#'
#' @inheritParams assessment_longitudinal_table
#' @param print_updating logical (default `FALSE`); should message be printed to let user know the table is updating. For debugging.
#'
#' @export
longTableServer <- function(
  id,
  dat,
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  print_updating = F
) {
  if (!shiny::is.reactive(dat)) {
    dat <- shiny::reactiveVal(dat)
  }

  if (!shiny::is.reactive(descriptions)) {
    descriptions <- shiny::reactiveVal(descriptions)
  }

  if (!shiny::is.reactive(fill_values)) {
    fill_values <- shiny::reactiveVal(fill_values)
  }

  if (!shiny::is.reactive(table_font_size)) {
    table_font_size <- shiny::reactiveVal(table_font_size)
  }

  if (!shiny::is.reactive(methods)) {
    methods <- shiny::reactiveVal(methods)
  }

  shiny::moduleServer(id, function(input, output, session) {
    all_visits <- shiny::reactiveVal(value = TRUE)

    shiny::observe({
      all_visits(!all_visits())

      shiny::updateActionButton(
        inputId = "show_hide_empty",
        label = c("Show All Visits", "Hide Visits with No Scores")[
          all_visits() + 1
        ]
      )
    }) |>
      shiny::bindEvent(input$show_hide_empty)

    output$long_table <- shiny::renderUI({
      if (print_updating) {
        print("Updating longitudinal table...")
      }

      cur_show_hide_empty <- input$show_hide_empty

      assessment_longitudinal_table(
        dat(),
        methods = methods(),
        table_font_size = table_font_size(),
        table_id = "long_table",
        show_all_visits = all_visits(),
        descriptions = descriptions(),
        fill_values = fill_values(),
        stubhead_label = shiny::HTML(
          paste0(
            '<button id="',
            shiny::NS(id, "show_hide_empty"),
            '" type="button"',
            'class="btn btn-default action-button"',
            'style="height: 16px; line-height: 80%; padding-top: 7px; padding-bottom: 16px; width: 250px">',
            ifelse(
              all_visits(),
              'Hide Visits with No Scores',
              'Show All Visits'
            ),
            '</button>'
          )
        )
      )
    })
  })
}


#' Long table app
#'
#' @description
#' A short description...
#'
#' @inheritParams longTableServer
#'
#' @returns
#' A shiny app.
#'
#' @rdname longTableModule
#'
#' @export
longTableApp <- function(
  dat,
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  print_updating = F
) {
  ui <- bslib::page_fillable(
    longTableUI("long_table")
  )

  server <- function(input, output, session) {
    longTableServer(
      "long_table",
      dat,
      descriptions = descriptions,
      fill_values = fill_values,
      methods = methods,
      table_font_size = table_font_size,
      print_updating = print_updating
    )
  }

  shiny::shinyApp(ui, server)
}
