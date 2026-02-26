#' @include data_source.R
csv_source <- S7::new_class(
  "csv_source",
  parent = data_source,
  constructor = function() {
    S7::new_object(
      data_source(
        name = "CSV file",
        id = "csv"
      )
    )
  }
)

S7::method(data_source_ui, csv_source) <- function(source, ns) {
  shiny::tagList(
    shiny::p(
      "Select a file to upload. Note: this file is expected to follow the format of the NACC data. For more, see https://naccdata.org."
    ),
    shiny::fileInput(
      inputId = ns("csv_file"),
      label = "Upload CSV file",
      accept = ".csv"
    )
  )
}

S7::method(data_source_server, csv_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      shiny::req(input$csv_file)

      dat <- data.table::fread(input$csv_file$datapath)

      # data_nacc validates the data set
      data_nacc(dat)
    })
  })
}
