#' @include data_source.R
#' @keywords internal
csv_source <- new_data_source(name = "CSV file", id = "csv", package = "ntrd")

#' @keywords internal
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

#' @keywords internal
S7::method(data_source_server, csv_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    params <- shiny::reactive({
      shiny::req(input$csv_file)

      list(file_path = input$csv_file$datapath)
    })

    list(params = params)
  })
}

#' @keywords internal
S7::method(data_load, csv_source) <- function(source, file_path) {
  dat <- data.table::fread(file_path)

  dat[,
    names(.SD) := purrr::imap(
      .SD,
      \(x, idx) {
        ntrs::get_npsych_scores(idx)(x)
      }
    ),
    .SDcols = names(dat)[names(dat) %in% ntrs::list_npsych_scores()]
  ]

  # data_nacc validates the data set
  data_nacc(dat)
}
