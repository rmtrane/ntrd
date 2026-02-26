demo_source <- S7::new_class(
  "demo_source",
  parent = data_source,
  constructor = function() {
    S7::new_object(
      data_source(
        name = "Demo NACC data",
        id = "demo"
      )
    )
  }
)

S7::method(data_source_ui, demo_source) <- function(source, ns) {
  shiny::p("Click 'Go' to load demo data. No configuration needed.")
}

S7::method(data_source_server, demo_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(params = reactive(list()), session = session)
  })
}

S7::method(data_load, demo_source) <- function(source, params) {
  list(
    data = data_nacc(data = demo_data)
  )
}
