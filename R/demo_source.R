demo_source <- new_data_source(
  name = "Demo NACC data",
  id = "demo",
  package = "ntrd"
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
  tmp <- data.table::copy(demo_data)

  tmp[,
    names(.SD) := purrr::imap(.SD, \(x, idx) {
      # do.call(idx, args = list(x = x))
      ntrs::get_npsych_scores(idx)(x)
    }),
    .SDcols = intersect(colnames(tmp), ntrs::list_npsych_scores())
  ]

  ## Add derived variables
  tmp[,
    c(
      "REYTOTAL",
      "REYAREC",
      "FAS",
      "MOCACLOCK"
    ) := list(
      calc_REYTOTAL(
        REY1REC,
        REY2REC,
        REY3REC,
        REY4REC,
        REY5REC
      ),
      calc_REYAREC(REYTCOR, REYFPOS),
      calc_FAS(
        BILLS,
        TAXES,
        SHOPPING,
        GAMES,
        STOVE,
        MEALPREP,
        EVENTS,
        PAYATTN,
        REMDATES,
        TRAVEL
      ),
      calc_MOCACLOCK(MOCACLOC, MOCACLON, MOCACLOH)
    )
  ]

  data_nacc(data = tmp)
}
