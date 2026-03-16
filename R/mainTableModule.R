#' Shiny Module to Display NACC T-Cog Neuropsychological Assessment Summary Table
#'
#' @param id An ID string to match module UI and server UI
#' @param table_font_size (optional) scalar indicating font size as a percentage.
#' @inheritParams assessment_summary_data
#'
#' @rdname mainTableModule
#'
#' @export

mainTableUI <- function(id) {
  bslib::card(bslib::card_body(
    shiny::uiOutput(shiny::NS(id, "mainTable")),
    if (rlang::is_installed("pagedown")) {
      shiny::tagList(
        shiny::actionButton(
          shiny::NS(id, "genPDF"),
          label = "Generate PDF for Download"
        ),
        shiny::uiOutput(shiny::NS(id, "downloadTable"))
      )
    } else {
      shiny::p(
        "To be able to download table, please install 'pagedown' using `install.packages('pagedown')`, and restart app."
      )
    },
    fillable = F
  ))
}

#' @rdname mainTableModule
#'
#' @param print_updating logical (defualt `FALSE`); should a message be displayed when table is being updated? For debugging.
#'
#' @export
mainTableServer <- function(
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
  include_caption = F,
  print_updating = F
) {
  if (!shiny::is.reactive(fill_values)) {
    if (is.null(fill_values)) {
      fill_values <- calc_fill_colors(n = length(descriptions))
    }
    fill_values <- shiny::reactiveVal(fill_values)
  }

  if (!shiny::is.reactive(descriptions)) {
    descriptions <- shiny::reactive(descriptions)
  }

  if (!shiny::is.reactive(table_font_size)) {
    table_font_size <- shiny::reactiveVal(table_font_size)
  }

  if (!shiny::is.reactive(methods)) {
    methods <- shiny::reactiveVal(methods)
  }

  shiny::moduleServer(id, function(input, output, session) {
    output$mainTable <- gt::render_gt(
      mainTable()
    )

    mainTable <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(descriptions, fill_values)

      for_table <- dat()

      if (is.null(for_table)) {
        cli::cli_alert_info(
          "{.arg for_table} is {.code NULL}. Returning empty element."
        )
        return()
      }

      if (!data.table::is.data.table(for_table)) {
        cli::cli_abort("{.arg for_table} must be a {.cls data.table} object.")
      }

      if (nrow(data.frame(for_table)) != 1) {
        cli::cli_alert_info(
          "{.arg for_table} must have exactly 1 row, but has {nrow(for_table)}. Returning empty element."
        )
        return()
      }

      if (print_updating) {
        cli::cli_alert(text = "Updating main table...")
      }

      summary_dat <- assessment_summary_data(
        dat = for_table,
        id = "NACCID",
        methods = methods(),
        include_caption = include_caption
      )

      mainTable(
        assessment_summary_table(
          summary_dat = summary_dat,
          bar_height = 16 * table_font_size() / 100
        ) |>
          gt::tab_options(
            data_row.padding = gt::px(2),
            row_group.padding = gt::px(4),
            table.font.size = gt::pct(table_font_size())
          )
      )
    })

    shiny::observe({
      output$downloadTable <- shiny::renderUI({
        tmp_path <- tempdir()
        tmp_html <- tempfile(tmpdir = tmp_path, fileext = ".html")
        tmp_pdf <- tempfile(tmpdir = tmp_path, fileext = ".pdf")

        gt::gtsave(
          data = mainTable() |>
            gt::tab_caption(
              caption = make_table_caption(dat()$NACCID, dat()$VISITDATE)
            ),
          filename = tmp_html
        )

        # launch the PDF file generation
        pagedown::chrome_print(
          input = tmp_html,
          output = tmp_pdf,
          extra_args = chrome_extra_args(),
          verbose = 1,
          async = TRUE
        )$then(
          onFulfilled = function(value) {
            shiny::showNotification(
              paste("PDF file succesfully generated"),
              type = "message"
            )
            output$downloadPDF <- shiny::downloadHandler(
              filename = function() {
                make_pdf_filename(dat()$NACCID, dat()$VISITDATE)
              },
              content = function(file) {
                file.copy(value, file)
              },
              contentType = "application/pdf"
            )
            # return a download button
            shiny::downloadButton(shiny::NS(id, "downloadPDF"), "Download PDF")
          },
          onRejected = function(error) {
            shiny::showNotification(
              error$message,
              duration = NULL,
              type = "error"
            )
            shiny::HTML("")
          }
        )
      })
    }) |>
      shiny::bindEvent(input$genPDF)
  })
}

#' @rdname mainTableModule
#'
#' @inheritParams assessment_summary_data
#' @param testing logical (default `FALSE`); should the app be run in testing mode?
#'
#' @return a Shiny app object.
#'
#' @export
mainTableApp <- function(
  dat,
  testing = FALSE
) {
  development <- dir.exists("inst/shiny/www")

  if (development) {
    print("Development...")
  }

  shiny::addResourcePath(
    "www",
    ifelse(
      development,
      "inst/shiny/www",
      system.file("www", package = "ntrd")
    )
  )

  ui <- bslib::page_fluid(
    shiny::tags$header(
      shiny::tags$script(src = "www/scripts.js"),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    shiny::tags$div(id = "spinner", class = "loader"),
    shiny::tags$div(id = "spinner_overlay", class = "loader_overlay"),
    mainTableUI("main_table")
  )

  server <- function(input, output, session) {
    mainTableServer(
      "main_table",
      dat = shiny::reactive(dat),
      methods = "infer",
      table_font_size = shiny::reactive(100),
      include_caption = T
    )
  }

  shiny::shinyApp(ui, server, options = list(port = 3229, test.mode = testing))
}


#' Generate PDF filename for download
#'
#' @param naccid Character string with the NACC ID.
#' @param visitdate Character string or Date with the visit date.
#' @param time POSIXct timestamp used for the "created-on" portion of the
#'   filename. Defaults to [Sys.time()].
#'
#' @return A character string suitable for use as a PDF filename.
#'
#' @keywords internal
make_pdf_filename <- function(naccid, visitdate, time = Sys.time()) {
  paste0(
    paste(naccid, visitdate, sep = "-"),
    "_created-on-",
    as.Date(time),
    "-at-",
    format(time, "%I-%M_%P"),
    ".pdf"
  )
}

#' Generate table caption from ID and visit date
#'
#' @param naccid Character vector of NACC IDs (will be uniquified).
#' @param visitdate Character or Date vector of visit dates (will be uniquified).
#'
#' @return A single character string caption.
#'
#' @keywords internal
make_table_caption <- function(naccid, visitdate) {
  paste("ID:", unique(naccid), ". Visit Date:", unique(visitdate))
}

#' Return Chrome CLI arguments
#'
#' This is a helper function which returns arguments to be passed to Chrome.
#' This function tests whether the code is running on shinyapps and returns the
#' appropriate Chrome extra arguments. Source: https://github.com/RLesur/chrome_print_shiny
#'
#' @param default_args Arguments to be used in any circumstances.
#'
#' @return A character vector with CLI arguments to be passed to Chrome.
#'
#' @keywords internal
chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- default_args
  # Test whether we are in a shinyapps container
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(
      args,
      "--no-sandbox", # required because we are in a container
      "--disable-dev-shm-usage"
    ) # in case of low available memory
  }
  args
}
