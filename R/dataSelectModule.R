#' Data selection UI module
#'
#' @description
#' A short description...
#'
#' @param id A single string.
#'
#' @returns
#' A `bslib::card` containing UI elements for selecting a data source and related options.
#'
#' @export
dataSelectUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    style = "width: 600px;",
    class = "mx-auto",
    shiny::uiOutput(ns("data_source_selector")),
    shiny::uiOutput(ns("data_source_ui")),
    height = "95vh"
  )
}


#' Data Selection Server Module
#'
#' @description
#' A short description...
#'
#' @param id A single string representing the module's identifier.
#'
#' @returns
#' A `shiny::moduleServer` function that returns a list containing:
#' \itemize{
#'   \item `dat_obj`: A reactive value holding the loaded data, which is expected
#'   to be a `data_nacc` object.
#'   \item `data_source_extras`: A reactive value holding a list of additional
#'   data source parameters or values.
#' }
#' Errors will be raised if the loaded data is not a `data_nacc` object.
#'
#' @export
dataSelectServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Find all available data_sources
    sources <- discover_data_sources()

    choices <- setNames(
      vapply(sources, \(s) s@id, character(1)),
      vapply(sources, \(s) s@name, character(1))
    )

    # print(choices)

    output$data_source_selector <- shiny::renderUI({
      if (length(choices) > 0) {
        shiny::selectizeInput(
          inputId = shiny::NS(id, "data_source"),
          label = "Data Source",
          choices = choices
        )
      }
    })

    # Config storage path
    config_dir <- tools::R_user_dir("ntrd", "config")
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Config file
    config_file <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(input$data_source)
      source <- sources[[input$data_source]]

      config_file(file.path(config_dir, paste0(source@id, ".bin")))
    })

    # --- Create data source UI ---
    output$data_source_ui <- shiny::renderUI({
      shiny::req(input$data_source)

      source <- sources[[input$data_source]]
      has_restore <- "restore" %in%
        names(data_source_servers[[input$data_source]])

      if (is.null(source)) {
        return(NULL)
      }

      # Check for saved configs
      shiny::req(config_file())
      has_saved <- file.exists(config_file())
      data_source_ns <- \(id) session$ns(shiny::NS(source@id, id))

      shiny::tagList(
        if (has_saved) {
          shiny::tagList(
            shiny::actionButton(
              inputId = shiny::NS(id, "load_config"),
              label = "Load saved configuration"
            ),
            shiny::hr()
          )
        },
        data_source_ui(source, ns = data_source_ns),
        bslib::layout_columns(
          bslib::input_task_button(
            id = shiny::NS(id, "go"),
            label = "Go"
          ),
          if (has_restore) {
            shiny::actionButton(
              label = "Save Configuration",
              inputId = shiny::NS(id, "launch_save_config")
            )
          }
        )
      )
    })

    # --- Modal to load previously saved config ---
    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Load Configuration",
          easyClose = TRUE,
          footer = NULL,
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "config_password"),
              label = "Data File Password",
              placeholder = "Enter Password"
            ),
            .cssSelector = "input",
            autocomplete = "current-password"
          ),
          bslib::input_task_button(
            id = shiny::NS(id, "retrieve_config"),
            label = "Load"
          )
        )
      )
    }) |>
      shiny::bindEvent(input$load_config)

    shiny::observe({
      shiny::req(input$data_source)

      params <- tryCatch(
        safer::retrieve_object(
          config_file(),
          key = input$config_password
        ),
        error = \(e) {
          shiny::showNotification(
            "Failed to load saved config.",
            type = "error"
          )
          NULL
        }
      )

      if (!is.null(params)) {
        data_source_servers[[input$data_source]]$restore(params)
        shiny::removeModal()
      }
    }) |>
      shiny::bindEvent(input$retrieve_config)

    # --- Start extension servers ---
    data_source_servers <- lapply(sources, \(source) {
      data_source_server(source, id = source@id)
    })

    # --- Click "Save Configuration" to access save popup
    shiny::observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Save Data Sources",
          easyClose = TRUE,
          footer = NULL,
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "config_password"),
              label = "Enter a password to save data sources",
              placeholder = "Enter Password"
            ),
            .cssSelector = "input",
            autocomplete = "new-password"
          ),
          shiny::tagAppendAttributes(
            shiny::passwordInput(
              inputId = shiny::NS(id, "config_password_repeated"),
              label = "Confirm Password",
              placeholder = "Re-enter Password"
            ),
            .cssSelector = "input",
            autocomplete = "new-password"
          ),
          shiny::uiOutput(shiny::NS(id, "check_passwords"))
        )
      )
    }) |>
      shiny::bindEvent(input$launch_save_config)

    # --- Check repeated passwords match. If yes, return save button
    output$check_passwords <- shiny::renderUI({
      shiny::req(input$config_password, input$config_password_repeated)

      if (input$config_password != input$config_password_repeated) {
        return(shiny::HTML(
          "<span style='color: red; font-weight: bold;'>Passwords don't match</span>"
        ))
      }

      # shiny::tagList(
      shiny::actionButton(
        inputId = shiny::NS(id, "save_config"),
        label = "Save Data Sources"
        # ),
        # shiny::tags$script(shiny::HTML(
        #   sprintf(
        #     "$(document).on('click', '#%1$s-download_data_sources', function() {Shiny.setInputValue(%1$s-download_data_sources_clicked, 1, {priority: 'event'});});",
        #     id
        #   )
        # ))
      )
    })

    # --- Click "Save Configuration" to save output of data_source_servers[[source]]
    shiny::observe({
      shiny::req(input$data_source)

      ## Get current source, and server results
      source <- sources[[input$data_source]]
      dat_src_server <- data_source_servers[[input$data_source]]

      if (file.exists(config_file())) {
        file.remove(config_file())
      }

      safer::save_object(
        object = dat_src_server$params(),
        key = input$config_password,
        conn = config_file()
      )

      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$save_config)

    # --- Reactive values for data object and extras
    dat_obj <- shiny::reactiveVal(NULL)
    data_source_extras <- shiny::reactiveVal(NULL)
    default_methods <- shiny::reactiveVal(NULL)

    # --- Click "Go!" to load data ---
    shiny::observe({
      shiny::req(input$data_source)

      ## Get current source, and server results
      source <- sources[[input$data_source]]
      ext_pkg <- S7::S7_class(source)@package

      ## Set defaults for the active extension
      if (ext_pkg != "ntrd") {
        set_defaults <- get0(
          ".set_defaults",
          envir = asNamespace(ext_pkg)
        )

        if (is.function(set_defaults)) {
          set_defaults()
        }
      }

      lapply(
        setNames(ntrs::list_npsych_scores(), ntrs::list_npsych_scores()),
        \(x) {
          get_std_defaults(get_npsych_scores(x)())
        }
      ) |>
        purrr::discard(is.null) |>
        default_methods()

      dat_src_server <- data_source_servers[[input$data_source]]

      if (is.null(dat_src_server)) {
        return()
      }

      result <- do.call(
        data_load,
        c(
          source = source,
          dat_src_server$params()
        )
      )

      if (is.null(result)) {
        return()
      }

      if (!S7::S7_inherits(result, data_nacc)) {
        cli::cli_abort(
          "{.val {input$data_source}}: {.fn data_source_server} must return a
          {.cls data_nacc} object."
        )
      }

      dat_obj(result)
    }) |>
      shiny::bindEvent(input$go)

    # Proxy extras from active plugin
    shiny::observe({
      shiny::req(input$data_source)
      srv <- data_source_servers[[input$data_source]]

      if ("extras" %in% names(srv) && !is.null(srv$extras)) {
        data_source_extras(shiny::reactiveValuesToList(srv$extras))
      }
    })

    return(list(
      dat_obj = dat_obj,
      extras = data_source_extras,
      default_methods = default_methods
    ))
  })
}


dataSelectApp <- function(testing = FALSE) {
  shinyAddResources()

  ui <- bslib::page_fluid(
    shinyApp_header(),
    dataSelectUI("dat_select")
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 1000 * 1024^2)

    dat_obj <- shiny::reactiveVal()

    data_input <- dataSelectServer("dat_select")

    shiny::observe({
      dat_obj(data_input$dat_obj())
    }) |>
      shiny::bindEvent(data_input$dat_obj())

    # shiny::observe({
    #   shiny::req(data_input$data_source_extras)
    #   # cat("data_source_extras:")
    #   # print(data_input$data_source_extras)
    #   browser()
    # })

    # shiny::observe({
    #   # print(names(data_input$dat_obj))
    #   # print(class(data_input$dat_obj))
    #   print(data_input$extras())
    # })
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
