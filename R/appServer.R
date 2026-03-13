#' Server Logic for shinyAssessmentApp
#'
#' @keywords internal
#'
#' @export
appServer <- function(input, output, session) {
  ## Hide 'Participant Data' on startup
  bslib::nav_hide(id = "main_navbar", target = "colSelect")
  bslib::nav_hide(id = "main_navbar", target = "tables-and-figures")
  # bslib::nav_hide(id = "long-trends", target = "biomarkers")

  ## Setup data select module
  dat_sel <- dataSelectServer("dataSelect")

  ## Reactive values to store data object, selected data source, and data type,
  ## all assigned from dataSelect module. Also, reactive value to indicate if
  ## user should be allowed to select columns for variables. We only allow this
  ## for csv upload.

  devmode <- shiny::reactiveVal(value = FALSE)

  selected_date <- shiny::reactiveVal()

  shiny::observe({
    if (!is.null(dat_sel$extras()$extension_ui)) {
      output$extension_ui <- shiny::renderUI({
        #   bslib::nav_panel(
        #     title = "Biomarkers",
        #     value = "biomarkers",
        dat_sel$extras()$extension_ui()
        # )
      })
      # bslib::nav_show(id = "long-trends", target = "biomarkers")
    }
  }) #()$panda_api_token)

  ## Reactive object with available columns to use to select from
  cols_avail <- shiny::reactive({
    if (S7::S7_inherits(dat_sel$dat_obj(), data_nacc)) {
      colnames(dat_sel$dat_obj()@data)
    }
  })

  # default_methods <- shiny::reactive({
  #   shiny::req(dat_sel$default_methods())

  #   dat_sel$default_methods()
  # })

  ## Reactive values to hold selected columns, and methods
  col_sel <- shiny::reactiveVal()
  std_methods <- shiny::reactiveVal()

  ## When dat_obj changes, flush selected columns and methods
  shiny::observe({
    col_sel(NA)
    std_methods(NA)
  }) |>
    shiny::bindEvent(
      dat_sel$dat_obj()
    )

  ## Select columns
  # colSelectOutput <- colSelectServer(
  methodSelectOutput <- methodSelectServer(
    "colSelect",
    # col_names = cols_avail,
    dat_obj = dat_sel$dat_obj,
    default_methods = dat_sel$default_methods,
    col_selection = "disable" # allow_col_selections()
  )

  shiny::observe({
    shiny::req(methodSelectOutput$std_methods())

    col_sel(methodSelectOutput$var_cols())
    std_methods(methodSelectOutput$std_methods())
  })

  shiny::observe({
    bslib::nav_show(id = "main_navbar", target = "colSelect", select = T)
  }) |>
    shiny::bindEvent(
      cols_avail(),
      ignoreInit = T
    )

  shiny::observe({
    bslib::nav_select(id = "main_navbar", selected = "colSelect")
  }) |>
    shiny::bindEvent(
      input$goToColSelect,
      ignoreInit = T,
      ignoreNULL = T
    )

  ## Prepare data and get nacc_var_groups
  fin_dat <- shiny::reactiveVal()
  nacc_var_groups <- shiny::reactiveVal()

  shiny::observe({
    if (!all(is.na(std_methods())) & !all(is.na(col_sel()))) {
      fin_dat(
        prepare_data(
          dat_sel$dat_obj(),
          methods = std_methods()
        )
      )

      ## Extract "domain" from all npsych_scores
      nacc_var_groups(
        unlist(
          lapply(
            setNames(ntrs::list_npsych_scores(), ntrs::list_npsych_scores()),
            \(x) {
              # match.fun(x)()@domain
              ntrs::get_npsych_scores(x)()@domain
            }
          )
        )
      )
    }
  }) |>
    shiny::bindEvent(
      col_sel(),
      std_methods(),
      ignoreNULL = T,
      ignoreInit = T
    )

  shiny::observe({
    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    shiny::showModal(
      shiny::modalDialog(
        title = "Columns Recognized",
        easy_close = TRUE,
        "Enough NACC columns were recognized automatically. To make
                custom selections, go to the 'Setup' tab.",
        footer = bslib::layout_columns(
          cols_widths = c(4, -4, 4),
          shiny::actionButton("goToColSelect", label = "Setup"),
          shiny::modalButton("Dismiss")
        )
      )
    )
  }) |>
    shiny::bindEvent(input$moveToTables)

  shiny::observe({
    shiny::removeModal()
    bslib::nav_select(id = "main_navbar", selected = "colSelect")
  }) |>
    shiny::bindEvent(input$goToColSelect)

  ## Once data has been readied the first time, move to 'Participant Data'
  ## and update options for study ID dropdown.
  study_id_choices <- shiny::reactiveVal()

  shiny::observe({
    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    new_choices <- unique(fin_dat()$NACCID)

    if (
      is.null(study_id_choices()) |
        any(!new_choices %in% study_id_choices()) |
        (!is.null(devmode()) &&
          (devmode() & is.null(names(study_id_choices()))))
    ) {
      study_id_choices(sort(unique(fin_dat()$NACCID)))

      cur_choices <- study_id_choices()

      if (!is.null(devmode()) && devmode()) {
        n_visits <- table(fin_dat()$NACCID)[cur_choices]
        names(cur_choices) <- paste0(names(n_visits), " (", n_visits, ")")
      }

      if (
        !is.null(input$current_studyid) & input$current_studyid %in% cur_choices
      ) {
        cur_select <- input$current_studyid
      } else {
        cur_select <- cur_choices[1]
      }

      shiny::updateSelectizeInput(
        session,
        "current_studyid",
        choices = cur_choices,
        selected = cur_select,
        server = TRUE
      )
    }
  }) |>
    shiny::bindEvent(
      fin_dat(),
      devmode(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

  ## Create demographics table
  output$demographics_table_output <- gt::render_gt({
    shiny::req(input$current_studyid)

    if (input$current_studyid %in% fin_dat()$NACCID) {
      demographics_table(
        subset(fin_dat(), fin_dat()$NACCID == input$current_studyid)
      )
    }
  })

  ## Update dropdown menu with visit dates when new study ID selected
  shiny::observe({
    dates <- fin_dat()$VISITDATE[fin_dat()$NACCID == input$current_studyid]

    sel_date <- NULL

    if (!is.null(selected_date()) && selected_date() %in% dates) {
      sel_date <- selected_date()
    }

    shiny::updateSelectizeInput(
      session,
      inputId = "current_date",
      choices = as.character(sort(unique(dates), decreasing = T)),
      selected = sel_date
    )

    selected_date(NULL)
  }) |>
    shiny::bindEvent(
      input$current_studyid,
      ignoreNULL = T,
      ignoreInit = T
    )

  ## When marker on one of the figures is clicked, input$update_date is set using session$sendCustomMessage (see plotVarModule.R)
  ## Here, we react to this event by setting the current date
  shiny::observe({
    shiny::req(input$update_date)

    shiny::updateSelectizeInput(
      session,
      inputId = "current_date",
      selected = input$update_date
    )
  })

  ## Get default descriptions if saved as option, otherwise set defaults
  default_descriptions <- getOption("ntrd.default_descriptions")
  if (is.null(default_descriptions)) {
    default_descriptions <- c(
      "Impaired" = 0.03,
      "Borderline" = 0.10,
      "Low Average" = 0.26,
      "Average" = 0.76,
      "High Average" = 0.92,
      "Superior" = 0.97,
      "Very Superior" = 1
    )
  }

  ## Get default colors if saved as option, otherwise set defaults
  default_fill_values <- getOption("ntrd.default_fill_values")
  if (is.null(default_fill_values)) {
    default_fill_values <- setNames(
      calc_fill_colors(n = length(default_descriptions)),
      nm = names(default_descriptions)
    )
  }

  ## Setup reactiveVal for descriptions with default values
  descriptions <- shiny::reactiveVal(
    value = default_descriptions
  )

  ## Setup reactiveVal for fill_values with default values
  fill_values <- shiny::reactiveVal(
    value = default_fill_values
  )

  ## Setup reactiveVal for table_font_size
  table_font_size <- shiny::reactiveVal(
    value = 80
  )

  ## Setup reactiveVal for shading
  shade_descriptions <- shiny::reactiveVal(
    value = TRUE
  )

  ## Server logic to let user modify description values and fill values.
  descriptions_and_fills <- descriptionsServer(
    id = "desc",
    default_descriptions = default_descriptions,
    default_fill_values = default_fill_values
  )

  ## Subset full data to the data needed for the main assessment table
  dat_for_table <- shiny::reactive({
    shiny::req(input$current_date)

    # Note: use data.table since `[[` doesn't preserve attributes, which we need
    # to infer std. methods. Can be replaced by using data.table.
    fin_dat()[
      fin_dat()$NACCID == input$current_studyid &
        fin_dat()$VISITDATE == input$current_date
    ]
  })

  mainTableServer(
    "main_table",
    dat = dat_for_table,
    table_font_size = table_font_size,
    descriptions = descriptions,
    fill_values = fill_values,
    methods = std_methods,
    include_caption = T,
    print_updating = F
  )

  #### Longitudinal Trends
  ## Subset full data to the data specific to input$current_studyid
  current_studyid_dat <- shiny::reactive({
    shiny::req(input$current_studyid)

    # Note: use data.table since `[[` doesn't preserve attributes, which we need
    # to infer std. methods. Can be replaced by using data.table.
    fin_dat()[
      fin_dat()$NACCID == input$current_studyid
    ]
  })

  ### Cognitive scores (Plots)
  ## Get x_range
  x_range <- shiny::reactiveVal()

  shiny::observe({
    shiny::req(fin_dat())

    x_range(date_range(fin_dat()$VISITDATE))
  }) |>
    shiny::bindEvent(
      fin_dat()
    )

  ## Get y-range's
  y_ranges <- shiny::reactiveValues()

  shiny::observe({
    lapply(unique(nacc_groups), \(cur_group) {
      ## Get variables in group corresponding
      cur_vars <- paste(
        "std",
        names(nacc_var_groups()[nacc_var_groups() == cur_group]),
        sep = "_"
      ) |>
        intersect(
          colnames(fin_dat())
        )

      y_ranges[[cur_group]] <- get_y_range(
        dat = fin_dat()[, cur_vars, with = F]
      )
    })
  }) |>
    shiny::bindEvent(
      fin_dat()
    )

  ## Plots UI
  output$plots_accordion <- shiny::renderUI({
    bslib::accordion(
      #!!!lapply(unique(nacc_var_groups()), \(x) plotUI(id = x)),
      !!!lapply(nacc_groups, \(x) plotUI(id = x)),
      id = "plots-accordion",
      open = TRUE
    )
  })

  ## Create all plots
  shiny::observe({
    # lapply(unique(nacc_var_groups()), \(x) {
    lapply(nacc_groups, \(x) {
      plotServer(
        x,
        dat = current_studyid_dat,
        x_range = x_range,
        y_range = shiny::reactive(y_ranges[[x]]),
        descriptions = descriptions,
        fill_values = fill_values,
        print_updating = T,
        shade_descriptions = shade_descriptions,
        new_id = x
      )
    })
  })

  ### Cognitive scores (Table)
  longTableServer(
    "long_table",
    dat = current_studyid_dat,
    methods = std_methods,
    table_font_size = table_font_size, # shiny::reactive(input$main_table_pct),
    fill_values = fill_values,
    descriptions = descriptions,
    print_updating = F
  )

  ## Diagnoses
  prevDiagnosesServer(
    "prev_diagnoses_table",
    dat = current_studyid_dat,
    table_font_size = table_font_size, # shiny::reactive(input$main_table_pct),
    print_updating = F
  )

  ## Biomarkers
  # To avoid R CMD check warnings:
  m <- NULL

  # base_query_file <- system.file(
  #   "json/panda_template.json",
  #   package = "ntrd"
  # )

  # all_values_et <- shiny::ExtendedTask$new(
  #   \(api) {
  #     m <<- mirai::mirai(
  #       {
  #         get_all_values(
  #           api_key = api,
  #           base_query_file = base_query_file #"inst/json/panda_template.json"
  #         )
  #       },
  #       .args = list(
  #         get_all_values = get_all_values,
  #         api = api,
  #         base_query_file = base_query_file
  #       )
  #     )

  #     m
  #   }
  # )

  # shiny::observe({
  #   shiny::req(biomarker_api())
  #   all_values_et$invoke(
  #     api = biomarker_api()
  #   )
  # })

  # biomarkerServer(
  #   "biomarker-tables",
  #   adrc_ptid = shiny::reactive(input$current_studyid),
  #   biomarker_api = shiny::reactive(
  #     dat_sel$extras()$biomarker_api
  #   ),
  #   all_values = shiny::reactive(dat_sel$extras()$all_values)
  # )

  shiny::observe({
    shiny::req(dat_sel$extras())
    shiny::req(input$current_studyid)

    if (!is.null(dat_sel$extras()$extension_server)) {
      dat_sel$extras()$extension_server(
        ptid = shiny::reactive(input$current_studyid),
        extras = dat_sel$extras
      )
    }
  })

  # } else {
  #   # If mirai is not installed, use simple reactiveValues
  #   biomarker_dat <- shiny::reactiveVal()
  #   biomarker_status <- shiny::reactiveVal("initiated")

  #   shiny::observe({
  #     biomarker_dat(
  #       get_biomarker_data(
  #         adrc_ptid = input$current_studyid,
  #         api_key = biomarker_api()
  #       )
  #     )

  #     biomarker_status("done")
  #   })

  #   biomarkerServer(
  #     "biomarker-tables",
  #     biomarker_data = biomarker_dat,
  #     biomarker_status = biomarker_status
  #   )
  # }

  ## Update reactiveVals for values chosen in Options pane.
  shiny::observe({
    bslib::accordion_panel_close(id = "options", values = TRUE)

    descriptions(
      descriptions_and_fills$descriptions()
    )

    fill_values(
      descriptions_and_fills$fill_values()
    )

    devmode(
      input$devmode
    )

    shade_descriptions(
      input$shade_descriptions
    )

    table_font_size(
      input$main_table_pct
    )

    ## Trigger a rerender of tables and plots by "poking" the input$current_studyid.
    ## First, get current date. This is used in the "update date".
    selected_date(input$current_date)

    session$sendCustomMessage(
      "setInputValue",
      list(
        inputId = "current_studyid",
        inputValue = input$current_studyid,
        priority = "event"
      )
    )
  }) |>
    shiny::bindEvent(
      input$update_colors
    )
}
