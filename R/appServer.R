#' Server Logic for shinyDashboard
#'
#' @param input,output,session Standard Shiny server arguments.
#'
#' @keywords internal
#'
#' @export
appServer <- function(input, output, session) {
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  ## Hide 'Participant Data' on startup
  bslib::nav_hide(id = "main_navbar", target = "colSelect")
  bslib::nav_hide(id = "main_navbar", target = "tables-and-figures")
  # bslib::nav_hide(id = "long-trends", target = "biomarkers")

  ## Setup data select module
  dat_sel <- dataSelectServer("dataSelect")

  ## Check for updates
  output$update_banner <- shiny::renderUI({
    shiny::req(dat_sel$update_info())

    update_info <- dat_sel$update_info()

    render_update_banner(
      result = update_info$result,
      package = update_info$package,
      action = shiny::actionButton(
        inputId = "do_update",
        label = "Update",
        class = "btn-sm btn-primary update-banner-action"
      )
    )
  })

  shiny::observe({
    info <- dat_sel$update_info()

    # Defensive: the button shouldn't render unless info has what we need,
    # but be safe — a stale click after a source switch is conceivable.
    if (!info$result@available || is.na(info$package)) {
      return()
    }

    pkg <- info$package

    shiny::showModal(shiny::modalDialog(
      title = "Update extension",
      shiny::tagList(
        shiny::tags$p(sprintf(
          "This will install version %s of %s, replacing the currently installed version %s.",
          info$result@latest,
          pkg,
          info$result@current
        )),
        shiny::tags$p(
          "The dashboard will close, R will restart twice (first to perform the update, ",
          "then to relaunch the dashboard), and the app will reopen automatically. ",
          "This typically takes 10\u201330 seconds."
        ),
        if (!is.na(info$result@news_url)) {
          shiny::tags$p(
            "Review the ",
            shiny::tags$a(
              "release notes",
              href = info$result@news_url,
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " before continuing."
          )
        }
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "confirm_update",
          "Update now",
          class = "btn-primary"
        )
      ),
      easyClose = TRUE
    ))
  }) |>
    shiny::bindEvent(input$do_update)

  shiny::observe({
    info <- dat_sel$update_info()
    pkg <- info$package

    if (is.na(pkg)) {
      shiny::removeModal()
      return()
    }

    if (!rstudioapi::isAvailable()) {
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Cannot update automatically",
        shiny::tags$p(
          "Automatic updates require RStudio. Please close the app and run ",
          "the following in your R console:"
        ),
        shiny::tags$pre(sprintf(
          'remotes::install_github("rmtrane/%s", upgrade = "always", dependencies = TRUE)',
          pkg
        )),
        easyClose = TRUE
      ))
      return()
    }

    shiny::removeModal()
    shiny::showNotification(
      sprintf(
        "Updating %s and restarting\u2026 your dashboard will reopen shortly.",
        pkg
      ),
      duration = NULL,
      type = "message",
      closeButton = FALSE
    )

    cmd <- build_update_restart_command(pkg)

    session$onSessionEnded(function() {
      rstudioapi::restartSession(command = cmd)
    })

    shiny::stopApp()
  }) |>
    shiny::bindEvent(input$confirm_update)

  ## Reactive values to store data object, selected data source, and data type,
  ## all assigned from dataSelect module. Also, reactive value to indicate if
  ## user should be allowed to select columns for variables. We only allow this
  ## for csv upload.

  devmode <- shiny::reactiveVal(value = FALSE)

  selected_date <- shiny::reactiveVal()

  shiny::observe({
    if (!is.null(dat_sel$extras()$extension_ui)) {
      ext_ui <- dat_sel$extras()$extension_ui() #id = "ext-module")

      if (inherits(ext_ui, "shiny.tag")) {
        ext_ui <- list(ext_ui)
      }

      for (nav_pan in rev(ext_ui)) {
        bslib::nav_insert(
          id = "long-trends",
          nav_pan,
          position = "after",
          target = "prev_diagnoses_table",
          select = FALSE
        )
      }
    }
  }) |>
    shiny::bindEvent(
      dat_sel$extras(),
      ignoreNULL = TRUE,
      once = TRUE
    )

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

  ## Reactive value to hold selected methods
  std_methods <- shiny::reactiveVal()

  ## When dat_obj changes, flush selected methods
  shiny::observe({
    std_methods(NA)
  }) |>
    shiny::bindEvent(
      dat_sel$dat_obj()
    )

  ## Select columns and domains
  methodSelectOutput <- methodSelectServer(
    "colSelect",
    dat_obj = dat_sel$dat_obj,
    default_methods = dat_sel$default_methods
  )

  shiny::observe({
    shiny::req(methodSelectOutput$std_methods())

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
  nacc_var_groups <- shiny::reactiveVal(value = nacc_var_groups)

  shiny::observe({
    shiny::req(dat_sel$dat_obj())
    shiny::req(!all(is.na(std_methods())))
    domain_asgn <- methodSelectOutput$domain_assignments()
    shiny::req(domain_asgn)

    dat_prepped <-
      prepare_data(
        dat_sel$dat_obj(),
        methods = std_methods()
      )

    npsych_subclass_map <- lapply(
      dat_prepped[, .SD, .SDcols = ntrs::is_npsych_scores],
      \(x) S7::S7_class(x)@name
    )

    for (x in names(domain_asgn)) {
      wh <- names(which(npsych_subclass_map == x))
      dat_prepped[[wh]]@domain <- domain_asgn[[x]]
    }

    fin_dat(dat_prepped)
  }) |>
    shiny::bindEvent(
      std_methods(),
      ignoreNULL = T,
      ignoreInit = T
    )

  shiny::observe({
    shiny::req(fin_dat())

    domain_asgn <- methodSelectOutput$domain_assignments()
    shiny::req(domain_asgn)

    dat_prepped <- fin_dat()

    npsych_subclass_map <- lapply(
      dat_prepped[, .SD, .SDcols = ntrs::is_npsych_scores],
      \(x) S7::S7_class(x)@name
    )

    for (x in names(domain_asgn)) {
      wh <- names(which(npsych_subclass_map == x))
      dat_prepped[[wh]]@domain <- domain_asgn[[x]]
    }

    fin_dat(dat_prepped)

    ## Use domain_assignments from the method select module
    nacc_var_groups(domain_asgn[!is.na(domain_asgn)])
  }) |>
    shiny::bindEvent(
      methodSelectOutput$domain_assignments(),
      ignoreNULL = T,
      ignoreInit = T
    )

  ## Show notification when defaults were auto-applied and main view is ready
  shiny::observe({
    shiny::req(fin_dat())
    shiny::req(methodSelectOutput$auto_applied())

    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    shiny::showNotification(
      "Default methods applied. To customize, go to the Setup tab.",
      type = "message",
      duration = 8
    )
  }) |>
    shiny::bindEvent(fin_dat(), once = TRUE)

  ## Once data has been readied the first time, move to 'Participant Data'
  ## and update options for study ID dropdown.
  study_id_choices <- shiny::reactiveVal()

  shiny::observe({
    ## Move to tab
    bslib::nav_show(
      id = "main_navbar",
      target = "tables-and-figures",
      select = T
    )

    ## Get all NACCIDs
    new_choices <- unique(fin_dat()$NACCID)

    if (
      ## If no id choices...
      is.null(study_id_choices()) |
        # ... or any new_choices not already in study_id_choices
        any(!new_choices %in% study_id_choices()) |
        # ... or in devmode and study_id_choices without names.
        (!is.null(devmode()) &&
          (devmode() & is.null(names(study_id_choices()))))
    ) {
      study_id_choices(sort(unique(fin_dat()$NACCID)))

      cur_choices <- study_id_choices()

      ## If in devmode...
      if (!is.null(devmode()) && devmode()) {
        ## ... add number of visits to names of study id choices
        n_visits <- table(fin_dat()$NACCID)[cur_choices]
        names(cur_choices) <- paste0(names(n_visits), " (", n_visits, ")")
      }

      ## If this is triggered after being initialized and a study id is already chosen...
      ## (this would happen if devmode is toggled in app)
      if (
        !is.null(input$current_studyid) & input$current_studyid %in% cur_choices
      ) {
        ## ... save the current choice
        cur_select <- input$current_studyid
      } else {
        ## Else select first choice
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

  ## Create gated current_studyid
  current_studyid <- shiny::reactive({
    shiny::req(
      input$current_studyid %in% fin_dat()$NACCID
    )

    input$current_studyid
  })

  ## Create demographics table
  output$demographics_table_output <- gt::render_gt({
    shiny::req(current_studyid())

    demographics_table(
      subset(fin_dat(), fin_dat()$NACCID == current_studyid())
    )
  })

  ## Update dropdown menu with visit dates when new study ID selected
  shiny::observe({
    shiny::req(current_studyid())

    dates <- fin_dat()$VISITDATE[fin_dat()$NACCID == current_studyid()]

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
      current_studyid(),
      ignoreNULL = T,
      ignoreInit = F
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
      fin_dat()$NACCID == current_studyid() &
        fin_dat()$VISITDATE == input$current_date
    ]
  })

  mainTableServer(
    "main_table",
    dat = dat_for_table,
    table_font_size = table_font_size,
    descriptions = descriptions,
    fill_values = fill_values,
    include_caption = T,
    print_updating = F
  )

  #### Longitudinal Trends
  ## Subset full data to the data specific to current_studyid()
  current_studyid_dat <- shiny::reactive({
    shiny::req(current_studyid())

    # Note: use data.table since `[[` doesn't preserve attributes, which we need
    # to infer std. methods. Can be replaced by using data.table.
    fin_dat()[
      fin_dat()$NACCID == current_studyid()
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
        new_id = x,
        var_groups = nacc_var_groups
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

  ## Extension
  extension_server_initialized <- shiny::reactiveVal(FALSE)

  shiny::observe({
    shiny::req(dat_sel$extras())
    shiny::req(!extension_server_initialized())
    shiny::req(fin_dat())

    if (!is.null(dat_sel$extras()$extension_server)) {
      dat_sel$extras()$extension_server(
        # id = "ext-module",
        ptid = current_studyid, #shiny::reactive(input$current_studyid),
        # dat = fin_dat,
        extras = dat_sel$extras
      )

      extension_server_initialized(TRUE)
    }
  })

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
        inputValue = current_studyid(),
        priority = "event"
      )
    )
  }) |>
    shiny::bindEvent(input$update_colors)
}
