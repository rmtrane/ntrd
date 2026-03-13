#' Plot Module
#'
#' @description
#' UI for displaying plots of standardized scores across visits.
#'
#' @param id A single string to ID the Shiny module, which also indicates
#' which group of variables to plot. Should be one of `unique(nacc_var_groups)`.
#'
#' @returns
#' A `shiny::tagList` object containing two `shiny::conditionalPanel`'s:
#' one with the plot, and one with text for when no standardized scores
#' were found. Both wrapped in a `bslib::accordion_panel` for inclusion in
#' main app.
#'
#' @rdname plotModule
#'
#' @export
plotUI <- function(id) {
  if (!id %in% nacc_groups) {
    cli::cli_abort(
      "{.arg id} should be one of {.var {nacc_groups}}, but is {.var {id}}"
    )
  }

  ##################
  ## CREATE TITLE

  tooltip_text <- FALSE

  if (id == "General Cognition") {
    tooltip_text <- "When both MoCA and MMSE are available, these are plotted as one line, but with different markers. Hover markers to see details."
  }

  if (id == "Attention/Processing") {
    tooltip_text <- shiny::HTML(
      "When both of the following pairs of scores are present, these are plotted as one line, but with different markers. Hover markers to see details.<br>
        - Digit Span Forward <> Number Span Forward.<br>
        - Digit Span Backward <> Number Span Backward."
    )
  }

  if (id == "Memory") {
    tooltip_text <- shiny::HTML(
      "When both of the following pairs of scores are present, these are plotted as one line, but with different markers. Hover markers to see details.<br>
        - Logical Memory IA, Immediate <> Craft Immediate - Paraphrase.<br>
        - Logical Memory IIA, Delayed <> Craft Delay - Paraphrase."
    )
  }

  if (id == "Language") {
    tooltip_text <- "When both Boston Naming Test and MINT are available, these are plotted as one line, but with different markers. Hover markers to see details."
  }

  ## If the tooltip_text is not still FALSE, we create the `cardtitle` with a tooltip.
  if (!isFALSE(tooltip_text)) {
    cardtitle <- bslib::tooltip(
      shiny::strong(
        id,
        .noWS = "outside"
      ) |>
        shiny::tagAppendAttributes(
          style = "text-decoration-line: underline; text-decoration-style: dotted; overflow-y: hidden;"
        ),
      tooltip_text,
      options = list(customClass = "my-tooltip")
    )
  } else {
    ## If `cardtitle` is still FALSE, we create `cardtitle` with only id, no tooltip.
    cardtitle <- shiny::strong(id, .noWS = "outside") |>
      shiny::tagAppendAttributes(style = "overflow-y: hidden;")
  }

  ## Output. tagList with two `conditionalPanel`'s handling the cases where plot should
  ## be shown and hidden, respectively.
  shiny::tagList(
    shiny::conditionalPanel(
      "input.showPlot == 'yes'",
      ns = shiny::NS(id),
      bslib::accordion_panel(
        title = cardtitle,
        bslib::card_body(
          plotly::plotlyOutput(
            shiny::NS(id, "plot"),
            width = "100%"
          )
        ),
        value = id
      )
    ),
    shiny::conditionalPanel(
      "input.showPlot == 'no'",
      ns = shiny::NS(id),
      # bslib::card(
      bslib::accordion_panel(
        title = cardtitle,
        bslib::card_body(
          shiny::p("No standardized scores found")
        ),
        value = id
      )
    )
  )
}

#' @description
#' Server logic to handle the creation and updating of plots to show standardized scores across visits.
#'
#' @param id A single string. Optional, defaults to `"Attention/Processing"`. Should be one of `nacc_var_groups`.
#' @param dat A data.table. Should be participant specific.
#' @param x_range A date range as a character vector with two elements.
#' @param y_range A numeric vector with two elements. Optional, defaults to `c(-2.5, 2.5)`.
#' @param descriptions A named numeric vector. Optional, defaults to the vector described in the function definition.
#' @param fill_values A vector of fill colors. Optional, defaults to `calc_fill_colors(n = 7)`.
#' @param print_updating A logical value. Optional, defaults to `TRUE`.
#' @param shade_descriptions A logical value indicating if the plots should be shaded according to the regions given by `descriptions` with colors given by `fill_values`. Optional, defaults to `TRUE`.
#' @param new_id Optional. String to be used for table ID. If `NULL` (default), random string assigned.
#'
#' @returns
#' No return value.
#'
#' @rdname plotModule
#'
#' @export
plotServer <- function(
  id = "Attention/Processing",
  dat,
  x_range = c("2016-05-01", "2020-08-01"),
  y_range = c(-2.5, 2.5),
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values = calc_fill_colors(n = 7),
  print_updating = T,
  shade_descriptions = TRUE,
  new_id = NULL
) {
  if (!id %in% nacc_groups) {
    cli::cli_abort(
      "{.arg id} should be one of {.var {nacc_groups}}, but is {.var {id}}"
    )
  }

  #################
  ## BEFORE SERVER
  ##
  ## Make sure everything that should be a reactive is a reactive
  if (!shiny::is.reactive(descriptions)) {
    descriptions <- shiny::reactiveVal(descriptions)
  }

  if (!shiny::is.reactive(fill_values)) {
    fill_values <- shiny::reactiveVal(fill_values)
  }

  if (!shiny::is.reactive(shade_descriptions)) {
    shade_descriptions <- shiny::reactiveVal(shade_descriptions)
  }

  if (!shiny::is.reactive(x_range)) {
    shade_descriptions <- shiny::reactiveVal(x_range)
  }

  if (!shiny::is.reactive(y_range)) {
    shade_descriptions <- shiny::reactiveVal(y_range)
  }

  ## Counter to include in cli_alert_info
  base_plot_drawing_counter <- shiny::reactiveVal(0)

  ## Variables in this group
  cur_vars <- names(nacc_var_groups[nacc_var_groups == id])

  #### Colors to use for lines and markers.
  ## We give crosswalk pairs same color.
  ## First, non-legacy scores.
  non_crosswalks <- setdiff(cur_vars, names(crosswalk_translations))
  cog_vars_colors <- setNames(
    rep(
      c(
        "#A6CEE3",
        "#1F78B4",
        "#B2DF8A",
        "#33A02C",
        "#FB9A99",
        # "#E31A1C",
        "#FDBF6F",
        "#FF7F00",
        "#CAB2D6",
        "#6A3D9A",
        "#B15928"
      ),
      length.out = length(non_crosswalks)
    ),
    nm = non_crosswalks
  )

  ## Next, add in legacy scores such that colors match.
  crosswalks <- intersect(cur_vars, names(crosswalk_translations))
  cog_vars_colors <- c(
    cog_vars_colors,
    setNames(
      cog_vars_colors[crosswalk_translations[crosswalks]],
      nm = crosswalks
    )
  )

  ## Legend names. Make reactive, since we might need to adjust if any crosswalk pairs are present
  legend_names <- shiny::reactiveVal(nacc_var_labels[cur_vars])

  ## Create visibility vector indicating if the traces are visible or not
  visibility <- do.call(
    shiny::reactiveValues,
    visibility_defaults
  )

  ##
  ###################

  ###################
  ## START SERVER
  shiny::moduleServer(id, function(input, output, session) {
    ## Set input$showPlot = "yes" so that plot is shown in UI
    session$sendCustomMessage(
      "setInputValue",
      message = list(
        inputId = paste(id, "showPlot", sep = "-"),
        inputValue = 'yes'
      )
    )
    ###################
    ## CREATE WORKING DATA
    ##
    ## Limit data to relevant columns. This could maybe help to not replot too frequently.
    cur_studyid_dat <- shiny::reactive({
      cur_vars <- names(nacc_var_groups[nacc_var_groups == id])
      cur_vars_in_dat <- intersect(
        c(paste0("std_", cur_vars), paste0("raw_", cur_vars)),
        colnames(dat())
      )

      dat()[, c("VISITDATE", cur_vars_in_dat), with = F]
    })

    ###################
    ## INITIATE PLOT
    output$plot <- plotly::renderPlotly({
      if (print_updating) {
        shiny::isolate({
          base_plot_drawing_counter(base_plot_drawing_counter() + 1)
        })

        cli::cli_alert_info(
          "Base plot for group {.var {id}} updating (done {base_plot_drawing_counter()} time{?s})"
        )
      }

      ## Base plot
      base_plot_z_scores(
        x_range = x_range(),
        y_range = y_range(),
        descriptions = descriptions(),
        fill_values = fill_values(),
        shade_descriptions = T,
        fill_alpha = 0.2,
        source = id,
        new_id = new_id
      ) |>
        ## Make sure layout is as desired
        plotly::layout(
          showlegend = TRUE,
          hoverlabel = list(align = "left"),
          legend = list(
            traceorder = "normal"
          ),
          xaxis = list(
            title = ""
          )
        ) |>
        ## Register events on click and double click in legend
        plotly::event_register(event = "plotly_click") |>
        plotly::event_register(event = "plotly_legendclick") |>
        plotly::event_register(event = "plotly_legenddoubleclick") |>
        ## Add JS function to run on render
        onRender(
          "function(el, x, input) {
            // Function to run when plot is plotted (including after restyling and relayout)
            el.on('plotly_afterplot', function () {
              afterPlot(x, input);
            });

            // Create input$base_plot_drawn to indicate base plots have been created. 
            // Include priority: 'event' to fire every time
            Shiny.setInputValue(input.ns + '-base_plot_drawn', 1, {priority: 'event'});
          }",
          data = list(
            name = "TraceMapping",
            ns = id
          )
        )
    }) |>
      shiny::bindEvent(
        x_range(),
        y_range(),
        descriptions(),
        fill_values()
      )

    shiny::observe({
      # Don't trigger until plot has been drawn
      shiny::req(input$base_plot_drawn)

      d <- plotly::event_data("plotly_click", source = id)

      if (!is.null(d) && !is.null(d$x)) {
        # shiny::showNotification(shiny::h2(d$x))

        session$sendCustomMessage("updateDate", message = list(value = d$x))
      }
    })

    ## Here we update the reactiveValues 'visibility'
    ## For all names in 'visibility_defaults', we want
    ## to listen for changes to input$`name`_visibility,
    ## since this is updated when visibility is toggled.
    for (nam in names(visibility_defaults)) {
      ## We use the rlang::inject to avoid environment weirdness.
      ## Without this, only the last observer is created.
      rlang::inject({
        shiny::observe({
          visibility[[!!nam]] <- input[[paste(
            !!nam,
            "visibility",
            sep = "_"
          )]]

          ## Create new var to pass to cli
          var_nm <- !!nam
          cli::cli_alert_info("visibility updated for {var_nm}")
        }) |>
          shiny::bindEvent(
            input[[paste(!!nam, "visibility", sep = "_")]]
          )
      })
    }

    ###################
    ## LINES AND MARKERS
    shiny::observe({
      shiny::req(cur_studyid_dat(), input$base_plot_drawn)

      if (print_updating) {
        cli::cli_alert_info("Updating plots.")
      }

      # Remove old traces if any. We can tell by checking input$TraceMapping. This is
      # set every time plot is rerendered. See the javascript in 'onRender' for the plot.
      if (!is.null(input$TraceMapping)) {
        ## Create matrix from input$TraceMapping
        traces <- matrix(
          input$TraceMapping,
          ncol = 4,
          byrow = TRUE
        )

        ## Get trace indices corresponding to traces in legend or with tracename "no_values" or including "/" (which indicates combined scores)
        indices <- as.integer(traces[
          traces[, 1] %in%
            c(legend_names(), "no_values") |
            grepl(pattern = "/", x = traces[, 1]),
          2
        ])

        ## Remove traces
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke("deleteTraces", indices)

        if (print_updating) {
          cli::cli_alert_info(
            "Old traces removed for {.var {id}}"
          )
        }
      } else {
        if (print_updating) {
          cli::cli_alert_info(
            "No traces to remove for {.var {id}}"
          )
        }
      }

      ## Get list of lists with new traces (lines and markers). Also returns
      # new x_range, new y_range, and updated legend_names. If no std_* columns
      # in data, will be empty.
      new_traces <- plotly_new_traces(
        new_dat = cur_studyid_dat(),
        visibility = shiny::reactiveValuesToList(visibility),
        legend_names = legend_names(),
        vars_colors = cog_vars_colors[cur_vars]
      )

      ## If no new traces...
      if (!"new_traces" %in% names(new_traces)) {
        # ... set input$showPlot to "no"
        session$sendCustomMessage(
          "setInputValue",
          message = list(
            inputId = paste(id, "showPlot", sep = "-"),
            inputValue = 'no'
          )
        )

        ## Hide accordion_panel
        session$sendCustomMessage(
          "accordionPanelToggle",
          message = list(id = id, action = "hide")
        )
      } else {
        ## Update legend_names
        legend_names(new_traces$legend_names)

        ## Add traces
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            method = "addTraces",
            new_traces$new_traces
          )

        ## Set input$showPlot = "yes" so that plot is shown in UI
        session$sendCustomMessage(
          "setInputValue",
          message = list(
            inputId = paste(id, "showPlot", sep = "-"),
            inputValue = 'yes'
          )
        )

        ## Show accordion_panel
        session$sendCustomMessage(
          "accordionPanelToggle",
          message = list(id = id, action = "show")
        )

        ## Relayout: update axes, and set showlegend = TRUE in case
        # all traces are hidden.
        plotly::plotlyProxy("plot", session) |>
          # plotly::plotlyProxyInvoke(
          #   method = "reconfig",
          #   staticPlot = FALSE
          # ) |>

          plotly::plotlyProxyInvoke(
            method = "relayout",
            list(
              xaxis = list(
                automargin = TRUE,
                range = new_traces$new_x_range, # date_range(new_dat$VISITDATE),
                minallowed = x_range()[1],
                maxallowed = x_range()[2]
              ),
              yaxis = list(
                title = "z-score",
                y = as.list(0),
                yanchor = "bottom",
                yref = "container",
                minallowed = y_range()[1],
                maxallowed = y_range()[2],
                range = c(
                  min(-2.5, new_traces$new_y_range[1]),
                  max(2.5, new_traces$new_y_range[2])
                ) *
                  1.02
              ),
              showlegend = TRUE
            )
          )
      }
    }) |>
      shiny::bindEvent(
        cur_studyid_dat(),
        input$base_plot_drawn,
        ignoreInit = F
      )
  })
}

#' Plot app
#'
#' @description
#' Shiny app using the plotUI and plotServer.
#'
#' @param dat_input A data frame. Defaults to `prepare_data(demo_data)`.
#' @param studyids Optional. If `NULL` (default), will use unique values from `dat_input$NACCID`.
#' @param testing Logical, whether to run the app in testing mode.
#'
#' @returns
#' A shiny app.
#'
#' @rdname plotModule
#'
#' @export
plotApp <- function(
  dat_input = prepare_data(demo_data),
  studyids = NULL,
  testing = FALSE
) {
  shinyAddResources()

  ui <- bslib::page_sidebar(
    theme = bslib::bs_theme(
      version = 5
    ) |>
      bslib::bs_add_rules(
        ".my-tooltip .tooltip-inner {
          min-width: 500px;
          text-align: left;
        }"
      ),
    sidebar = bslib::sidebar(
      shiny::selectizeInput(
        inputId = "studyid",
        label = "Study ID",
        choices = NULL,
        selected = NULL
      )
    ),
    shinyApp_header(),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        id = "main-table",
        full_screen = T,
        bslib::card_header(
          "NACC T-Cog Neuropsychological Assessment Summary Table"
        ),
        bslib::card_body(
          shiny::selectInput(
            inputId = "current_date",
            label = "Date",
            choices = NULL
          ),
          mainTableUI("main_table")
          # verbatimTextOutput(NS("plot_cog_var", "click"))
          # verbatimTextOutput(NS("plot_cog_var", "PrintTraceMapping"))
        )
      ),
      bslib::card(
        id = "main-plot",
        full_screen = T,
        bslib::card_header("Longitudinal Trends"),
        bslib::accordion(
          !!!lapply(unique(nacc_groups), \(x) plotUI(id = x)),
          open = TRUE,
          id = "plots-accordion"
        )
      )
    )
  )

  server <- function(input, output, session) {
    if (!shiny::is.reactive(dat_input)) {
      cli::cli_alert_info("{.arg dat_input} converting to reactive")
      dat <- shiny::reactive(dat_input)
    } else {
      dat <- dat_input
    }

    shiny::observe({
      shiny::req(dat)

      if (!shiny::is.reactive(dat)) {
        return()
      }

      if (is.null(studyids)) {
        studyids <- unique(dat()$NACCID)
      }

      shiny::updateSelectizeInput(
        inputId = "studyid",
        choices = studyids,
        server = TRUE
      )
    })

    shiny::observe({
      shiny::req(input$studyid, dat)

      shiny::updateSelectInput(
        inputId = "current_date",
        choices = dat()$VISITDATE[dat()$NACCID == input$studyid]
      )
    }) |>
      shiny::bindEvent(input$studyid)

    ## Subset full data to the data specific to input$current_studyid
    current_studyid_dat <- shiny::reactive({
      shiny::req(input$studyid)

      # Note: use data.table since `[[` doesn't preserve attributes, which we need
      # to infer std. methods. Can be replaced by using data.table.
      dat()[dat()$NACCID == input$studyid]
    })

    ### Cognitive scores (Plots)
    ## Get x_range's and y_range's

    ## Get date range
    x_range <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(dat())

      x_range(date_range(dat()$VISITDATE))
    }) |>
      shiny::bindEvent(
        dat()
      )

    ## Get y-range's
    y_ranges <- shiny::reactiveValues()

    shiny::observe({
      lapply(nacc_groups, \(cur_group) {
        ## Get variables in group corresponding
        cur_vars <- paste(
          "std",
          names(nacc_var_groups[nacc_var_groups == cur_group]),
          sep = "_"
        ) |>
          intersect(
            colnames(dat())
          )

        y_ranges[[cur_group]] <- get_y_range(
          dat = dat()[, cur_vars, with = F]
        )
      })
    }) |>
      shiny::bindEvent(
        dat()
      )

    shiny::observe({
      cli::cli_alert_info("{.var x_range} updated")
    }) |>
      shiny::bindEvent(
        x_range()
      )

    shiny::observe({
      cli::cli_alert_info("{.var y_ranges} updated")
    }) |>
      shiny::bindEvent(
        y_ranges
      )

    descriptions <- shiny::reactiveVal(c(
      "Impaired" = 0.03,
      "Borderline" = 0.10,
      "Low Average" = 0.26,
      "Average" = 0.76,
      "High Average" = 0.92,
      "Superior" = 0.97,
      "Very Superior" = 1
    ))

    fill_values <- shiny::reactiveVal(calc_fill_colors(7))

    shade_descriptions <- shiny::reactiveVal(TRUE)

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

    # mainTableServer(
    #   "main_table",
    #   dat = shiny::reactive(
    #     current_studyid_dat()[
    #       current_studyid_dat()$VISITDATE == input$test_date
    #     ]
    #   ),
    #   descriptions = c(
    #     "Impaired" = 0.03,
    #     "Borderline" = 0.10,
    #     "Low Average" = 0.26,
    #     "Average" = 0.76,
    #     "High Average" = 0.92,
    #     "Superior" = 0.97,
    #     "Very Superior" = 1
    #   ),
    #   fill_values = calc_fill_colors(7),
    #   methods = "infer",
    #   table_font_size = shiny::reactive(80)
    # )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
