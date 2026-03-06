#' Descriptions UI
#'
#' @description
#' UI to let the user select the groups used for coloring. Includes a table where
#' group labels can be specified, cutoffs set, and color selected.
#'
#' @param id A string used to namespace the module.
#'
#' @rdname descriptionsModule
#'
#' @returns
#' A UI definition.
#'
#' @export
descriptionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(
      ns("descriptions")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::actionButton(
        inputId = ns("add_row"),
        label = "+"
      ),
      shiny::actionButton(
        inputId = ns("reset"),
        label = "Reset"
      )
    )
  )
}

#' Descriptions Server
#'
#' @description
#' A short description...
#'
#' @param id A string.
#' @param default_descriptions Optional. A named numeric vector with names for `Label` and values for `Upper_Bound`.
#' @param default_fill_values Optional. A named character vector with names for `Label` and values for colors (as HEX values)
#'
#' @returns
#' A list containing `fill_values` and `descriptions`.
#'
#' @rdname descriptionsModule
#'
#' @export
descriptionsServer <- function(
  id,
  default_descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  default_fill_values = NULL
) {
  if (!is.numeric(default_descriptions)) {
    cli::cli_abort(
      "{.arg default_descriptions} must be of class `numeric` (got {class(default_descriptions)})"
    )
  }

  if (is.null(names(default_descriptions))) {
    cli::cli_abort(
      "{.arg default_descriptions} must be named."
    )
  }

  if (is.null(default_fill_values)) {
    default_fill_values <- setNames(
      calc_fill_colors(n = length(default_descriptions)),
      nm = names(default_descriptions)
    )
  }

  if (!check_colors(default_fill_values)) {
    cli::cli_abort(
      "{.arg default_fill_values} contains entries that are not recognized as hex values."
    )
  }

  if (length(default_fill_values) != length(default_descriptions)) {
    cli::cli_abort(
      "Length of {.arg default_descriptions} ({length(default_descriptions)}) must be the same as length of {.arg default_fill_values} ({length(default_fill_values)})"
    )
  }

  if (
    !identical(
      sort(names(default_fill_values)),
      sort(names(default_descriptions))
    )
  ) {
    cli::cli_abort(
      "The names of {.arg default_descriptions} ({names(default_descriptions)}) must be the same as the names of {.arg default_fill_values} ({names(default_fill_values)})"
    )
  }

  shiny::moduleServer(id, function(input, output, session) {
    init_n <- length(default_fill_values)

    default_descriptions <- data.frame(
      rowId = 1:init_n,
      Label = names(default_descriptions),
      Upper_Bound = default_descriptions * 100,
      col = calc_fill_colors(init_n),
      Color = paste0(
        '<input type="color" id="color',
        1:init_n,
        '" value = "',
        calc_fill_colors(init_n),
        '">'
      ),
      Remove = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation"></i>'
    )

    descriptions <- shiny::reactiveVal(
      default_descriptions
    )

    output$descriptions <- DT::renderDataTable({
      DT::datatable(
        default_descriptions,
        colnames = c(
          "Upper Bound" = "Upper_Bound",
          " " = "Remove"
        ),
        options = list(
          ordering = F,
          dom = "t",
          columnDefs = list(
            list(
              targets = which(colnames(default_descriptions) == "rowId") - 1,
              visible = F
            ),
            list(
              targets = which(colnames(default_descriptions) == "Upper_Bound") -
                1,
              render = DT::JS(
                'function(data, type, row, meta) {
                  return data + `%`
                }'
              ),
              width = "125px"
            ),
            list(
              targets = which(colnames(default_descriptions) == "col") - 1,
              visible = F
            ),
            list(
              targets = which(colnames(default_descriptions) == "Color") - 1,
              width = "50px"
            ),
            list(
              targets = which(colnames(default_descriptions) == "Remove") - 1,
              width = "15px"
            )
          ),
          preDrawCallback = DT::JS(
            'function() { Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = DT::JS(
            paste0(
              'function() {
                Shiny.bindAll(this.api().table().node());
                addEventListenersToColors("',
              id,
              '");
              }'
            )
          )
        ),
        rownames = F,
        editable = list(
          target = "cell",
          disable = list(
            columns = which(
              !colnames(default_descriptions) %in% c("Upper_Bound", "Label")
            ) -
              1
          )
        ),
        selection = "none",
        escape = F
      )
    })

    descriptions_proxy <- DT::dataTableProxy("descriptions")

    shiny::observe({
      descriptions(default_descriptions)

      DT::replaceData(
        descriptions_proxy,
        data = default_descriptions,
        rownames = F
      )
    }) |>
      shiny::bindEvent(input$reset)

    shiny::observe({
      tmp <- descriptions()

      # message(input$newColorPicked)
      # message(input$color1)

      tmp$col[paste0("color", tmp$rowId) == input$newColorPicked] <- input[[
        input$newColorPicked
      ]]

      # message(paste(
      #   input$newColorPicked,
      #   "was updated to",
      #   input[[input$newColorPicked]]
      # ))

      descriptions(tmp)
    }) |>
      shiny::bindEvent(input$newColorPicked)

    shiny::observe({
      shiny::req(input$descriptions_cell_clicked$col)

      ## as.numeric necessary for the shinytest2 tests to work
      row <- as.numeric(input$descriptions_cell_clicked[["row"]])
      col <- as.numeric(input$descriptions_cell_clicked[["col"]])

      remove_col <- which(colnames(descriptions()) == "Remove")

      if (col == remove_col - 1) {
        tmp <- descriptions()
        tmp <- tmp[-row, ]

        if (max(tmp$Upper_Bound) < 100) {
          tmp$Upper_Bound[which.max(tmp$Upper_Bound)] <- 100
        }

        DT::replaceData(descriptions_proxy, data = tmp, rownames = F)

        descriptions(tmp)
      }
    }) |>
      shiny::bindEvent(
        input$descriptions_cell_clicked
      )

    shiny::observe({
      ## This is for the shinytest2 tests to work
      desc_cell_edit <- input$descriptions_cell_edit
      desc_cell_edit$row <- as.numeric(desc_cell_edit$row)
      desc_cell_edit$col <- as.numeric(desc_cell_edit$col)

      tmp <- DT::editData(
        data = descriptions(),
        info = desc_cell_edit,
        rownames = F
      )

      tmp$Color <- paste0(
        '<input type="color" id="color-',
        tmp$rowId,
        '" value = "',
        tmp$col,
        '">'
      )

      tmp <- tmp[order(tmp$Upper_Bound), ]

      DT::replaceData(
        descriptions_proxy,
        tmp,
        rownames = F
      )

      descriptions(tmp)
    }) |>
      shiny::bindEvent(
        input$descriptions_cell_edit
      )

    shiny::observe({
      tmp <- rbind(
        descriptions(),
        data.frame(
          "rowId" = max(descriptions()$rowId) + 1,
          "Label" = "New Group",
          "Upper_Bound" = Inf,
          "Color" = paste0(
            '<input type="color" id="color-',
            max(descriptions()$rowId) + 1,
            '" value = "#ffffff">'
          ),
          "col" = "#ffffff",
          "Remove" = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation"></i>'
        )
      )

      tmp$Color <- paste0(
        '<input type="color" id="color-',
        tmp$rowId,
        '" value = "',
        tmp$col,
        '">'
      )

      DT::replaceData(descriptions_proxy, data = tmp, rownames = FALSE)

      descriptions(tmp)
    }) |>
      shiny::bindEvent(
        input$add_row
      )

    desc_out <- shiny::reactiveVal()
    fill_out <- shiny::reactiveVal()

    shiny::observe({
      desc_out(with(descriptions(), setNames(Upper_Bound / 100, Label)))
      fill_out(with(descriptions(), setNames(col, Label)))
    })

    return(
      list(
        fill_values = fill_out,
        descriptions = desc_out
      )
    )
  })
}

#' Descriptions App
#'
#' @rdname descriptionsModule
#'
#' @param testing Logical, whether to run the app in testing mode.
#'
#' @export
descriptionsApp <- function(
  default_descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  testing = FALSE
) {
  development <- dir.exists("inst/www") &&
    basename(getwd()) == "ntrd"

  if (development) {
    print("Development...")
    www_path <- "inst/www"
    qmd_path <- "inst/qmd"
  } else {
    require("ntrd")

    www_path <- system.file("www", package = "ntrd")
    qmd_path <- system.file("qmd", package = "ntrd")
  }

  shiny::addResourcePath("www", www_path)
  shiny::addResourcePath("qmd", qmd_path)

  ui <- bslib::page_fluid(
    shiny::tags$head(
      shiny::tags$script(
        src = "www/scripts.js"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "www/styles.css"
      )
    ),
    shiny::tags$div(id = "spinner", class = "loader"),
    shiny::tags$div(id = "spinner_overlay", class = "loader_overlay"),
    descriptionsUI("desc"),
    shiny::verbatimTextOutput("fill_out"),
    shiny::verbatimTextOutput("cur_fill_out"),
    shiny::tableOutput("cur_desc")
  )

  server <- function(input, output, session) {
    desc <- descriptionsServer(
      "desc",
      default_descriptions = default_descriptions
    )

    fill_values <- shiny::reactiveVal()

    shiny::observe({
      fill_values(desc$fill_values())
    })

    output$fill_out <- shiny::renderText({
      paste(
        "Default colors:",
        paste(calc_fill_colors(length(default_descriptions)), collapse = ", ")
      )
    })

    output$cur_fill_out <- shiny::renderText({
      paste("Current colors:", paste(fill_values(), collapse = ", "))
    })

    output$cur_desc <- shiny::renderTable(
      data.frame(
        labels = names(desc$descriptions()),
        values = desc$descriptions()
      ),
      rownames = FALSE
    )

    shiny::exportTestValues(
      fill_values = fill_values(),
      descriptions = desc$descriptions()
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
