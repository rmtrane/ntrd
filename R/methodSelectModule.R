#' Shiny Module for Selecting Domains and Standardization Methods
#'
#' This shiny module displays npsych_scores variables from the loaded data
#' and lets the user assign cognitive domains and standardization methods
#' using dropdown menus in an HTML table.
#'
#' @param id id to link shiny modules
#'
#' @rdname methodSelectModule
#'
#' @export
methodSelectUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(
      ns("assign"),
      label = "Apply Selections"
    ),
    shiny::uiOutput(ns("vars_table_output"))
  )
}

#' @rdname methodSelectModule
#'
#' @param id id to link shiny modules
#' @param dat_obj reactive data object containing the loaded data.
#' @param default_methods named list. Each entry should be named after a
#'   variable. The entry should be a named character vector with two elements:
#'   one named 'method' to indicate the standardization method to use, and one
#'   named 'version' to indicate the version to use for standardization.
#'
#' @export
methodSelectServer <- function(
  id,
  dat_obj,
  default_methods
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Detect npsych_scores columns from the input data
    npsych_vars <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(dat_obj())
      dat <- dat_obj()@data

      is_npsych <- vapply(
        colnames(dat),
        \(col) ntrs::is_npsych_scores(dat[[col]]),
        logical(1)
      )
      npsych_vars(sort(names(is_npsych[is_npsych])))
    })

    ## Precompute method choices per variable
    method_choices_map <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(npsych_vars())

      choices_map <- lapply(
        setNames(npsych_vars(), npsych_vars()),
        \(x) {
          X <- ntrs::get_npsych_scores(x)()
          methods_available <- ntrs::list_std_methods(X)
          if (length(methods_available) == 0) {
            return(character(0))
          }

          unlist(lapply(methods_available, \(mth) {
            mth_versions <- ntrs::list_method_versions(X, mth)
            do.call(
              paste0,
              c(
                list(mth),
                if (length(mth_versions) > 0) list(" (", mth_versions, ")")
              )
            )
          }))
        }
      )

      method_choices_map(choices_map)
    }) |>
      shiny::bindEvent(npsych_vars())

    ## Render the HTML table with real selectizeInputs
    output$vars_table_output <- shiny::renderUI({
      shiny::req(npsych_vars())
      shiny::req(method_choices_map())

      all_vars <- npsych_vars()
      dat <- dat_obj()@data
      choices_map <- method_choices_map()
      domain_choices <- c("(exclude)", nacc_groups)

      rows <- lapply(all_vars, \(x) {
        desc <- S7::prop(dat[[x]], "short_descriptor")
        if (is.null(desc) || is.na(desc)) {
          desc <- ""
        }

        current_domain <- dat[[x]]@domain
        if (is.null(current_domain) || is.na(current_domain)) {
          current_domain <- "(exclude)"
        }

        method_sel <- NULL
        if (!is.null(default_methods()) && x %in% names(default_methods())) {
          method_sel <- do.call(
            paste0,
            c(
              list(default_methods()[[x]]$method),
              if (!is.null(default_methods()[[x]]$version)) {
                list(" (", default_methods()[[x]]$version, ")")
              }
            )
          )
        }

        method_input <- if (length(choices_map[[x]]) > 0) {
          shiny::selectizeInput(
            inputId = ns(paste0(x, "_method")),
            label = NULL,
            choices = choices_map[[x]],
            selected = method_sel,
            width = "100%"
          )
        }

        shiny::tags$tr(
          shiny::tags$td(x),
          shiny::tags$td(desc),
          shiny::tags$td(
            shiny::selectizeInput(
              inputId = ns(paste0(x, "_domain")),
              label = NULL,
              choices = domain_choices,
              selected = current_domain,
              width = "100%"
            )
          ),
          shiny::tags$td(method_input)
        )
      })

      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Variable"),
            shiny::tags$th("Short Descriptor"),
            shiny::tags$th("Domain"),
            shiny::tags$th("Method")
          )
        ),
        shiny::tags$tbody(rows)
      )
    })

    ## Return values
    std_methods <- shiny::reactiveVal()
    domain_assignments <- shiny::reactiveVal()
    auto_applied <- shiny::reactiveVal(FALSE)

    ## Auto-apply defaults on first load
    shiny::observe({
      all_vars <- npsych_vars()
      shiny::req(all_vars)
      dat <- dat_obj()@data

      domain_tmp <- vapply(
        all_vars,
        \(x) {
          dom <- dat[[x]]@domain
          if (is.null(dom) || is.na(dom)) NA_character_ else dom
        },
        character(1)
      )

      std_methods_tmp <- list()
      defaults <- default_methods()
      for (x in all_vars) {
        if (!is.null(defaults) && x %in% names(defaults)) {
          std_methods_tmp[[x]] <- as.list(defaults[[x]])
        }
      }

      if (length(std_methods_tmp) > 0) {
        auto_applied(TRUE)
      }

      std_methods(std_methods_tmp)
      domain_assignments(domain_tmp)
    }) |>
      shiny::bindEvent(npsych_vars(), once = TRUE)

    ## "Apply Selections" handler — reads from inputs
    shiny::observe({
      all_vars <- npsych_vars()
      shiny::req(all_vars)

      domain_tmp <- vapply(
        all_vars,
        \(x) {
          dom <- input[[paste0(x, "_domain")]]
          if (is.null(dom) || dom == "(exclude)") NA_character_ else dom
        },
        character(1)
      )

      std_methods_tmp <- list()
      for (x in all_vars) {
        meth_val <- input[[paste0(x, "_method")]]
        if (!is.null(meth_val) && nchar(meth_val) > 0) {
          meth_vers <- gsub(
            pattern = "\\(|\\)",
            replacement = "",
            x = strsplit(meth_val, split = " ")[[1]]
          )
          if (length(meth_vers) > 0) {
            std_methods_tmp[[x]] <- as.list(setNames(
              meth_vers,
              c("method", "version")[seq_along(meth_vers)]
            ))
          }
        }
      }

      std_methods(std_methods_tmp)
      domain_assignments(domain_tmp)
    }) |>
      shiny::bindEvent(input$assign)

    return(list(
      std_methods = std_methods,
      domain_assignments = domain_assignments,
      auto_applied = auto_applied
    ))
  })
}

#' Standalone app for testing methodSelectModule
#'
#' @param testing logical; passed to `shiny::shinyApp(..., options = list(test.mode))`
#'
#' @rdname methodSelectModule
#'
#' @returns A shiny app.
#'
#' @export
methodSelectApp <- function(testing = FALSE) {
  development <- dir.exists("inst/www") &&
    basename(getwd()) == "ntrd"

  shinyAddResources(development)

  default_methods <- lapply(
    setNames(ntrs::list_npsych_scores(), ntrs::list_npsych_scores()),
    \(x) {
      get_std_defaults(get_npsych_scores(x)())
    }
  ) |>
    purrr::discard(is.null)

  ui <- bslib::page_fluid(
    shinyApp_header(),
    methodSelectUI(id = "methodselect")
  )

  server <- function(input, output, session) {
    mod_output <- methodSelectServer(
      id = "methodselect",
      dat_obj = shiny::reactive(data_load(demo_source())),
      default_methods = shiny::reactive(default_methods)
    )

    shiny::exportTestValues(std_methods = mod_output$std_methods())
    shiny::exportTestValues(
      domain_assignments = mod_output$domain_assignments()
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
