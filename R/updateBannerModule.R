#' Shiny module for the in-app update banner and flow
#'
#' UI: renders a `uiOutput` slot that the server fills with a banner when
#' an update is available.
#' Server: takes a reactive yielding `list(package, result)` and handles
#' the full click-to-restart flow.
#'
#' @name update_banner_module
NULL

#' @rdname update_banner_module
#' @param id Module id.
#' @export
update_banner_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("banner"))
}

#' @rdname update_banner_module
#' @param id Module id.
#' @param update_info A reactive returning `list(package, result)` where
#'   `result` is an `update_result`.
#' @export
update_banner_server <- function(id, update_info) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- shiny::renderUI({
      info <- update_info()
      render_update_banner(
        result = info$result,
        package = info$package,
        action = shiny::actionButton(
          ns("do_update"),
          "Update",
          class = "btn-sm btn-primary update-banner-action"
        )
      )
    })

    shiny::observeEvent(input$do_update, {
      info <- update_info()
      if (!info$result@available || is.na(info$package)) {
        return()
      }

      shiny::showModal(build_confirm_modal(info, ns))
    })

    shiny::observeEvent(input$confirm_update, {
      info <- update_info()
      pkg <- info$package
      shiny::removeModal()
      if (is.na(pkg)) {
        return()
      }

      if (!supports_command_restart()) {
        shiny::showModal(build_manual_install_modal(pkg))
        return()
      }

      shiny::showNotification(
        sprintf("Updating %s\u2026 the dashboard will reopen shortly.", pkg),
        duration = NULL,
        type = "message",
        closeButton = FALSE
      )

      cmd <- build_update_restart_command(pkg)
      session$onSessionEnded(function() {
        rstudioapi::restartSession(command = cmd)
      })
      shiny::stopApp()
    })
  })
}


#' Self-contained Shiny app for testing the update banner module
#'
#' Mounts `dataSelectServer` and `update_banner_server` together with no
#' other app machinery, to verify the banner renders correctly from the
#' `update_info` reactive produced by the data-select module.
#'
#' @param fake_update Optional override for the `update_info` reactive.
#'   When `NULL` (the default), the real `update_info` from
#'   `dataSelectServer` is used. When `TRUE`, a demo update is shown
#'   regardless of which source is selected. When an `update_result`
#'   object, that result is shown directly. Useful for iterating on the
#'   banner visuals without depending on a real version mismatch.
#'
#' @returns A `shiny::shinyApp` object.
#' @keywords internal
#' @export
update_banner_app <- function(fake_update = NULL) {
  shinyAddResources(development = TRUE)

  ui <- bslib::page_navbar(
    header = shiny::tagList(shinyApp_header(), update_banner_ui("banner")),
    title = "update_banner integration harness",
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel(
      title = "Data Selection",
      value = "dataSelect",
      dataSelectUI("data_select")
    )
  )

  server <- function(input, output, session) {
    data_select <- dataSelectServer("data_select")

    update_info <- shiny::reactive({
      if (is.null(fake_update)) {
        return(data_select$update_info())
      }

      if (isTRUE(fake_update)) {
        return(list(
          package = data_select$update_info()$package,
          result = update_result(
            available = TRUE,
            current = "0.0.0.9000",
            latest = "0.0.0.9999",
            news_url = "https://github.com/rmtrane/ntrdWisconsin/blob/HEAD/NEWS.md"
          )
        ))
      } else if (S7::S7_inherits(fake_update, update_result)) {
        return(list(
          package = data_select$update_info()$package,
          result = fake_update
        ))
      } else {
        stop("`fake_update` must be NULL, TRUE, or an `update_result`.")
      }
    })

    update_banner_server("banner", update_info = update_info)
  }

  shiny::shinyApp(ui, server)
}


#' Build the "confirm update" modal dialog
#'
#' Shown when the user clicks the banner's Update button. Summarizes the
#' version change, explains the restart cascade in plain language, and
#' offers a link to release notes when available.
#'
#' @param info A list with `package` (character) and `result` (an
#'   `update_result`).
#' @param ns The module's namespacing function, used to namespace the
#'   confirm-button input id.
#'
#' @returns A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
build_confirm_modal <- function(info, ns) {
  result <- info$result
  pkg <- info$package

  body <- shiny::tagList(
    shiny::tags$p(sprintf(
      "Version %s of %s is available. The currently installed version is %s.",
      result@latest,
      pkg,
      result@current
    )),
    shiny::tags$p(
      "Updating will close the dashboard, restart R to install the new ",
      "version, then restart R again. This typically takes a minutes or",
      "two. Once the update is complete, a message will appear in the console with instructions to relaunch the dashboard."
    ),
    if (!is.na(result@news_url)) {
      shiny::tags$p(
        "Review the ",
        shiny::tags$a(
          "release notes",
          href = result@news_url,
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        " before continuing."
      )
    }
  )

  shiny::modalDialog(
    title = sprintf("Update %s", pkg),
    body,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        ns("confirm_update"),
        "Update now",
        class = "btn-primary"
      )
    ),
    easyClose = TRUE
  )
}


#' Build the "manual install instructions" modal dialog
#'
#' Shown when the user clicks confirm but the running environment cannot
#' restart the session with a follow-up command (Positron, plain R,
#' Rscript, VS Code R extension, etc.). Directs the user to call the
#' extension's own `ntrd_update_extension()` entry point, which is the
#' same function the framework would invoke automatically in RStudio.
#'
#' @param package Character string giving the extension package name.
#'
#' @returns A `shiny::modalDialog`.
#' @keywords internal
#' @noRd
build_manual_install_modal <- function(package) {
  command <- sprintf("%s::ntrd_update_extension()", package)

  shiny::modalDialog(
    title = "Update available",
    shiny::tagList(
      shiny::tags$p(
        "Automatic updates require RStudio. To update ",
        shiny::tags$code(package),
        " from the R console, close this dashboard and run:"
      ),
      shiny::tags$pre(shiny::tags$code(command)),
      shiny::tags$p(
        "Then restart R and relaunch the dashboard."
      )
    ),
    easyClose = TRUE,
    footer = shiny::modalButton("OK")
  )
}
