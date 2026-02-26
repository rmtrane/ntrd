#' Wisconsin ADRC data source
#'
#'
#' @returns
#' An S7 object of class `wadrc_source`, inheriting from `data_source`, with
#' `name = "Wisconsin ADRC"` and `id = "wadrc_redcap"`.
#'
#' @keywords internal
wadrc_source <- S7::new_class(
  "wadrc_source",
  parent = data_source,
  constructor = function() {
    S7::new_object(
      data_source(
        name = "Wisconsin ADRC",
        id = "wadrc_redcap"
      )
    )
  }
)

#' Data source UI for WADRC source
#'
#' @param source An S7 `wadrc_source` object.
#' @param ns A Shiny namespace function.
#'
#' @returns
#' A `shiny::tagList` containing UI elements for configuring REDCap API tokens and a Panda API Key.
#'
#' @name data_source_ui
#'
#' @keywords internal
S7::method(data_source_ui, wadrc_source) <- function(source, ns) {
  shiny::tagList(
    bslib::card(
      bslib::card_title("REDCap API Tokens"),
      api_token_inputs(
        label = "UDS-2:",
        placeholder = "Enter REDCap API Token for UDS-2",
        inputId = ns("uds2_api_token")
      ),
      api_token_inputs(
        label = "UDS-3:",
        placeholder = "Enter REDCap API Token for UDS-3",
        inputId = ns("uds3_api_token")
      ),
      api_token_inputs(
        label = "UDS-4:",
        placeholder = "Enter REDCap API Token for UDS-4",
        inputId = ns("uds4_api_token")
      )
    ),
    bslib::card(
      bslib::card_title("Panda API Key"),
      api_token_inputs(
        label = "Key:",
        placeholder = "Enter Panda API Key (optional)",
        inputId = ns("panda_api_token")
      )
    )
  )
}


#' API token inputs
#'
#'
#' @param label A single string. Optional.
#' @param placeholder A single string. Optional.
#' @param inputId A single string. Optional.
#'
#' @returns
#' A Shiny `fluidRow` UI element containing a labeled password input field.
#'
#' @keywords internal
api_token_inputs <- function(
  label = "UDS-2:",
  placeholder = "Enter REDCap API Token for UDS-2",
  inputId = ns("uds2_api_token")
) {
  shiny::fluidRow(
    shiny::column(
      2,
      tags$label(
        label,
        `for` = inputId,
        style = "text-align: right; display: block; padding-top: 7px;"
      )
    ),
    shiny::column(
      6,
      shiny::tagAppendAttributes(
        shiny::passwordInput(
          inputId = inputId,
          label = NULL,
          placeholder = placeholder
        ),
        .cssSelector = "input",
        autocomplete = "current-password"
      )
    )
  )
}


#' Data source server for wadrc_source
#'
#' @description
#' A short description...
#'
#' @param source An S7 `wadrc_source` object.
#' @param id A single string, the Shiny module ID.
#'
#' @returns
#' A `shiny::moduleServer` function that returns a list containing:
#' \itemize{
#'   \item `params`: A reactive list of API tokens, requiring `uds2_api_token`,
#'     `uds3_api_token`, and `uds4_api_token` to be present.
#'   \item `restore`: A function to update the API token input fields.
#'   \item `extras`: A `shiny::reactiveValues` object holding `all_values` (data
#'     fetched using `panda_api_token`) and `panda_api_token`.
#'   \item `session`: The Shiny session object.
#' }
#'
#' @name wadrc_source
#'
#' @export
S7::method(data_source_server, wadrc_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    params <- shiny::reactive({
      shiny::req(input$uds2_api_token)
      shiny::req(input$uds3_api_token)
      shiny::req(input$uds4_api_token)

      list(
        uds2_api_token = input$uds2_api_token,
        uds3_api_token = input$uds3_api_token,
        uds4_api_token = input$uds4_api_token,
        panda_api_token = input$panda_api_token
      )
    })

    # fmt: skip
    restore <- function(params) {
      shiny::updateTextInput(session, "uds2_api_token", value = params$uds2_api_token)
      shiny::updateTextInput(session, "uds3_api_token", value = params$uds3_api_token)
      shiny::updateTextInput(session, "uds4_api_token", value = params$uds4_api_token)
      shiny::updateTextInput(session, "panda_api_token", value = params$panda_api_token)
    }

    extras <- shiny::reactiveValues(
      all_values = NULL,
      panda_api_token = NULL
    )

    shiny::observe({
      extras$panda_api_token <- input$panda_api_token
    })

    all_values_et <- shiny::ExtendedTask$new(
      \(api) {
        mirai::mirai(
          {
            get_all_values(
              api_key = api,
              base_query_file = bq_file
            )
          },
          .args = list(
            get_all_values = get_all_values,
            api = api,
            bq_file = system.file(
              "json/panda_template.json",
              package = "NpsychAssessmentTool"
            )
          )
        )
      }
    )

    shiny::observe({
      if (all_values_et$status() == "success") {
        extras$all_values <- all_values_et$result()
      }
    }) |>
      shiny::bindEvent(all_values_et$status())

    # Kick off fetch whenever panda token is available
    shiny::observe({
      shiny::req(input$panda_api_token)
      shiny::req(nzchar(input$panda_api_token))
      all_values_et$invoke(api = input$panda_api_token)
    })

    list(
      params = params,
      restore = restore,
      extras = extras,
      session = session
    )
  })
}

#' Load WADRC data
#'
#' @description
#' A short description...
#'
#' @param source An object of class `wadrc_source`.
#' @param uds2_api_token A single string.
#' @param uds3_api_token A single string.
#' @param uds4_api_token A single string.
#' @param ... Currently unused; must be empty.
#'
#' @returns
#' An object of class `data_nacc` if data is successfully retrieved and combined.
#' If no data is retrieved from any UDS, `NULL` is returned and an error notification is shown in a Shiny context.
#' Notifications are displayed within a Shiny application to indicate data loading progress and completion.
#'
#' @name wadrc_source
#'
#' @export
S7::method(data_load, wadrc_source) <- function(
  source,
  uds2_api_token,
  uds3_api_token,
  uds4_api_token,
  ...
) {
  prepped_list <- list(
    pull_redcap_data(
      token = uds2_api_token,
      fields = wadrc_uds2_redcap_fields,
      uds = 2
    ),
    pull_redcap_data(
      token = uds3_api_token,
      fields = wadrc_uds3_redcap_fields,
      uds = 3
    ),
    pull_redcap_data(
      token = uds4_api_token,
      fields = wadrc_uds4_redcap_fields,
      uds = 4
    )
  )

  prepped_list <- Filter(Negate(is.null), prepped_list)

  if (length(prepped_list) == 0) {
    shiny::showNotification("No data retrieved.", type = "error")
    return(NULL)
  }

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::showNotification(
      "Combining data sets",
      duration = NULL,
      id = "combining"
    )
  }

  combined <- data.table::rbindlist(prepped_list, fill = TRUE) |>
    unique()

  combined <- fill_data_downup(
    out = combined,
    ptid = "NACCID",
    visityr = "VISITYR",
    visitmo = "VISITMO",
    visitday = "VISITDAY",
    educ = "EDUC",
    constant_across_visits = c("BIRTHYR", "BIRTHMO", "SEX", "RACE")
  )

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::removeNotification(id = "combining")
    shiny::showNotification(
      "REDCap data ready!",
      type = "message"
    )
  }

  data_nacc(data = combined)
}


#' Pull REDCap data
#'
#' @description
#' A short description...
#'
#' @param token A single string, the REDCap API token.
#' @param fields A character vector of field names to pull.
#' @param uds A single string or number representing the UDS version.
#'
#' @returns
#' A prepared data table from `wadrc_data_prep()`. If the data pull fails,
#' it returns `NULL` and will show a notification in a Shiny app or emit a warning.
#'
#' @export
pull_redcap_data <- function(token, fields, uds) {
  # If shiny running, display notification
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::showNotification(
      paste0("Pulling data from REDCap UDS-", uds, " database..."),
      duration = NULL,
      type = "message",
      id = "pulling_from_redcap"
    )
  }
  res <- REDCapR::redcap_read_oneshot(
    redcap_uri = "https://redcap.medicine.wisc.edu/api/",
    token = token,
    fields = fields,
    guess_max = Inf
  )

  if (!res$success && nrow(res$data) > 10) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "pulling_from_redcap")
      shiny::showNotification(
        paste0("Failed to pull UDS-", uds, " data from REDCap"),
        duration = NULL
      )
    } else {
      cli::cli_warn("Failed to pull UDS-{uds} data from REDCap")
    }
    return(NULL)
  }

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::removeNotification(id = "pulling_from_redcap")
    shiny::showNotification(
      paste0("Preparing UDS-", uds, " data"),
      duration = NULL,
      id = "preparing_uds"
    )
  }

  out <- wadrc_data_prep(
    adrc_data = data.table::as.data.table(res$data),
    uds = paste0("uds", uds)
  )

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::removeNotification(id = "preparing_uds")
  }

  out
}
