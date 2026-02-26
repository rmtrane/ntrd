#' data_source
#'
#' @description
#' A short description...
#'
#' @slot name A character string for the display name.
#' @slot id A character string for the unique identifier.
#'
#' @export
data_source <- S7::new_class(
  "data_source",
  properties = list(
    name = S7::class_character, # display name, e.g. "WADRC (REDCap)"
    id = S7::class_character # unique identifier, e.g. "wadrc_redcap"
  )
)


# =============================================================================
# Generics
# =============================================================================

#' Data source UI
#'
#' @description
#' A short description...
#'
#' @param source An S7 data source object.
#'
#' @returns
#' A Shiny UI element.
#'
#' @export
data_source_ui <- S7::new_generic(
  "data_source_ui",
  dispatch_args = "source"
)

#' data_source_server generic
#'
#' @description
#' A short description...
#'
#' @param source An S7 object representing a data source.
#' @param ... Arguments passed to S7 methods.
#'
#' @returns
#' An S7 server object.
#'
#' @export
data_source_server <- S7::new_generic(
  "data_source_server",
  dispatch_args = "source"
)

#' Load data
#'
#' @description
#' A short description...
#'
#' @param source The data source object for dispatch.
#'
#' @returns
#' The loaded data.
#'
#' @export
data_load <- S7::new_generic(
  "data_load",
  dispatch_args = "source"
)
