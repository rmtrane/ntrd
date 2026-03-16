#' data_source
#'
#' @description
#' A short description...
#'
#' @param name A character string for the display name.
#' @param id A character string for the unique identifier.
#'
#' @export
data_source <- S7::new_class(
  "data_source",
  properties = list(
    name = S7::class_character, # display name, e.g. "WADRC (REDCap)"
    id = S7::class_character # unique identifier, e.g. "wadrc_redcap"
  )
)

#' Create a new data source class
#'
#' Convenience constructor that creates an S7 class inheriting from
#' [data_source]. The returned object is a **class** (not an instance) that
#' can be instantiated with no arguments.
#'
#' @param name Character. Human-readable display name shown in the UI dropdown.
#' @param id Character. Unique identifier used internally as a key.
#'
#' @returns An S7 class that inherits from `data_source`.
#'
#' @examples
#' my_source <- new_data_source(name = "My Source", id = "my_source")
#' my_source()
#'
#' @export
new_data_source <- function(name, id) {
  pkg <- package %||% environmentName(topenv(parent.frame()))

  S7::new_class(
    id,
    parent = data_source,
    constructor = function() {
      S7::new_object(
        data_source(name = name, id = id, package = pkg, ...)
      )
    }
  )
}


# =============================================================================
# Generics
# =============================================================================

#' Data source UI
#'
#' @description
#' A short description...
#'
#' @param source An S7 data source object.
#' @param ... Arguments passed to S7 methods.
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
#' @param ... Arguments passed to S7 methods.
#'
#' @returns
#' The loaded data.
#'
#' @export
data_load <- S7::new_generic(
  "data_load",
  dispatch_args = "source"
)
