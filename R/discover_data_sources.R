#' Discover all available data source extensions
#'
#' Finds built-in data sources (defined in this package) and data sources from extensions
#' (from installed packages that declare themselves via DESCRIPTION fields).
#'
#' @returns A named list of `data_source` objects, keyed by their `id`.
#' @export
discover_data_sources <- function() {
  # Step 1: Load extensions packages so their S7 methods get registered
  load_extensions()

  # Step 2: Introspect the generic's methods environment
  method_env <- S7::prop(data_source_server, "methods")
  method_names <- ls(method_env)

  # Also get data_source_ui methods for cross-checking
  ui_method_env <- S7::prop(data_source_ui, "methods")
  ui_method_names <- ls(ui_method_env)

  sources <- list()

  for (nm in method_names) {
    # Check that a UI method also exists for this class
    if (!nm %in% ui_method_names) {
      cli::cli_warn(
        "{.cls {nm}} has a {.fn data_source_server} method but no
        {.fn data_source_ui} method. Skipping."
      )
      next
    }

    tryCatch(
      {
        m <- method_env[[nm]]
        cls <- S7::prop(m, "signature")[[1]]
        instance <- cls()

        if (!S7::S7_inherits(instance, data_source)) {
          cli::cli_warn(
            "{.cls {nm}} does not inherit from {.cls data_source}. Skipping."
          )
          next
        }

        sources[[instance@id]] <- instance
      },
      error = \(e) {
        cli::cli_warn("Failed to instantiate {.cls {nm}}: {e$message}")
      }
    )
  }

  sources
}
