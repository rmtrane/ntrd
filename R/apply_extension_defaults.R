#' Apply extension defaults
#'
#' Looks up and calls `.set_defaults()` in the namespace of an extension
#' package. This is a no-op when `ext_pkg` is `"ntrd"` (the base package).
#'
#' @param ext_pkg Character string. The package name of the data source.
#'
#' @returns `TRUE` (invisibly) if `.set_defaults()` was found and called,
#'   `FALSE` otherwise.
#'
#' @keywords internal
apply_extension_defaults <- function(ext_pkg) {
  if (ext_pkg == "ntrd") {
    return(invisible(FALSE))
  }

  set_defaults <- get0(
    ".set_defaults",
    envir = asNamespace(ext_pkg)
  )

  if (is.function(set_defaults)) {
    set_defaults()
    return(invisible(TRUE))
  }

  invisible(FALSE)
}
