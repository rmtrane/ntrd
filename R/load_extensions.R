#' Load extension packages
#'
#' Scans installed packages for those declaring themselves as
#' NpsychAssessmentTool extensions and loads their namespaces.
#' Loading registers their S7 methods (via S7::methods_register()
#' in .onLoad), making them visible to discover_data_sources().
#'
#' @returns NULL (called for side effects)
#' @keywords internal
load_extensions <- function() {
  fields <- c(
    "Package",
    "Config/NpsychAssessmentTool/extension"
  )

  seen_pkgs <- character(0)

  for (lib in .libPaths()) {
    pkg_dirs <- list.dirs(lib, full.names = TRUE, recursive = FALSE)

    for (pkg_dir in pkg_dirs) {
      pkg_name <- basename(pkg_dir)
      if (pkg_name %in% seen_pkgs) {
        next
      }
      seen_pkgs <- c(seen_pkgs, pkg_name)

      desc_file <- file.path(pkg_dir, "DESCRIPTION")
      if (!file.exists(desc_file)) {
        next
      }

      info <- tryCatch(
        {
          dcf <- read.dcf(desc_file, fields = fields)
          if (nrow(dcf) > 0) as.list(dcf[1, ]) else NULL
        },
        error = \(e) NULL
      )

      if (is.null(info)) {
        next
      }

      is_extension <- identical(
        info[["Config/NpsychAssessmentTool/extension"]],
        "true"
      )

      if (!is_extension) {
        next
      }

      # Skip if already loaded
      if (isNamespaceLoaded(pkg_name)) {
        next
      }

      tryCatch(
        {
          loadNamespace(pkg_name)
          cli::cli_inform("Loaded extension: {.pkg {pkg_name}}")
        },
        error = \(e) {
          cli::cli_warn(
            "Failed to load extension {.pkg {pkg_name}}: {e$message}"
          )
        }
      )
    }
  }

  invisible(NULL)
}
