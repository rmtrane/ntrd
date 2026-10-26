#' Load extension packages
#'
#' Scans installed packages for those declaring themselves as
#' ntrd extensions and loads their namespaces.
#' Loading registers their S7 methods (via S7::methods_register()
#' in .onLoad), making them visible to discover_data_sources().
#'
#' @returns NULL (called for side effects)
#' @keywords internal
load_extensions <- function() {
  fields <- c(
    "Package",
    "Config/ntrd/extension"
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
        info[[fields[2]]],
        "true"
      )

      if (!is_extension) {
        next
      }

      if (isNamespaceLoaded(pkg_name)) {
        tryCatch(
          {
            eval(quote(S7::methods_register()), envir = asNamespace(pkg_name))
            cli::cli_inform(
              "Re-registered methods for already-loaded extension: {.pkg {pkg_name}}."
            )
          },
          error = \(e) {
            cli::cli_warn(
              "Failed to re-register methods for {.pkg {pkg_name}}: {e$message}"
            )
          }
        )
      } else {
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
  }

  .check_generic_conflicts()

  invisible(NULL)
}

#' Check for conflicting generic definitions across extensions
#'
#' Warns when two loaded packages both define an S7 generic with the
#' same `std_using_*` name, which indicates an extension design issue.
#'
#' @returns NULL (called for side effects)
#' @keywords internal
.check_generic_conflicts <- function() {
  seen <- list() # generic_name -> package_name

  for (ns_name in loadedNamespaces()) {
    ns <- asNamespace(ns_name)

    for (nm in grep("^std_using_", ls(ns), value = TRUE)) {
      obj <- get(nm, envir = ns)
      if (!inherits(obj, "S7_generic")) {
        next
      }

      if (nm %in% names(seen) && seen[[nm]] != ns_name) {
        cli::cli_warn(c(
          "!" = "Generic {.fn {nm}} is defined in both {.pkg {seen[[nm]]}} and {.pkg {ns_name}}.",
          "i" = "Extension authors should use shared generics from a common dependency rather than redefining them."
        ))
      } else {
        seen[[nm]] <- ns_name
      }
    }
  }
}
