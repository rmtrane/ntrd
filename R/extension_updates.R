#' Framework machinery for extension update checks
#'
#' @description
#' Internal helpers and a small public surface (`check_extension_update()`,
#' `validate_update_check_result()`, `clear_update_cache()`) used by the ntrd
#' Shiny app to discover whether an extension has opted in to in-app updates,
#' fetch (and cache) the update-availability information, validate its shape,
#' and build the restart command used by the "Update" button. See also
#' [update_success_message()], [update_failure_message()], and the
#' [update_finalize()] dispatcher for the user-facing copy printed during
#' the restart cascade.
#'
#' Extensions opt in by exporting two functions:
#' \itemize{
#'   \item \code{ntrd_update_available()} — returns an
#'     \code{\link{update_result}} S7 object. A plain list with the same
#'     fields is also accepted as a backwards-compatibility courtesy.
#'   \item \code{ntrd_update_extension()} — performs the install. Runs in a
#'     restarted R session. Functions made by
#'     \code{\link{default_github_update_extension}} are not called at all:
#'     ntrd reads the repo they record and installs by package name. Any
#'     other function is looked up and called directly in that session,
#'     which loads the extension's namespace — but not `ntrd`'s, which
#'     isn't loaded until the install has finished.
#' }
#'
#' See \code{\link{default_github_update_available}} and
#' \code{\link{default_github_update_extension}} for factory functions that
#' produce these two for GitHub-hosted extensions.
#'
#' @name extension_updates
NULL


# Internal session cache for update-check results, keyed by package name.
# Each value is a list with components `checked_at` (POSIXct) and `result`
# (the validated update_result). Cleared automatically when the package
# is unloaded.
.update_cache <- new.env(parent = emptyenv())

# Internal seam: indirection over base::getNamespace() so that tests
# can mock namespace lookup via testthat::local_mocked_bindings().
get_pkg_namespace <- function(package) {
  getNamespace(package)
}

# Internal seams over utils:: and rstudioapi:: calls so tests can mock them
# via testthat::local_mocked_bindings().
get_from_namespace <- function(name, ns) {
  utils::getFromNamespace(name, ns = ns)
}

restart_session <- function(command) {
  rstudioapi::restartSession(command = command)
}


#' Check whether an extension package opts in to in-app updates
#'
#' Returns `TRUE` if the package's namespace exports both
#' `ntrd_update_available` and `ntrd_update_extension` as functions, and
#' `FALSE` otherwise. If only one of the two is exported, a one-time warning
#' is emitted (per package, per session).
#'
#' @param package Character string giving the package name.
#'
#' @returns `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
extension_supports_updates <- function(package) {
  if (!is.character(package) || length(package) != 1 || is.na(package)) {
    return(FALSE)
  }

  ns <- tryCatch(get_pkg_namespace(package), error = function(e) NULL)
  if (is.null(ns)) {
    return(FALSE)
  }

  has_check <- exists(
    "ntrd_update_available",
    envir = ns,
    mode = "function",
    inherits = FALSE
  )
  has_install <- exists(
    "ntrd_update_extension",
    envir = ns,
    mode = "function",
    inherits = FALSE
  )

  if (has_check && has_install) {
    return(TRUE)
  }

  if (has_check || has_install) {
    warn_key <- paste0(".half_optin_warned_", package)
    if (!exists(warn_key, envir = .update_cache, inherits = FALSE)) {
      missing_fn <- if (has_check) {
        "ntrd_update_extension"
      } else {
        "ntrd_update_available"
      }
      cli::cli_warn(c(
        "Extension {.pkg {package}} exports one but not both update hooks.",
        "i" = "Missing export: {.fn {missing_fn}}.",
        "i" = "In-app updates are disabled for this extension."
      ))
      assign(warn_key, TRUE, envir = .update_cache)
    }
  }

  FALSE
}


#' Validate (or coerce) the result of an `ntrd_update_available()` call
#'
#' Accepts either an `update_result` S7 instance (the canonical form) or a
#' plain list with the expected fields (a courtesy for extensions that
#' haven't migrated to the constructor yet). Returns a validated
#' `update_result`. On any error — wrong type, missing field, invalid
#' value — emits a warning and returns the safe "no update available"
#' default, i.e. `update_result()`.
#'
#' This wrapper exists because extension authors write the function whose
#' result this validates; the framework can never trust the value
#' completely and must downgrade gracefully on malformed input.
#'
#' @param x Object to validate. Either an `update_result` or a list.
#' @param package Optional package name, used only to make warning messages
#'   more informative.
#'
#' @returns An `update_result` S7 object.
#' @export
validate_update_check_result <- function(x, package = NULL) {
  pkg_label <- if (is.null(package)) "extension" else package

  if (S7::S7_inherits(x, update_result)) {
    return(x)
  }

  if (!is.list(x)) {
    cli::cli_warn(c(
      "Update-check result from {.pkg {pkg_label}} is not an {.cls update_result} or list; ignoring.",
      "i" = "Got an object of class {.cls {class(x)[1]}}."
    ))
    return(update_result())
  }

  # Strip any extra fields the extension author may have included that
  # aren't part of the contract; pass through only known properties.
  known <- c("available", "current", "latest", "news_url")
  args <- x[intersect(names(x), known)]

  tryCatch(
    do.call(update_result, args),
    error = function(e) {
      cli::cli_warn(c(
        "Update-check result from {.pkg {pkg_label}} could not be coerced to {.cls update_result}.",
        "i" = "{conditionMessage(e)}",
        "i" = "Treating as: no update available."
      ))
      update_result()
    }
  )
}


#' Check whether an update is available for an extension package
#'
#' Wraps the extension's exported `ntrd_update_available()` with caching,
#' error handling, and result validation. Safe to call from Shiny reactive
#' contexts; will never raise an error.
#'
#' @param package Character string giving the extension package name.
#' @param force Logical. If `TRUE`, ignore any cached result and re-check.
#'   Default `FALSE`.
#' @param ttl Numeric. Cache time-to-live in seconds. Cached results older
#'   than `ttl` are refreshed on the next call. Default 3600 (one hour).
#'
#' @returns An `update_result` S7 object. If the extension does not opt in
#'   to updates, or if the check errors, returns the safe `update_result()`
#'   default.
#' @export
check_extension_update <- function(package, force = FALSE, ttl = 3600) {
  if (!extension_supports_updates(package)) {
    return(update_result())
  }

  # Cache lookup
  if (
    !isTRUE(force) && exists(package, envir = .update_cache, inherits = FALSE)
  ) {
    entry <- get(package, envir = .update_cache, inherits = FALSE)
    if (
      is.list(entry) &&
        !is.null(entry$checked_at) &&
        as.numeric(Sys.time() - entry$checked_at, units = "secs") < ttl
    ) {
      return(entry$result)
    }
  }

  ns <- get_pkg_namespace(package)
  raw <- tryCatch(
    ns$ntrd_update_available(),
    error = function(e) {
      cli::cli_warn(c(
        "Update check for {.pkg {package}} failed.",
        "i" = "{conditionMessage(e)}"
      ))
      NULL
    }
  )

  result <- if (is.null(raw)) {
    update_result()
  } else {
    validate_update_check_result(raw, package)
  }

  assign(
    package,
    list(checked_at = Sys.time(), result = result),
    envir = .update_cache
  )

  result
}


#' Clear the update-check cache
#'
#' Primarily useful for testing and for a "check again" UI affordance.
#'
#' @param package Optional package name. If supplied, only that package's
#'   cached result is cleared; otherwise the entire cache is cleared.
#'
#' @returns Invisible `NULL`.
#' @export
clear_update_cache <- function(package = NULL) {
  if (is.null(package)) {
    rm(
      list = ls(envir = .update_cache, all.names = TRUE),
      envir = .update_cache
    )
  } else if (exists(package, envir = .update_cache, inherits = FALSE)) {
    rm(list = package, envir = .update_cache)
  }
  invisible(NULL)
}


#' Whether the running environment supports `restartSession(command = ...)`
#'
#' RStudio supports this fully. Positron currently does not honor the
#' `command` argument (it requires it to be empty), so we treat Positron
#' as unsupported for the automatic-update flow even though
#' `rstudioapi::isAvailable()` returns TRUE there. Plain R, Rscript, and
#' VS Code's R extension are also unsupported.
#'
#' @returns `TRUE` if `restartSession(command = ...)` will run the command
#'   in the restarted session, `FALSE` otherwise.
#' @keywords internal
#' @noRd
supports_command_restart <- function() {
  identical(Sys.getenv("RSTUDIO"), "1") && rstudioapi::isAvailable()
}


#' Build the restart command that performs an extension update
#'
#' Constructs the string passed to `rstudioapi::restartSession(command = ...)`.
#' The generated script prints a short "installing..." message, attempts the
#' install in a `tryCatch`, and queues a second restart that runs
#' [update_finalize()] with the outcome. That leaves the user at an idle R
#' prompt with a status message and instructions to relaunch the dashboard;
#' the dashboard does not reopen by itself.
#'
#' How the script installs the update depends on the extension's
#' `ntrd_update_extension()`:
#'
#' \itemize{
#'   \item If it was made by [default_github_update_extension()] (so it
#'     carries an `"ntrd_github_repo"` attribute), the script calls
#'     [remotes::install_github()] directly, by package name, without
#'     looking up the function at all.
#'   \item Otherwise, the script looks up `ntrd_update_extension()` with
#'     `getNamespace()` and calls it. This loads the extension's namespace
#'     (and whatever it imports), but it's base R only — no package lookup
#'     function from `ntrd` is involved.
#' }
#'
#' Either way, nothing from `ntrd` itself is loaded until the second
#' restart, after the install has finished, so `ntrd` is never one of the
#' packages the install might be replacing.
#'
#' @param package Character string giving the extension package name.
#' @param repo GitHub repository to install from, or `NULL` to fall back to
#'   looking up the extension's `ntrd_update_extension()` at restart time. By
#'   default, read from the extension's `ntrd_update_extension()`.
#'
#' @returns A character string suitable for `rstudioapi::restartSession()`'s
#'   `command` argument.
#' @keywords internal
build_update_restart_command <- function(
  package,
  repo = extension_install_repo(package)
) {
  if (
    !is.character(package) ||
      length(package) != 1 ||
      is.na(package) ||
      !nzchar(package)
  ) {
    cli::cli_abort("{.arg package} must be a single non-empty string.")
  }

  q <- function(x) encodeString(x, quote = "\"")

  install_expr <- if (is.null(repo)) {
    sprintf("getNamespace(%s)$ntrd_update_extension()", q(package))
  } else {
    sprintf(
      "remotes::install_github(%s, upgrade = \"always\", dependencies = TRUE, quiet = TRUE)",
      q(repo)
    )
  }

  intro <- sprintf(
    "\n== Updating %s ==\nInstalling the latest version. This may take a few minutes...\n\n",
    package
  )
  finalize <- function(ok) {
    q(sprintf("ntrd::update_finalize(%s, %s)", ok, q(package)))
  }

  paste(
    sprintf("cat(%s)", q(intro)),
    paste0(
      "ok <- tryCatch({\n",
      "  ",
      install_expr,
      "\n",
      "  TRUE\n",
      "}, error = function(e) {\n",
      "  message(\"Install error: \", conditionMessage(e))\n",
      "  FALSE\n",
      "})"
    ),
    sprintf(
      "rstudioapi::restartSession(command = if (ok) %s else %s)",
      finalize("TRUE"),
      finalize("FALSE")
    ),
    sep = "\n"
  )
}

# GitHub repo recorded by default_github_update_extension(), or NULL when the
# extension supplies its own install function.
extension_install_repo <- function(package) {
  fun <- tryCatch(
    get_from_namespace("ntrd_update_extension", ns = package),
    error = function(e) NULL
  )
  repo <- attr(fun, "ntrd_github_repo", exact = TRUE)
  if (is.character(repo) && length(repo) == 1 && !is.na(repo) && nzchar(repo)) {
    repo
  } else {
    NULL
  }
}
