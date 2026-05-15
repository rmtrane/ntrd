#' Print formatted messages for the extension update flow
#'
#' @description
#' These functions print user-facing messages during the in-app update
#' flow's restarted-session steps. They are exported so that they can be
#' invoked by fully qualified name from the strings built by
#' [build_update_restart_command()], which run in restarted R sessions
#' where `ntrd` is not attached.
#'
#' Each function takes the extension package name as its only argument and
#' returns invisibly. [update_finalize()] dispatches to the appropriate
#' message based on whether the install succeeded.
#'
#' Users do not normally call these directly.
#'
#' @param package Character string giving the extension package name.
#' @returns Invisible `NULL`.
#'
#' @name update_messages
NULL


#' @rdname update_messages
#' @export
update_intro_message <- function(package) {
  cli::cli_h1("Updating {.pkg {package}}")
  cli::cli_alert_info(
    "Installing the latest version. This may take a moment..."
  )
  invisible(NULL)
}


#' @rdname update_messages
#' @export
update_success_message <- function(package) {
  cli::cli_h1("Update complete")
  cli::cli_alert_success("{.pkg {package}} has been updated successfully.")
  cli::cli_text("")
  cli::cli_text("To relaunch the dashboard, run:")
  cli::cli_code("ntrd::shinyDashboard()")
  invisible(NULL)
}


#' @rdname update_messages
#' @export
update_failure_message <- function(package) {
  cli::cli_h1("Update did not complete")
  cli::cli_alert_warning("{.pkg {package}} could not be updated.")
  cli::cli_text("")
  cli::cli_text("See the messages above for details.")
  cli::cli_text(
    "To relaunch the dashboard on the previously installed version, select the 'Neuropsychological Test Result Dashboard' addin from the 'Addins' menu, or run {.code ntrd::shinyDashboard()} in the console."
  )
  invisible(NULL)
}


#' Dispatch to the success or failure message after an install attempt
#'
#' Called from the second restart's command (see
#' [build_update_restart_command()]) with the boolean result of the install
#' and the package name. Validates its inputs so that misuse fails loudly.
#'
#' @param ok Logical scalar. `TRUE` if the install succeeded, `FALSE` if it
#'   errored.
#' @param package Character string giving the extension package name.
#'
#' @returns Invisible `NULL`.
#' @export
#' @keywords internal
update_finalize <- function(ok, package) {
  stopifnot(
    is.logical(ok),
    length(ok) == 1,
    !is.na(ok),
    is.character(package),
    length(package) == 1,
    nzchar(package)
  )

  if (ok) {
    update_success_message(package)
  } else {
    update_failure_message(package)
  }
  invisible(NULL)
}
