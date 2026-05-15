#' Result of an extension update check
#'
#' @description
#' An S7 class representing the outcome of an `ntrd_update_available()` call.
#' Extension authors writing custom update checks should end their function
#' with a call to `update_result()` so that the framework receives a
#' well-typed value. The constructor's defaults make the "nothing to report"
#' case trivial: `update_result()` is a valid no-update-available result.
#'
#' @details
#' Properties:
#' \describe{
#'   \item{`available`}{Logical scalar. `TRUE` when a newer version of the
#'     extension is known to exist. Defaults to `FALSE`, which is the safe
#'     default: a forgotten field will not produce a spurious update
#'     prompt.}
#'   \item{`current`}{Character scalar. The installed version of the
#'     extension, or `NA_character_` if it could not be determined.}
#'   \item{`latest`}{Character scalar. The latest available version, or
#'     `NA_character_` if it could not be determined.}
#'   \item{`news_url`}{Character scalar. An optional URL pointing to a
#'     changelog or release notes to show alongside the update prompt; use
#'     `NA_character_` (the default) when none is available. Consistent with
#'     `current` and `latest`, `NA` represents "absent".}
#' }
#'
#' Validation is enforced by the class: invalid construction errors at the
#' call site. The framework wraps construction in error handling
#' (see `validate_update_check_result()`), so a misbehaving extension cannot
#' crash the app — it will be downgraded to a safe "no update" result with
#' a warning.
#'
#' @param available Logical scalar. Defaults to `FALSE`.
#' @param current Character scalar. Defaults to `NA_character_`.
#' @param latest Character scalar. Defaults to `NA_character_`.
#' @param news_url Character scalar. Defaults to `NA_character_`. Use `NA`
#'   to indicate that no NEWS URL is available.
#'
#' @returns An `update_result` S7 object.
#'
#' @examples
#' # The empty constructor — "no update available", safe default
#' update_result()
#'
#' # Couldn't fetch the remote version, but we know what's installed
#' update_result(current = "0.1.0")
#'
#' # An update is available
#' update_result(
#'   available = TRUE,
#'   current = "0.1.0",
#'   latest = "0.2.0",
#'   news_url = "https://github.com/example/pkg/blob/HEAD/NEWS.md"
#' )
#'
#' # No NEWS URL available — use NA, not NULL
#' update_result(
#'   available = TRUE,
#'   current = "0.1.0",
#'   latest = "0.2.0",
#'   news_url = NA_character_
#' )
#'
#' @export
update_result <- S7::new_class(
  name = "update_result",
  package = "ntrd",
  properties = list(
    available = S7::new_property(
      class = S7::class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          "must be a single non-NA logical"
        }
      }
    ),
    current = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (length(value) != 1) "must be a single character (may be NA)"
      }
    ),
    latest = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (length(value) != 1) "must be a single character (may be NA)"
      }
    ),
    news_url = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (length(value) != 1) "must be a single character (use NA for absent)"
        else if (!is.na(value) && !nzchar(value)) "must not be the empty string"
      }
    )
  )
)
