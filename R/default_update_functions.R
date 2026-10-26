#' Default update-check and install functions for GitHub-hosted extensions
#'
#' @description
#' Factory functions that produce the two functions an extension must export
#' to opt in to ntrd's in-app update mechanism (`ntrd_update_available()` and
#' `ntrd_update_extension()`). Extensions hosted on GitHub can use these
#' directly; extensions hosted elsewhere (GitLab, CRAN, internal repos)
#' should provide their own implementations following the same contract.
#'
#' @details
#' `default_github_update_available()` returns a function that fetches the
#' `DESCRIPTION` file from the GitHub repository's default branch, parses out
#' the `Version:` field, and compares it against the locally installed
#' version. The result is wrapped in an \code{\link{update_result}} object.
#'
#' `default_github_update_extension()` returns a function that calls
#' [remotes::install_github()] with `upgrade = "always"` and
#' `dependencies = TRUE`, so dependencies (including `ntrd` itself) are also
#' brought up to date.
#'
#' Both factories call `force()` on their arguments so that captured values
#' are safe to use even if the factory is ever called in a loop.
#'
#' @param repo A GitHub repository specification of the form
#'   `"user/repo"`. May optionally include a ref (`"user/repo@ref"`); the ref
#'   is stripped for purposes of deriving the package name but preserved in
#'   the install call.
#' @param package Optional package name. If `NULL` (the default), it is
#'   derived from `repo` by taking the part after `"/"` and stripping any
#'   `@ref` suffix. Provide explicitly when the repository name differs from
#'   the R package name.
#'
#' @returns
#' For `default_github_update_available()`: a function taking no arguments
#' that returns an \code{\link{update_result}} S7 object.
#'
#' For `default_github_update_extension()`: a function taking no arguments
#' that performs the install and returns invisibly.
#'
#' @examples
#' \dontrun{
#' # In an extension package's R/updates.R:
#'
#' #' @export
#' ntrd_update_available <- ntrd::default_github_update_available(
#'   "rmtrane/ntrdWisconsin"
#' )
#'
#' #' @export
#' ntrd_update_extension <- ntrd::default_github_update_extension(
#'   "rmtrane/ntrdWisconsin"
#' )
#' }
#'
#' @name default_github_update
NULL


#' @rdname default_github_update
#' @export
default_github_update_available <- function(repo, package = NULL) {
  force(repo)
  if (is.null(package)) {
    # Strip optional @ref before taking the package name
    package <- sub("@.*$", "", strsplit(repo, "/", fixed = TRUE)[[1]][2])
  }
  force(package)

  function() {
    current <- tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(e) NA_character_
    )
    if (is.na(current)) {
      return(update_result())
    }

    latest <- tryCatch(
      as.character(get_github_version(repo)),
      error = function(e) NA_character_
    )
    if (is.na(latest)) {
      return(update_result(current = current))
    }

    available <- tryCatch(
      utils::compareVersion(latest, current) > 0,
      error = function(e) FALSE
    )

    update_result(
      available = available,
      current = current,
      latest = latest,
      news_url = sprintf(
        "https://github.com/%s/blob/HEAD/NEWS.md",
        sub("@.*$", "", repo)
      )
    )
  }
}


#' @rdname default_github_update
#' @export
default_github_update_extension <- function(repo) {
  force(repo)

  function() {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      cli::cli_abort(
        "The {.pkg remotes} package is required to install extensions from GitHub."
      )
    }

    remotes::install_github(
      repo,
      upgrade = "always",
      dependencies = TRUE,
      quiet = TRUE
    )

    invisible(NULL)
  }
}


#' Fetch the Version field from a GitHub-hosted package's DESCRIPTION
#'
#' Reads the raw DESCRIPTION file from the default branch (resolved via
#' `HEAD`) and parses the `Version:` field. If `repo` contains an `@ref`
#' suffix, that ref is used instead of `HEAD`.
#'
#' @param repo A GitHub repository specification of the form `"user/repo"`
#'   or `"user/repo@ref"`.
#'
#' @returns A `package_version` object.
#' @keywords internal
#' @noRd
get_github_version <- function(repo) {
  ref <- "HEAD"
  if (grepl("@", repo, fixed = TRUE)) {
    parts <- strsplit(repo, "@", fixed = TRUE)[[1]]
    repo <- parts[1]
    ref <- parts[2]
  }

  url <- sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION",
    repo,
    ref
  )

  con <- url(url)
  on.exit(close(con), add = TRUE)
  desc_lines <- readLines(con, warn = FALSE)

  ver_line <- grep("^Version:", desc_lines, value = TRUE)
  if (length(ver_line) == 0) {
    cli::cli_abort(
      "No {.field Version:} field found in DESCRIPTION at {.url {url}}."
    )
  }

  ver_string <- trimws(sub("^Version:", "", ver_line[1]))
  package_version(ver_string)
}
