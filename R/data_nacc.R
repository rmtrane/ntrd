#' NACC data class
#'
#' @description
#' A short description...
#'
#' @slot data A `data.table` containing NACC data. If the provided value
#'   is not a `data.table`, it will be coerced to one by the setter.
#'
#' @returns
#' An S7 object of class `data_nacc`. Object construction will fail if the
#'   provided data is missing required columns, has incorrect column types
#'   (`NACCID` must be character; `VISITYR`, `VISITMO`, `VISITDAY`, `SEX`, `EDUC`, `BIRTHYR` must be numeric),
#'   or contains invalid values (e.g., `SEX` not 1/2/NA, `VISITYR` < 2005, `VISITMO` not 1-12, `VISITDAY` not 1-31,
#'   or an invalid date combination from year/month/day).
#'
#' @export
data_nacc <- S7::new_class(
  "data_nacc",
  properties = list(
    data = S7::new_property(
      class = S7::class_any,
      setter = function(self, value) {
        if (!data.table::is.data.table(value)) {
          value <- data.table::as.data.table(value)
        }
        self@data <- value
        self
      }
    )
  ),
  validator = function(self) {
    d <- self@data
    problems <- character(0)
    max_examples <- 5L

    required_cols <- c(
      "NACCID",
      "VISITYR",
      "VISITMO",
      "VISITDAY",
      "SEX",
      "EDUC",
      "BIRTHYR"
    )

    missing <- setdiff(required_cols, colnames(d))
    if (length(missing) > 0) {
      problems <- c(
        problems,
        cli::format_inline(
          "Missing required column{?s}: {.field {missing}}"
        )
      )
    }

    present <- intersect(required_cols, colnames(d))

    # --- Type checks ---

    if ("NACCID" %in% present && !is.character(d$NACCID)) {
      problems <- c(
        problems,
        cli::format_inline(
          "{.field NACCID} must be {.cls character}, not {.cls {class(d$NACCID)}}"
        )
      )
    }

    numeric_cols <- intersect(
      c("VISITYR", "VISITMO", "VISITDAY", "SEX", "EDUC", "BIRTHYR"),
      present
    )
    for (col in numeric_cols) {
      if (!is.numeric(d[[col]])) {
        problems <- c(
          problems,
          cli::format_inline(
            "{.field {col}} must be {.cls numeric}, not {.cls {class(d[[col]])}}"
          )
        )
      }
    }

    # Helper: format row references with "e.g." only when truncated
    fmt_rows <- function(bad_rows) {
      n <- length(bad_rows)
      examples <- utils::head(bad_rows, max_examples)
      prefix <- if (n > max_examples) "e.g. " else ""
      cli::format_inline("{prefix}{examples}")
    }

    # --- Value checks (only if column exists and is correct type) ---

    if ("SEX" %in% present && is.numeric(d$SEX)) {
      bad_rows <- which(!d$SEX %in% c(1, 2, NA))
      if (length(bad_rows) > 0) {
        problems <- c(
          problems,
          cli::format_inline(
            "{.field SEX} must be 1, 2, or {.val {NA}}. {length(bad_rows)} invalid row{?s}: {fmt_rows(bad_rows)}"
          )
        )
      }
    }

    if ("VISITYR" %in% present && is.numeric(d$VISITYR)) {
      bad_rows <- which(!is.na(d$VISITYR) & d$VISITYR < 2005)
      if (length(bad_rows) > 0) {
        problems <- c(
          problems,
          cli::format_inline(
            "{.field VISITYR} must be >= 2005. {length(bad_rows)} invalid row{?s}, {fmt_rows(bad_rows)}"
          )
        )
      }
    }

    if ("VISITMO" %in% present && is.numeric(d$VISITMO)) {
      bad_rows <- which(!is.na(d$VISITMO) & (d$VISITMO < 1 | d$VISITMO > 12))
      if (length(bad_rows) > 0) {
        problems <- c(
          problems,
          cli::format_inline(
            "{.field VISITMO} must be 1\u201312. {length(bad_rows)} invalid row{?s}, {fmt_rows(bad_rows)}"
          )
        )
      }
    }

    if ("VISITDAY" %in% present && is.numeric(d$VISITDAY)) {
      bad_rows <- which(!is.na(d$VISITDAY) & (d$VISITDAY < 1 | d$VISITDAY > 31))
      if (length(bad_rows) > 0) {
        problems <- c(
          problems,
          cli::format_inline(
            "{.field VISITDAY} must be 1\u201331. {length(bad_rows)} invalid row{?s}, {fmt_rows(bad_rows)}"
          )
        )
      }
    }

    # --- Date validity check ---
    date_cols <- c("VISITYR", "VISITMO", "VISITDAY")
    if (
      all(date_cols %in% present) &&
        all(vapply(date_cols, \(col) is.numeric(d[[col]]), logical(1)))
    ) {
      complete <- which(
        !is.na(d$VISITYR) & !is.na(d$VISITMO) & !is.na(d$VISITDAY)
      )

      if (length(complete) > 0) {
        date_strings <- sprintf(
          "%04d-%02d-%02d",
          as.integer(d$VISITYR[complete]),
          as.integer(d$VISITMO[complete]),
          as.integer(d$VISITDAY[complete])
        )
        parsed <- as.Date(date_strings, format = "%Y-%m-%d")
        bad_idx <- which(is.na(parsed))

        if (length(bad_idx) > 0) {
          bad_rows <- complete[bad_idx]
          n <- length(bad_rows)
          examples <- utils::head(bad_rows, max_examples)
          example_dates <- utils::head(date_strings[bad_idx], max_examples)

          labels <- vapply(
            seq_along(examples),
            \(i) {
              cli::format_inline(
                "row {examples[i]} ({.val {example_dates[i]}})"
              )
            },
            character(1)
          )
          labels_collapsed <- paste(labels, collapse = ", ")
          prefix <- if (n > max_examples) "e.g. " else ""
          problems <- c(
            problems,
            cli::format_inline(
              "{n} invalid date{?s}: {prefix}{labels_collapsed}"
            )
          )
        }
      }
    }

    if (length(problems) > 0) problems else NULL
  }
)

if (FALSE) {
  data_nacc(
    data.frame(
      NACCID = 1:5,
      VISITYR = c(2006:2010),
      VISITMO = c(1:5),
      VISITDAY = c(31),
      SEX = c(1, 2, 1, NA, 3)
    )
  )
}
