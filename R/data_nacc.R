#' NACC data class
#'
#' @description
#' A short description...
#'
#' @param data A `data.table` containing NACC data. If the provided value
#'   is not a `data.table`, it will be coerced to one by the setter, which
#'   also adds `VISITDATE` and `NACCAGE` if these are not already present.
#'
#' @returns
#' An S7 object of class `data_nacc`. Object construction will fail if the
#'   provided data is missing required columns, has incorrect column types
#'   (`NACCID` must be character; either `VISITYR`, `VISITMO`, and `VISITDAY` as numerics, or `VISITDATE` as
#'   `"YYYY-MM-DD"`; `SEX` and `EDUC` must be numeric; `BIRTHYR`and `BIRTHMO` or `NACCAGE` must be present as numerics),
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
        } else {
          value <- data.table::copy(value)
        }

        if (!"VISITDATE" %in% colnames(value)) {
          value$VISITDATE <- as.Date(ifelse(
            test = is.na(value$VISITYR) |
              is.na(value$VISITMO) |
              is.na(value$VISITDAY),
            yes = NA,
            no = paste(value$VISITYR, value$VISITMO, value$VISITDAY, sep = "-")
          ))
        }

        visitdate_cols <- intersect(
          colnames(value),
          c("VISITYR", "VISITMO", "VISITDAY")
        )

        if (length(visitdate_cols) > 0) {
          data.table::set(value, j = visitdate_cols, value = NULL)
        }

        # value$VISITYR <- value$VISITMO <- value$VISITDAY <- NULL

        if (!"NACCAGE" %in% colnames(value)) {
          value$NACCAGE <- lubridate::time_length(
            value$VISITDATE -
              as.Date(ifelse(
                test = is.na(value$BIRTHYR) | is.na(value$BIRTHMO),
                yes = NA,
                no = paste(value$BIRTHYR, value$BIRTHMO, 15, sep = "-")
              )),
            unit = "years"
          )
        }

        value[,
          names(.SD) := lapply(.SD, as.numeric),
          .SDcols = is.logical
        ]

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
      # "VISITYR",
      # "VISITMO",
      # "VISITDAY",
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
