#' Calculate Lumipulse pTau217/ABeta42 ratio
#'
#' @description
#' Take vectors of pTau217 and ABeta42 values and return the ratio according to FDA guidance.
#'
#' @param pTau217 A numeric vector of pTau217 values.
#' @param ABeta42 A numeric vector of ABeta42 values.
#'
#' @returns
#' A numeric vector representing the adjusted and rounded pTau217/ABeta42 ratio.
#' Values in `pTau217` are capped at 10000. Values in `ABeta42` are capped at 500
#' under specific conditions. Certain combinations of input values will result
#' in `NA` or `0.047` in the returned ratio. For more details, see [this FDA document](https://www.accessdata.fda.gov/cdrh_docs/reviews/K242706.pdf)
#'
#' @export
lumipulse_pTau217_ABeta42_ratio <- function(pTau217, ABeta42) {
  # See https://www.accessdata.fda.gov/cdrh_docs/reviews/K242706.pdf for reasoning behind these adjustments
  pTau217[pTau217 > 10000] <- 10000
  ABeta42[ABeta42 >= 500 & pTau217 <= 1.85] <- 500

  ratio <- pTau217 / ABeta42

  ratio[
    (pTau217 < 0.047 & ABeta42 < 12.8) |
      (pTau217 >= 500 & ABeta42 > 1.85)
  ] <- NA

  ratio[
    pTau217 < 0.047 & ABeta42 >= 12.8
  ] <- 0.047

  round(ratio, digits = 5)
}

#' Lumipulse ABeta42/ABeta40 ratio
#'
#' @description
#' Take vectors of ABeta40 and ABeta42 values and return the ratio.
#'
#' @param ABeta40 A numeric value or vector.
#' @param ABeta42 A numeric value or vector.
#'
#' @returns
#' A numeric vector representing the ABeta42/ABeta40 ratio. Values for `ABeta42` are
#' capped at 2200. Ratios will be `NA` if `ABeta42 < 38`, `ABeta40 < 158`, or `ABeta40 > 28450`.
#' For details, see [this FDA document](https://www.accessdata.fda.gov/cdrh_docs/pdf20/DEN200072.pdf).
#'
#' @export
lumipulse_ABeta42_ABeta40_ratio <- function(ABeta40, ABeta42) {
  # See https://www.accessdata.fda.gov/cdrh_docs/pdf20/DEN200072.pdf for details on adjustments made
  ABeta42[ABeta42 > 2200] <- 2200
  ratio <- ABeta42 / ABeta40

  ratio[ABeta42 < 38 | ABeta40 < 158 | ABeta40 > 28450] <- NA

  ratio
}


#' Categorize a ratio
#'
#' @description
#' A short description...
#'
#' @param ratio A single numeric value.
#' @param thresholds A data frame or list with `label`, `bin`, `min`, and `max` elements.
#' @param out One of `"label"` or `"bin"`. Optional.
#'
#' @returns
#' A single value (a label or a bin identifier) from the `thresholds` object. Returns `NA` if `ratio` falls outside the defined thresholds.
#'
#' @export
categorize_ratio <- function(ratio, thresholds, out = c("label", "bin")) {
  if (length(out) != 1) {
    cli::cli_abort(
      "{.arg out} must be either {.val label} or {.val bin}, but was {.val {out}}"
    )
  }

  for_out <- switch(out, label = thresholds$label, bin = thresholds$bin)

  for_out[.bincode(
    ratio,
    breaks = unique(c(thresholds$min, thresholds$max)),
    right = TRUE,
    include.lowest = TRUE
  )]
}

if (FALSE) {
  tibble::tibble(
    ratio = seq(0, 1, by = 0.00001)
  ) |>
    dplyr::mutate(
      label = categorize_ratio(ratio, thresholds, out = "label"),
      bin = categorize_ratio(ratio, thresholds, out = "bin")
    ) |>
    dplyr::filter(
      .by = bin,
      ratio %in% range(ratio)
    )
}
