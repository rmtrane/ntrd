get_y_range <- function(dat) {
  if (!data.table::is.data.table(dat)) {
    cli::cli_abort(
      "{.arg dat} must be a {.cls data.table}, not {.cls {class(dat)}}."
    )
  }

  # Are there any non-missing values for this group of variables?
  dat <- dat[,
    lapply(.SD, \(x) if (any(!is.na(x))) x)
  ]

  ## If no columns are left...
  if (ncol(dat) == 0) {
    ## ... set range
    return(c(-2.5, 2.5))
  }

  ## If at least some obs, first check if any T-scores
  t_score_cols <- names(which(
    lapply(dat, \(x) attributes(x)$method) == "tscores"
  ))

  ## Change T-scores to z-values
  for (col in t_score_cols) {
    dat[[col]] <- (dat[[col]] - 50) / 10
  }

  ## Get most extreme values for all std_* columns
  most_extreme_val <- dat[,
    setNames(
      nm = names(.SD),
      object = lapply(.SD, \(x) {
        if (all(is.na(x))) {
          return(NA)
        }

        max(abs(x), na.rm = T)
      })
    )
  ]

  most_extreme_val <- max(most_extreme_val, na.rm = T)

  c(min(-2.5, -most_extreme_val), max(2.5, most_extreme_val))
}
