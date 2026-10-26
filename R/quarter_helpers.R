quarter_floor <- function(d) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  qm <- ((m - 1L) %/% 3L) * 3L + 1L
  as.Date(sprintf("%d-%02d-01", y, qm))
}

quarter_ceiling <- function(d) {
  f <- quarter_floor(d)
  # if d already sits on a quarter boundary, ceiling == floor
  ifelse(d == f, f, quarter_floor(seq(f, by = "3 months", length.out = 2)[2]))
  # simpler: just add 3 months to floor when d != f
}
