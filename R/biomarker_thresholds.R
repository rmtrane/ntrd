#' Rules for Categorizing Biomarkers
#'
#' @format List of lists of lists such that `biomarker_thresholds[[table_name]][[biomarker]]` gives a list with entries `source` (list with `description` indicating where the categorization is described, and `link`), and `thresholds` (a `data.frame` with columns `label`, `min`, `max`, and `bin` describing categories).
"biomarker_thresholds"
