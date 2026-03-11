#' Create Labels for Assessment Summary Table
#'
#' Helper to create row names for `assessment_summary_table` based on values of
#' various scores.
#'
#' @param CDRSUM Standard CDR sum of boxes
#' @param TRAILARR Number of commission errors for Trail Making Part A
#' @param TRAILALI Number of correct lines for Trail Making Part A
#' @param TRAILBRR Number of commission errors for Trail Making Part B
#' @param TRAILBLI Number of correct lines for Trail Making Part B
#' @param UDSBENRS Benson Complex Figure Recall - Recognized original stimulus among four options
#' @param UDSBENTD Total score for 10- to 15-minute delayed drawing of Benson figure
#' @param UDSBENTC Total Score for copy of Benson figure
#' @param REY1REC Rey AVLT Trail 1 total recall
#' @param REY2REC Rey AVLT Trail 2 total recall
#' @param REY3REC Rey AVLT Trail 3 total recall
#' @param REY4REC Rey AVLT Trail 4 total recall
#' @param REY5REC Rey AVLT Trail 5 total recall
#' @param REYTCOR Rey AVLT delayed recognition; total correct
#' @param REYFPOS Rey AVLT delayed recognition; total false positives
#' @param CRAFTURS Craft Story 21 Recall (Immediate) - Total story units recalled, paraphrase scoring
#' @param CRAFTVRS Craft Story 21 Recall (Immediate) - Total story units recalled, verbatim scoring
#' @param CRAFTDVR Craft Story 21 Recall (Delayed) - Total story units recalled, verbatim scoring
#' @param CRAFTDRE Craft Story 21 Recall (Delayed) - Total story units recalled, paraphrase scoring
#'
#' @examples
#' var_labels(
#'   CDRSUM = 14,
#'   TRAILARR = 0,
#'   TRAILALI = 24,
#'   TRAILBRR = 2,
#'   TRAILBLI = 24,
#'   UDSBENRS = 1,
#'   UDSBENTD = 14,
#'   UDSBENTC = 14,
#'   REY1REC = -4,
#'   REY2REC = -4,
#'   REY3REC = -4,
#'   REY4REC = -4,
#'   REY5REC = -4,
#'   REYTCOR = -4,
#'   REYFPOS = -4,
#'   CRAFTURS = 20,
#'   CRAFTVRS = 12,
#'   CRAFTDVR = 11,
#'   CRAFTDRE = 13
#' )
#'
#' @export
var_labels <- function(
  CDRSUM = NA,
  TRAILARR = NA,
  TRAILALI = NA,
  TRAILBRR = NA,
  TRAILBLI = NA,
  UDSBENRS = NA,
  UDSBENTD = NA,
  UDSBENTC = NA,
  REY1REC = NA,
  REY2REC = NA,
  REY3REC = NA,
  REY4REC = NA,
  REY5REC = NA,
  REYTCOR = NA,
  REYFPOS = NA,
  CRAFTURS = NA,
  CRAFTVRS = NA,
  CRAFTDVR = NA,
  CRAFTDRE = NA
) {
  # all_scores <- list(
  #   "CDRSUM" = ifelse(is.na(CDRSUM), "&mdash;", CDRSUM),
  #   "TRAILARR" = ifelse(is.na(TRAILARR), "&mdash;", TRAILARR),
  #   "TRAILALI" = ifelse(is.na(TRAILALI), "&mdash;", TRAILALI),
  #   "TRAILBRR" = ifelse(is.na(TRAILBRR), "&mdash;", TRAILBRR),
  #   "TRAILBLI" = ifelse(
  #     is.na(TRAILBLI),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       TRAILBLI,
  #       var_name = "TRAILBLI",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "UDSBENRS" = ifelse(
  #     is.na(UDSBENRS),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       UDSBENRS,
  #       var_name = "UDSBENRS",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "UDSBENTD" = ifelse(
  #     is.na(UDSBENTD),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       UDSBENTD,
  #       var_name = "UDSBENTD",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "UDSBENTC" = ifelse(
  #     is.na(UDSBENTC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       UDSBENTC,
  #       var_name = "UDSBENTC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REY1REC" = ifelse(
  #     is.na(REY1REC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REY1REC,
  #       var_name = "REY1REC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REY2REC" = ifelse(
  #     is.na(REY2REC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REY2REC,
  #       var_name = "REY2REC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REY3REC" = ifelse(
  #     is.na(REY3REC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REY3REC,
  #       var_name = "REY3REC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REY4REC" = ifelse(
  #     is.na(REY4REC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REY4REC,
  #       var_name = "REY4REC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REY5REC" = ifelse(
  #     is.na(REY5REC),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REY5REC,
  #       var_name = "REY5REC",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REYTCOR" = ifelse(
  #     is.na(REYTCOR),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REYTCOR,
  #       var_name = "REYTCOR",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "REYFPOS" = ifelse(
  #     is.na(REYFPOS),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       REYFPOS,
  #       var_name = "REYFPOS",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "CRAFTURS" = ifelse(
  #     is.na(CRAFTURS),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       CRAFTURS,
  #       var_name = "CRAFTURS",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "CRAFTVRS" = ifelse(
  #     is.na(CRAFTVRS),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       CRAFTVRS,
  #       var_name = "CRAFTVRS",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "CRAFTDVR" = ifelse(
  #     is.na(CRAFTDVR),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       CRAFTDVR,
  #       var_name = "CRAFTDVR",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "CRAFTDRE" = ifelse(
  #     is.na(CRAFTDRE),
  #     "&mdash;",
  #     NpsychBatteryNorms::valid_values_only(
  #       CRAFTDRE,
  #       var_name = "CRAFTDRE",
  #       remove_errorcodes = T
  #     )
  #   ),
  #   "craft_delay_ver_retain" = ifelse(
  #     CRAFTDVR %in% rdd$CRAFTDVR$codes,
  #     "&mdash;",
  #     floor(CRAFTDVR / CRAFTVRS * 100)
  #   ),
  #   "craft_delay_par_retain" = ifelse(
  #     CRAFTDRE %in% rdd$CRAFTDRE$codes,
  #     "&mdash;",
  #     floor(CRAFTDRE / CRAFTURS * 100)
  #   ),
  #   "benson_retained" = ifelse(
  #     UDSBENTD %in% rdd$UDSBENTD$codes,
  #     "&mdash;",
  #     floor(UDSBENTD / UDSBENTC * 100)
  #   ),
  #   "benson_recog" = ifelse(
  #     is.na(UDSBENRS) | UDSBENRS == -4,
  #     "&mdash;",
  #     NpsychBatteryNorms::values_to_labels(UDSBENRS, 'UDSBENRS')
  #   )
  # )

  inputs <- list(
    CDRSUM = CDRSUM,
    TRAILARR = TRAILARR,
    TRAILALI = TRAILALI,
    TRAILBRR = TRAILBRR,
    TRAILBLI = TRAILBLI,
    UDSBENRS = UDSBENRS,
    UDSBENTD = UDSBENTD,
    UDSBENTC = UDSBENTC,
    REY1REC = REY1REC,
    REY2REC = REY2REC,
    REY3REC = REY3REC,
    REY4REC = REY4REC,
    REY5REC = REY5REC,
    REYTCOR = REYTCOR,
    REYFPOS = REYFPOS,
    CRAFTURS = CRAFTURS,
    CRAFTVRS = CRAFTVRS,
    CRAFTDVR = CRAFTDVR,
    CRAFTDRE = CRAFTDRE
  )

  inputs <- sapply(
    inputs,
    \(x) {
      if (ntrs::is_npsych_scores(x)) {
        if (S7::S7_class(x)@name == "UDSBENRS") {
          x_num <- as.numeric(x)
          if (x_num %in% x@range) {
            return(names(which(x@codes == x_num)))
          } else {
            return(NA)
          }
        }

        return(ntrs::remove_error_codes(x))
      } else {
        return(x)
      }
    },
    simplify = FALSE
  )

  calc_inputs <- with(
    inputs,
    list(
      "craft_delay_ver_retain" = floor(CRAFTDVR / CRAFTVRS * 100),
      "craft_delay_par_retain" = floor(CRAFTDRE / CRAFTURS * 100),
      "benson_retained" = floor(UDSBENTD / UDSBENTC * 100)
    )
  )

  inputs <- c(
    inputs,
    calc_inputs
  )

  with(
    inputs,
    data.frame(
      ## General Cognition
      "CDRGLOB" = paste0("CDR Global (", CDRSUM, " SOB)"),
      "MOCATOTS" = "MoCA",
      "MOCBTOTS" = "MoCA Blind",
      "NACCMMSE" = "Mini Mental State Examination (MMSE)",
      # "cog_ticsm" = "TICS-m",
      ## Attention/Processing Speed
      "TRAILA" = paste0(
        "Trailmaking Part A (",
        TRAILARR,
        " errors; ",
        TRAILALI,
        "/24 CL)"
      ),
      "OTRAILA" = "Oral Trailmaking Part A - Completion Time",
      "OTRLARR" = "Oral Trailmaking Part A - Errors",
      "DIGFORCT" = "Number Span Forward - Total",
      "DIGFORSL" = "Number Span Forward - Span Length",
      "DIGBACCT" = "Number Span Backward - Total",
      "DIGBACLS" = "Number Span Backward - Span Length",
      "DIGIF" = "Digit Span Forward - Total",
      "DIGIFLEN" = "Digit Span Forward - Span Length",
      "DIGIB" = "Digit Span Backward - Total",
      "DIGIBLEN" = "Digit Span Backward - Span Length",
      "WAIS" = "WAIS-R Digit Symbol",
      ## Language
      "MINTTOTS" = "MINT",
      "BOSTON" = "Boston Naming Test",
      "ANIMALS" = "Animal Fluency",
      "VEG" = "Vegetable Fluency",
      "UDSVERTN" = "F+L Words",
      # "cog_flc_flu" = "F+L+C Words",
      "UDSVERFC" = "F Words",
      "UDSVERLC" = "L Words",
      ## Visuospatial
      "UDSBENTC" = "Benson Figure Copy",
      ## Memory
      "UDSBENTD" = paste0(
        "Benson Delay (",
        benson_retained,
        "% retained; Recog = ",
        UDSBENRS,
        ")"
      ),
      "CRAFTVRS" = "Craft Immediate - Verbatim",
      "LOGIMEM" = "Logical Memory IA, Immediate",
      "MEMUNITS" = "Logical Memory IIA, Delayed",
      "CRAFTURS" = "Craft Immediate - Paraphrase",
      "CRAFTDVR" = paste0(
        "Craft Delay - Verbatim (",
        craft_delay_ver_retain,
        "% retained)"
      ),
      "CRAFTDRE" = paste0(
        "Craft Delay - Paraphrase (",
        craft_delay_par_retain,
        "% retained)"
      ),
      "REYTOTAL" = paste0(
        "RAVLT Total Learning (",
        REY1REC,
        ",",
        REY2REC,
        ",",
        REY3REC,
        ",",
        REY4REC,
        ",",
        REY5REC,
        ")"
      ),
      "REYDLIST" = "RAVLT Distractor List",
      "REY6REC" = "RAVLT Short Delay",
      "REYDREC" = "RAVLT Long Delay",
      "REYAREC" = paste0(
        "RAVLT Recognition (Correct: ",
        REYTCOR,
        "/15, FP: ",
        REYFPOS,
        "/15)"
      ),
      ## Execute Functioning
      "TRAILB" = paste0(
        "Trailmaking Part B (",
        TRAILBRR,
        " errors; ",
        TRAILBLI,
        "/24 CL)"
      ),
      "MOCACLOCK" = "Clock Drawing Test",
      "OTRAILB" = "Oral Trailmaking Part B - Completion Time",
      "OTRLBRR" = "Oral Trailmaking Part B - Errors",
      ## Mood
      "NACCGDS" = "GDS-15 (Depression Symptoms)",
      "CESDTOTAL" = "CES-D (Total Score)"
    )
  )
}
