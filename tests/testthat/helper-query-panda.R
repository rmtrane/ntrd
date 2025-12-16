panda_access <- !inherits(
  try(
    httr2::req_perform(httr2::request("https://panda.medicine.wisc.edu")),
    silent = TRUE
  ),
  "try-error"
)

check_table_names <- function(panda_returns) {
  expect_equal(
    names(panda_returns),
    c(
      "Local Roche CSF - Sarstedt freeze 2, cleaned",
      "Local Roche CSF - Sarstedt freeze 3",
      "Local Roche CSF - Sarstedt freeze, cleaned",
      "NTK MultiObs - CSF analytes",
      "NTK2 MultiObs - CSF, 20230311",
      "MK6240_NFT_Rating",
      "NAV4694 Visual Ratings",
      "PIB Visual Rating 20180126",
      "HDX Plasma - pTau217",
      "Amprion - CSF a-Synuclein"
    )
  )
}

check_table_colnames <- \(table, table_name) {
  expected_colnames <- switch(
    table_name,
    "Local Roche CSF - Sarstedt freeze 2, cleaned" = c(
      "age_lp",
      "sample_date",
      "pTau_raw",
      "tTau_raw",
      "pTau_bin",
      "tTau_bin" #,
      # "ABeta42_raw",
      # "ABeta42_bin",
      # "pTau_ABeta42_raw",
      # "pTau_ABeta42_bin"
    ),
    "Local Roche CSF - Sarstedt freeze 3" = c(
      "age_lp",
      "sample_date",
      "pTau181_raw",
      "tTau_raw",
      "pTau181_bin",
      "tTau_bin",
      # "ABeta42_raw",
      # "ABeta42_bin",
      # "pTau_ABeta42_raw",
      # "pTau_ABeta42_bin",
      "ABeta42_gen1_raw",
      "ABeta42_gen1_bin",
      "pTau181_ABeta42_gen1_raw",
      "pTau181_ABeta42_gen1_bin",
      "ABeta42_gen2_raw",
      "ABeta42_gen2_bin",
      "pTau181_ABeta42_gen2_raw",
      "pTau181_ABeta42_gen2_bin"
    ),
    "Local Roche CSF - Sarstedt freeze, cleaned" = c(
      "age_lp",
      "sample_date",
      "ABeta42_raw",
      "pTau_raw",
      "tTau_raw",
      "pTau_ABeta42_raw",
      "ABeta42_bin",
      "pTau_bin",
      "tTau_bin",
      "pTau_ABeta42_bin"
    ),
    "NTK MultiObs - CSF analytes" = c(
      "view_lp_appts_lp_date",
      "age_lp",
      "pTau_raw",
      "ABeta42_40_raw",
      "pTau_ABeta42_raw",
      "ABeta42_40_bin",
      "pTau_ABeta42_bin",
      "pTau_bin"
    ),
    "NTK2 MultiObs - CSF, 20230311" = c(
      "LPdate",
      "age_lp",
      "pTau_raw",
      "ABeta42_40_raw",
      "pTau_ABeta42_raw",
      "ABeta42_40_bin",
      "pTau_ABeta42_bin",
      "pTau_bin"
    ),
    "MK6240_NFT_Rating" = c(
      "view_petscan_appts_petscan_date",
      "view_petscan_appts_age_at_appointment",
      "comment",
      "braak_1",
      "braak_2",
      "braak_3",
      "braak_4",
      "braak_5",
      "braak_6"
    ),
    "NAV4694 Visual Ratings" = c(
      "view_petscan_appts_petscan_date",
      "view_petscan_appts_age_at_appointment",
      "rating_0_1_2_3"
    ),
    "PIB Visual Rating 20180126" = c(
      "view_petscan_appts_petscan_date",
      "view_petscan_appts_age_at_appointment",
      "rating_0_1_2_3"
    ),
    "HDX Plasma - pTau217" = c(
      "enumber",
      "age_at_appointment",
      "obtained_date",
      "pTau217_plasma_raw",
      "pTau217_plasma_cat"
    )
  )

  expect_equal(
    colnames(table),
    expected_colnames,
    label = paste(
      "Column names for",
      table_name,
      "do not match expected values."
    )
  )
}
