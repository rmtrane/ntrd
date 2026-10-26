test_that("var_labels returns a data.frame", {
  result <- var_labels()
  expect_s3_class(result, "data.frame")
})

test_that("var_labels returns expected column names", {
  result <- var_labels()
  expected_cols <- c(
    "CDRGLOB",
    "MOCATOTS",
    "MOCBTOTS",
    "NACCMMSE",
    "TRAILA",
    "OTRAILA",
    "OTRLARR",
    "DIGFORCT",
    "DIGFORSL",
    "DIGBACCT",
    "DIGBACLS",
    "DIGIF",
    "DIGIFLEN",
    "DIGIB",
    "DIGIBLEN",
    "WAIS",
    "MINTTOTS",
    "BOSTON",
    "ANIMALS",
    "VEG",
    "UDSVERTN",
    "UDSVERFC",
    "UDSVERLC",
    "UDSBENTC",
    "UDSBENTD",
    "CRAFTVRS",
    "LOGIMEM",
    "MEMUNITS",
    "CRAFTURS",
    "CRAFTDVR",
    "CRAFTDRE",
    "REYTOTAL",
    "REYDLIST",
    "REY6REC",
    "REYDREC",
    "REYAREC",
    "TRAILB",
    "MOCACLOCK",
    "OTRAILB",
    "OTRLBRR",
    "NACCGDS",
    "CESDTOTAL"
  )
  expect_equal(sort(colnames(result)), sort(expected_cols))
})

test_that("var_labels incorporates CDRSUM into CDRGLOB label", {
  result <- var_labels(CDRSUM = 14)
  expect_match(result$CDRGLOB, "14")
  expect_match(result$CDRGLOB, "SOB")
})

test_that("var_labels incorporates trail making details", {
  result <- var_labels(TRAILARR = 0, TRAILALI = 24, TRAILBRR = 2, TRAILBLI = 20)
  expect_match(result$TRAILA, "0 errors")
  expect_match(result$TRAILA, "24/24 CL")
  expect_match(result$TRAILB, "2 errors")
  expect_match(result$TRAILB, "20/24 CL")
})

test_that("var_labels handles NA defaults gracefully", {
  result <- var_labels()
  expect_match(result$CDRGLOB, "NA")
  expect_s3_class(result, "data.frame")
})

test_that("var_labels computes retention percentages", {
  result <- var_labels(
    UDSBENTD = 14,
    UDSBENTC = 16,
    CRAFTVRS = 12,
    CRAFTDVR = 10,
    CRAFTURS = 20,
    CRAFTDRE = 15
  )
  # benson: floor(14/16 * 100) = 87
  expect_match(result$UDSBENTD, "87% retained")
  # craft verbatim: floor(10/12 * 100) = 83
  expect_match(result$CRAFTDVR, "83% retained")
  # craft paraphrase: floor(15/20 * 100) = 75
  expect_match(result$CRAFTDRE, "75% retained")
})

test_that("var_labels includes RAVLT trial scores", {
  result <- var_labels(
    REY1REC = 5,
    REY2REC = 7,
    REY3REC = 9,
    REY4REC = 10,
    REY5REC = 12
  )
  expect_match(result$REYTOTAL, "5,7,9,10,12")
})

test_that("var_labels works with npsych_scores and handles UDSBENRS correctly", {
  result <- var_labels(
    REY1REC = ntrs::REY1REC(5),
    REY2REC = ntrs::REY2REC(7),
    REY3REC = ntrs::REY3REC(9),
    REY4REC = ntrs::REY4REC(10),
    REY5REC = ntrs::REY5REC(12),
    UDSBENTD = ntrs::UDSBENTD(13),
    UDSBENTC = ntrs::UDSBENTC(15),
    UDSBENRS = ntrs::UDSBENRS(1)
  )

  expect_match(result$REYTOTAL, "5,7,9,10,12")
  expect_match(result$UDSBENTD, "Benson Delay.+86% retained; Recog = Yes.+")

  result <- var_labels(
    REY1REC = ntrs::REY1REC(5),
    REY2REC = ntrs::REY2REC(7),
    REY3REC = ntrs::REY3REC(9),
    REY4REC = ntrs::REY4REC(10),
    REY5REC = ntrs::REY5REC(12),
    UDSBENTD = ntrs::UDSBENTD(13),
    UDSBENTC = ntrs::UDSBENTC(15),
    UDSBENRS = ntrs::UDSBENRS(-4)
  )

  expect_match(result$UDSBENTD, "Benson Delay.+86% retained; Recog = NA.+")
})
