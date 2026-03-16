test_that("data_nacc can be created from a valid data.frame", {
  df <- data.frame(
    NACCID = c("A001", "A001"),
    VISITYR = c(2020, 2021),
    VISITMO = c(3, 6),
    VISITDAY = c(15, 20),
    SEX = c(1, 1),
    EDUC = c(16, 16),
    BIRTHYR = c(1950, 1950),
    BIRTHMO = c(6, 6)
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
})

test_that("data_nacc computes VISITDATE from year/month/day components", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 3,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6
  )

  obj <- data_nacc(data = df)
  expect_true("VISITDATE" %in% colnames(obj@data))
  expect_equal(obj@data$VISITDATE, as.Date("2020-03-15"))
})

test_that("data_nacc removes VISITYR/VISITMO/VISITDAY after creating VISITDATE", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 3,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6
  )

  obj <- data_nacc(data = df)
  expect_false("VISITYR" %in% colnames(obj@data))
  expect_false("VISITMO" %in% colnames(obj@data))
  expect_false("VISITDAY" %in% colnames(obj@data))
})

test_that("data_nacc computes NACCAGE from VISITDATE and BIRTHYR/BIRTHMO", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 6,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6
  )

  obj <- data_nacc(data = df)
  expect_true("NACCAGE" %in% colnames(obj@data))
  expect_equal(obj@data$NACCAGE, 70, tolerance = 0.1)
})

test_that("data_nacc preserves existing VISITDATE if present", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_equal(obj@data$VISITDATE, as.Date("2020-03-15"))
})

test_that("data_nacc coerces input to data.table", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_true(data.table::is.data.table(obj@data))
})

test_that("data_nacc validation fails for missing required columns", {
  df <- data.frame(
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950
  )

  expect_error(data_nacc(data = df), "NACCID")
})

test_that("data_nacc validation fails for non-character NACCID", {
  df <- data.frame(
    NACCID = 123,
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "NACCID")
})

test_that("data_nacc validation fails for invalid SEX values", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 3,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "SEX")
})

test_that("data_nacc accepts NA values for SEX", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = NA_real_,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
})

test_that("data_nacc handles NA visit date components", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = NA_real_,
    VISITMO = 3,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_true(is.na(obj@data$VISITDATE))
})

test_that("data_nacc converts logical columns to numeric", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70,
    SOME_FLAG = TRUE
  )

  obj <- data_nacc(data = df)
  expect_true(is.numeric(obj@data$SOME_FLAG))
})

# --- Additional setter tests ---

test_that("data_nacc accepts data.table input without re-coercion", {
  dt <- data.table::data.table(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 70
  )

  obj <- data_nacc(data = dt)
  expect_true(data.table::is.data.table(obj@data))
  expect_equal(obj@data$NACCID, "A001")
})

test_that("data_nacc preserves existing NACCAGE when already present", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 6,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 99
  )

  obj <- data_nacc(data = df)
  expect_equal(obj@data$NACCAGE, 99)
})

test_that("data_nacc computes NACCAGE as NA when BIRTHYR is NA", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 6,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = NA_real_,
    BIRTHMO = 6
  )

  obj <- data_nacc(data = df)
  expect_true(is.na(obj@data$NACCAGE))
})

test_that("data_nacc computes NACCAGE as NA when BIRTHMO is NA", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 6,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = NA_real_
  )

  obj <- data_nacc(data = df)
  expect_true(is.na(obj@data$NACCAGE))
})

test_that("data_nacc handles partial visit date components", {
  # Only VISITYR present, no VISITMO or VISITDAY
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  # VISITDATE should be created as NA (missing VISITMO and VISITDAY)
  expect_true(is.na(obj@data$VISITDATE))
  # VISITYR should still be removed
  expect_false("VISITYR" %in% colnames(obj@data))
})

test_that("data_nacc handles multiple rows with mixed NA visit components", {
  df <- data.frame(
    NACCID = c("A001", "A001", "A001"),
    VISITYR = c(2020, NA, 2022),
    VISITMO = c(3, 6, NA),
    VISITDAY = c(15, 20, 10),
    SEX = c(1, 1, 1),
    EDUC = c(16, 16, 16),
    BIRTHYR = c(1950, 1950, 1950),
    BIRTHMO = c(6, 6, 6)
  )

  obj <- data_nacc(data = df)
  expect_equal(obj@data$VISITDATE[1], as.Date("2020-03-15"))
  expect_true(is.na(obj@data$VISITDATE[2]))
  expect_true(is.na(obj@data$VISITDATE[3]))
})

test_that("data_nacc converts multiple logical columns to numeric", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70,
    FLAG_A = TRUE,
    FLAG_B = FALSE
  )

  obj <- data_nacc(data = df)
  expect_true(is.numeric(obj@data$FLAG_A))
  expect_true(is.numeric(obj@data$FLAG_B))
  expect_equal(obj@data$FLAG_A, 1)
  expect_equal(obj@data$FLAG_B, 0)
})

# --- Additional validator tests ---

test_that("data_nacc validation fails when multiple required columns are missing", {
  df <- data.frame(
    VISITDATE = as.Date("2020-03-15"),
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "NACCID")
})

test_that("data_nacc validation fails for non-numeric SEX", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = "Male",
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "SEX")
})

test_that("data_nacc validation fails for non-numeric EDUC", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = "sixteen",
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "EDUC")
})

test_that("data_nacc validation fails for non-numeric BIRTHYR", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = "1950",
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "BIRTHYR")
})

test_that("data_nacc accepts SEX = 2 as valid", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 2,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
  expect_equal(obj@data$SEX, 2)
})

test_that("data_nacc accepts mix of SEX = 1, 2, NA across rows", {
  df <- data.frame(
    NACCID = c("A001", "A002", "A003"),
    VISITDATE = as.Date(c("2020-03-15", "2020-04-15", "2020-05-15")),
    SEX = c(1, 2, NA),
    EDUC = c(16, 12, 18),
    BIRTHYR = c(1950, 1960, 1970),
    NACCAGE = c(70, 60, 50)
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
})

test_that("data_nacc validation fails for multiple rows with some invalid SEX", {
  df <- data.frame(
    NACCID = c("A001", "A002", "A003"),
    VISITDATE = as.Date(c("2020-03-15", "2020-04-15", "2020-05-15")),
    SEX = c(1, 3, 2),
    EDUC = c(16, 12, 18),
    BIRTHYR = c(1950, 1960, 1970),
    NACCAGE = c(70, 60, 50)
  )

  expect_error(data_nacc(data = df), "SEX")
})

test_that("data_nacc validation reports multiple problems", {
  df <- data.frame(
    NACCID = 123,
    VISITDATE = as.Date("2020-03-15"),
    SEX = 3,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  # Should mention both NACCID type and SEX value issues
  expect_error(data_nacc(data = df), "NACCID")
})

test_that("data_nacc works with extra columns beyond required ones", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70,
    MOCATOTS = 25,
    ANIMALS = 18,
    TRAILA = 35
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
  expect_true(all(c("MOCATOTS", "ANIMALS", "TRAILA") %in% colnames(obj@data)))
})

test_that("data_nacc handles empty data.frame with correct columns", {
  df <- data.frame(
    NACCID = character(0),
    VISITDATE = as.Date(character(0)),
    SEX = numeric(0),
    EDUC = numeric(0),
    BIRTHYR = numeric(0),
    NACCAGE = numeric(0)
  )

  obj <- data_nacc(data = df)
  expect_true(S7::S7_inherits(obj, data_nacc))
  expect_equal(nrow(obj@data), 0)
})

# --- Setter: VISITDATE preservation alongside components ---

test_that("data_nacc preserves VISITDATE and removes components when both present", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    VISITYR = 2020,
    VISITMO = 3,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  obj <- data_nacc(data = df)
  expect_equal(obj@data$VISITDATE, as.Date("2020-03-15"))
  expect_false("VISITYR" %in% colnames(obj@data))
  expect_false("VISITMO" %in% colnames(obj@data))
  expect_false("VISITDAY" %in% colnames(obj@data))
})

# --- Setter: NACCAGE computation edge cases ---

test_that("data_nacc computes NACCAGE as NA when VISITDATE is NA", {
  df <- data.frame(
    NACCID = "A001",
    VISITYR = NA_real_,
    VISITMO = NA_real_,
    VISITDAY = NA_real_,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1950,
    BIRTHMO = 6
  )

  obj <- data_nacc(data = df)
  expect_true(is.na(obj@data$VISITDATE))
  expect_true(is.na(obj@data$NACCAGE))
})

test_that("data_nacc computes NACCAGE correctly for non-trivial ages", {
  # Visit on 2020-01-15, birth approx 1955-09-15 → ~64.33 years
  df <- data.frame(
    NACCID = "A001",
    VISITYR = 2020,
    VISITMO = 1,
    VISITDAY = 15,
    SEX = 1,
    EDUC = 16,
    BIRTHYR = 1955,
    BIRTHMO = 9
  )

  obj <- data_nacc(data = df)
  # Birth date used: 1955-09-15, visit date: 2020-01-15
  expected_age <- lubridate::time_length(
    as.Date("2020-01-15") - as.Date("1955-09-15"),
    unit = "years"
  )
  expect_equal(obj@data$NACCAGE, expected_age)
})

test_that("data_nacc NACCAGE chain: all components NA → VISITDATE NA → NACCAGE NA", {
  df <- data.frame(
    NACCID = c("A001", "A002"),
    VISITYR = c(2020, NA),
    VISITMO = c(6, NA),
    VISITDAY = c(15, NA),
    SEX = c(1, 2),
    EDUC = c(16, 12),
    BIRTHYR = c(1950, 1960),
    BIRTHMO = c(6, 6)
  )

  obj <- data_nacc(data = df)
  # First row: valid date and age
  expect_false(is.na(obj@data$VISITDATE[1]))
  expect_false(is.na(obj@data$NACCAGE[1]))
  # Second row: all NA → both NA
  expect_true(is.na(obj@data$VISITDATE[2]))
  expect_true(is.na(obj@data$NACCAGE[2]))
})

# --- Validator: SEX boundary values ---

test_that("data_nacc validation fails for SEX = 0", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = 0,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "SEX")
})

test_that("data_nacc validation fails for negative SEX", {
  df <- data.frame(
    NACCID = "A001",
    VISITDATE = as.Date("2020-03-15"),
    SEX = -1,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  expect_error(data_nacc(data = df), "SEX")
})

# --- Validator: fmt_rows truncation ---

test_that("data_nacc validation truncates row listing when > 5 invalid rows", {
  df <- data.frame(
    NACCID = rep("A001", 8),
    VISITDATE = as.Date("2020-03-15") + 0:7,
    SEX = rep(5, 8),
    EDUC = rep(16, 8),
    BIRTHYR = rep(1950, 8),
    NACCAGE = rep(70, 8)
  )

  expect_error(data_nacc(data = df), "e\\.g\\.")
})

# --- Validator: multiple problems ---

test_that("data_nacc validation error includes all problems", {
  df <- data.frame(
    NACCID = 123,
    VISITDATE = as.Date("2020-03-15"),
    SEX = 3,
    EDUC = 16,
    BIRTHYR = 1950,
    NACCAGE = 70
  )

  err <- tryCatch(data_nacc(data = df), error = function(e) e)
  # Both NACCID type and SEX value problems should be reported
  expect_match(conditionMessage(err), "NACCID")
  expect_match(conditionMessage(err), "SEX")
})

test_that("data_nacc validation error lists all missing columns", {
  df <- data.frame(
    VISITDATE = as.Date("2020-03-15"),
    NACCAGE = 70
  )

  err <- tryCatch(data_nacc(data = df), error = function(e) e)
  expect_match(conditionMessage(err), "NACCID")
  expect_match(conditionMessage(err), "SEX")
  expect_match(conditionMessage(err), "EDUC")
  expect_match(conditionMessage(err), "BIRTHYR")
})
