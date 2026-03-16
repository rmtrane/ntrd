test_that("demo_data is a data.table", {
  expect_true(data.table::is.data.table(demo_data))
})

test_that("demo_data has required columns for data_nacc", {
  required <- c("NACCID", "SEX", "EDUC", "BIRTHYR")
  expect_true(all(required %in% colnames(demo_data)))
})

test_that("demo_data can be used to construct data_nacc", {
  obj <- data_nacc(data = demo_data)
  expect_true(S7::S7_inherits(obj, data_nacc))
})

test_that("crosswalk_translations is a named character vector", {
  expect_type(crosswalk_translations, "character")
  expect_true(!is.null(names(crosswalk_translations)))
})

test_that("visibility_defaults is a list", {
  expect_type(visibility_defaults, "list")
  expect_true(length(visibility_defaults) > 0)
})

test_that("nacc_var_groups is a named character vector", {
  expect_type(nacc_var_groups, "character")
  expect_true(!is.null(names(nacc_var_groups)))
})

test_that("critical_vars is a character vector", {
  expect_type(critical_vars, "character")
  expect_true(length(critical_vars) > 0)
})

test_that("diag_contr_pairs has expected columns", {
  expected_cols <- c("presump_etio_diag", "contribution", "other", "disease")
  expect_true(all(expected_cols %in% colnames(diag_contr_pairs)))
})

test_that("rdd is a list", {
  expect_type(rdd, "list")
  expect_true(length(rdd) > 0)
})
