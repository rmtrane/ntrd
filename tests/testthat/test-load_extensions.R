# --- load_extensions tests ---

test_that(".check_generic_conflicts runs without error or warning", {
  expect_no_warning(.check_generic_conflicts())
})

test_that("load_extensions runs without error and returns invisibly", {
  result <- load_extensions()
  expect_null(result)
})
