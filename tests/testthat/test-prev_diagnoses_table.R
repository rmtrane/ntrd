test_that("error", {
  expect_error(
    prev_diagnoses_table(dat = data.frame()),
    regexp = "object must be a"
  )
})

test_that("prev_diagnoses_table works", {
  get_prepared_demo_data("NACC017767") |>
    prev_diagnoses_table() |>
    expect_snapshot()
})
