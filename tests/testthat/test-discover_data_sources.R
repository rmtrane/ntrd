test_that("discover_data_sources returns a list", {
  result <- discover_data_sources()
  expect_type(result, "list")
})

test_that("discover_data_sources finds built-in demo source", {
  result <- discover_data_sources()
  expect_true("demo" %in% names(result))
})

test_that("discover_data_sources finds built-in csv source", {
  result <- discover_data_sources()
  expect_true("csv" %in% names(result))
})

test_that("all discovered sources inherit from data_source", {
  result <- discover_data_sources()

  for (nm in names(result)) {
    expect_true(
      S7::S7_inherits(result[[nm]], data_source),
      info = paste("Source", nm, "does not inherit from data_source")
    )
  }
})

test_that("all discovered sources have an id slot", {
  result <- discover_data_sources()

  for (nm in names(result)) {
    expect_type(result[[nm]]@id, "character")
    expect_equal(result[[nm]]@id, nm)
  }
})
