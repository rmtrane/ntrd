test_that("demo_source has the expected metadata", {
  src <- demo_source()
  expect_true(S7::S7_inherits(src, data_source))
  expect_equal(src@name, "Demo NACC data")
  expect_equal(src@id, "demo")
  expect_equal(src@package, "ntrd")
})

test_that("data_source_ui returns a shiny tag", {
  ui <- data_source_ui(demo_source(), ns = shiny::NS("test"))
  expect_s3_class(ui, "shiny.tag")
})

test_that("data_source_server returns params and session", {
  shiny::testServer(
    app = function(id) data_source_server(demo_source(), id),
    expr = {
      expect_named(
        session$returned,
        c("params", "session"),
        ignore.order = TRUE
      )
      expect_equal(session$returned$params(), list())
    }
  )
})

test_that("data_load returns a data_nacc object with derived columns", {
  res <- data_load(demo_source(), params = list())

  expect_true(S7::S7_inherits(res, data_nacc))
  expect_true(
    all(c("REYTOTAL", "REYAREC", "FAS", "MOCACLOCK") %in% colnames(res@data))
  )
})
