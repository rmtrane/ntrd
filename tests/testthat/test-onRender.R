test_that("onRender returns x unchanged when jsCode is length 0", {
  widget <- list(jsHooks = list())
  result <- onRender(widget, character(0))

  expect_identical(result, widget)
})

test_that("onRender appends single jsCode to jsHooks$render", {
  widget <- list(jsHooks = list())
  result <- onRender(widget, "console.log('hello');")

  expect_length(result$jsHooks$render, 1)
  expect_equal(result$jsHooks$render[[1]]$code, "console.log('hello');")
})

test_that("onRender collapses multi-element character vector with newlines", {
  widget <- list(jsHooks = list())
  code <- c("var x = 1;", "var y = 2;", "console.log(x + y);")
  result <- onRender(widget, code)

  expect_equal(
    result$jsHooks$render[[1]]$code,
    paste(code, collapse = "\n")
  )
})

test_that("onRender stores data parameter in hook", {
  widget <- list(jsHooks = list())
  my_data <- list(key = "value", n = 42)
  result <- onRender(widget, "console.log(data);", data = my_data)

  expect_equal(result$jsHooks$render[[1]]$data, my_data)
})

test_that("onRender stores NULL data by default", {
  widget <- list(jsHooks = list())
  result <- onRender(widget, "console.log('test');")

  expect_null(result$jsHooks$render[[1]]$data)
})

test_that("multiple onRender calls accumulate hooks", {
  widget <- list(jsHooks = list())
  widget <- onRender(widget, "console.log(1);")
  widget <- onRender(widget, "console.log(2);")
  widget <- onRender(widget, "console.log(3);")

  expect_length(widget$jsHooks$render, 3)
  expect_equal(widget$jsHooks$render[[1]]$code, "console.log(1);")
  expect_equal(widget$jsHooks$render[[2]]$code, "console.log(2);")
  expect_equal(widget$jsHooks$render[[3]]$code, "console.log(3);")
})
