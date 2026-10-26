test_that("data_source can be created with name, id, and package", {
  src <- data_source(name = "Test Source", id = "test_src", package = "ntrd")
  expect_true(S7::S7_inherits(src, data_source))
  expect_equal(src@name, "Test Source")
  expect_equal(src@id, "test_src")
  expect_equal(src@package, "ntrd")
})

test_that("new_data_source creates a class that inherits from data_source", {
  my_class <- new_data_source(name = "My Source", id = "my_source", package = "ntrd")
  instance <- my_class()
  expect_true(S7::S7_inherits(instance, data_source))
  expect_equal(instance@name, "My Source")
  expect_equal(instance@id, "my_source")
  expect_equal(instance@package, "ntrd")
})

test_that("new_data_source creates distinct classes for different ids", {
  class_a <- new_data_source(name = "Source A", id = "source_a", package = "ntrd")
  class_b <- new_data_source(name = "Source B", id = "source_b", package = "ntrd")

  instance_a <- class_a()
  instance_b <- class_b()

  expect_equal(instance_a@id, "source_a")
  expect_equal(instance_b@id, "source_b")
  expect_true(S7::S7_inherits(instance_a, data_source))
  expect_true(S7::S7_inherits(instance_b, data_source))
})

test_that("new_data_source instances can be constructed with no arguments", {
  my_class <- new_data_source(name = "No Args", id = "no_args", package = "ntrd")
  expect_no_error(my_class())
})
