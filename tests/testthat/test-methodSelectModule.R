# --- methodSelectModule tests ---

# Shared setup helper to reduce repetition
local_method_test_data <- function() {
  dat_obj <- data_load(demo_source())
  default_methods <- get_default_methods()
  list(dat_obj = dat_obj, default_methods = default_methods)
}

# ---------- methodSelectUI tests ----------

test_that("methodSelectUI returns a tagList", {
  ui <- methodSelectUI("test")
  expect_s3_class(ui, "shiny.tag.list")

  ui_html <- as.character(ui)
  expect_true(grepl("test-assign", ui_html))
  expect_true(grepl("test-vars_table_output", ui_html))
})

test_that("methodSelectUI contains an Apply Selections button", {
  ui <- methodSelectUI("ms")
  ui_html <- as.character(ui)
  expect_true(grepl("Apply Selections", ui_html))
  expect_true(grepl("ms-assign", ui_html))
})

# ---------- methodSelectServer tests (testServer) ----------

test_that("methodSelectServer returns list with expected reactives", {
  td <- local_method_test_data()

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(td$default_methods)
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      expect_type(result, "list")
      expect_true("std_methods" %in% names(result))
      expect_true("domain_assignments" %in% names(result))
      expect_true("auto_applied" %in% names(result))
    }
  )
})

test_that("methodSelectServer auto-applies defaults when available", {
  td <- local_method_test_data()

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(td$default_methods)
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      # auto_applied should be TRUE when defaults exist
      expect_true(result$auto_applied())

      # std_methods should be populated
      expect_true(length(result$std_methods()) > 0)

      # domain_assignments should be a named character vector
      expect_type(result$domain_assignments(), "character")
      expect_true(length(result$domain_assignments()) > 0)
    }
  )
})

test_that("methodSelectServer does not auto-apply when no defaults", {
  td <- local_method_test_data()

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(list())
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      expect_false(result$auto_applied())
      expect_length(result$std_methods(), 0)
    }
  )
})

test_that("methodSelectServer apply button reads inputs and updates outputs", {
  td <- local_method_test_data()

  # Pick a variable name with defaults ahead of time
  var_name <- names(td$default_methods)[[1]]
  domain_input <- paste0(var_name, "_domain")

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(td$default_methods)
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      # Confirm auto-applied defaults
      expect_true(length(result$std_methods()) > 0)

      # Set the domain to "(exclude)" for the chosen variable, then apply
      args <- setNames(list("(exclude)"), domain_input)
      do.call(session$setInputs, args)

      session$setInputs(assign = 1)
      session$flushReact()

      new_domains <- result$domain_assignments()
      expect_true(is.na(new_domains[[var_name]]))
    }
  )
})

test_that("methodSelectServer apply button parses method with version", {
  td <- local_method_test_data()

  var_name <- names(td$default_methods)[[1]]
  method_input <- paste0(var_name, "_method")

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(td$default_methods)
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      # Simulate setting a method input with version format "method (version)"
      args <- setNames(list("heaton (2004)"), method_input)
      do.call(session$setInputs, args)

      # Click apply
      session$setInputs(assign = 1)
      session$flushReact()

      updated_methods <- result$std_methods()
      expect_equal(updated_methods[[var_name]]$method, "heaton")
      expect_equal(updated_methods[[var_name]]$version, "2004")
    }
  )
})

test_that("methodSelectServer apply button handles method without version", {

  td <- local_method_test_data()

  var_name <- names(td$default_methods)[[1]]
  method_input <- paste0(var_name, "_method")

  shiny::testServer(
    methodSelectServer,
    args = list(
      dat_obj = shiny::reactive(td$dat_obj),
      default_methods = shiny::reactive(td$default_methods)
    ),
    {
      session$flushReact()

      result <- session$getReturned()

      # Simulate setting a method input without version (just "method_name")
      args <- setNames(list("somemethod"), method_input)
      do.call(session$setInputs, args)

      session$setInputs(assign = 1)
      session$flushReact()

      updated_methods <- result$std_methods()
      expect_equal(updated_methods[[var_name]]$method, "somemethod")
      expect_null(updated_methods[[var_name]]$version)
    }
  )
})

# ---------- methodSelectApp tests (shinytest2) ----------

test_that("methodSelectApp initializes and auto-applies defaults", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    methodSelectApp(testing = TRUE),
    name = "methodSelectApp-defaults",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  test_vals <- app$get_values(export = TRUE)$export

  # std_methods should be auto-populated from defaults
  expect_true(length(test_vals$std_methods) > 0)
  expect_type(test_vals$std_methods, "list")

  # domain_assignments should be a named character vector
  expect_true(length(test_vals$domain_assignments) > 0)
  expect_type(test_vals$domain_assignments, "character")
})

test_that("methodSelectApp apply button updates exported values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    methodSelectApp(testing = TRUE),
    name = "methodSelectApp-apply",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Get initial values
  initial_vals <- app$get_values(export = TRUE)$export
  expect_true(length(initial_vals$std_methods) > 0)

  # Click the Apply Selections button
  app$click(selector = "#methodselect-assign")
  app$wait_for_idle()

  # Values should still be present after clicking apply
  updated_vals <- app$get_values(export = TRUE)$export
  expect_true(length(updated_vals$std_methods) > 0)
  expect_true(length(updated_vals$domain_assignments) > 0)
})

test_that("methodSelectApp changing domain and applying updates exports", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    methodSelectApp(testing = TRUE),
    name = "methodSelectApp-change-domain",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Get a variable name from the initial export to target
  initial_vals <- app$get_values(export = TRUE)$export
  var_name <- names(initial_vals$domain_assignments)[[1]]

  # Set the domain to "(exclude)" for the first variable
  input_id <- paste0("methodselect-", var_name, "_domain")
  app$set_inputs(!!input_id := "(exclude)")
  app$wait_for_idle()

  # Click Apply Selections
  app$click(selector = "#methodselect-assign")
  app$wait_for_idle()

  updated_vals <- app$get_values(export = TRUE)$export
  expect_true(is.na(updated_vals$domain_assignments[[var_name]]))
})
