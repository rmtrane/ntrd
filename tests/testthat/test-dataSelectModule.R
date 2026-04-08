# --- dataSelectModule tests ---

# ---------- dataSelectUI tests ----------

test_that("dataSelectUI returns a bslib card", {
  ui <- dataSelectUI("test")
  expect_s3_class(ui, "bslib_fragment")

  ui_html <- paste(as.character(ui), collapse = "")
  expect_true(grepl("test-data_source_selector", ui_html))
  expect_true(grepl("test-data_source_ui", ui_html))
})

# ---------- Go button with demo_source ----------

test_that("dataSelectServer Go button loads demo data as data_nacc", {
  tmp_config_dir <- tempfile("ntrd_config_")
  dir.create(tmp_config_dir, recursive = TRUE)
  on.exit(unlink(tmp_config_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    R_user_dir = function(...) tmp_config_dir,
    .package = "tools"
  )

  shiny::testServer(
    dataSelectServer,
    args = list(),
    {
      # Select demo source
      session$setInputs(data_source = "demo")
      session$flushReact()

      # Before clicking Go, dat_obj should be NULL
      result <- session$getReturned()
      expect_null(result$dat_obj())

      # Click Go — this triggers data_load(demo_source(), <empty params>)
      session$setInputs(go = 1)
      session$flushReact()

      # dat_obj should now be a data_nacc object
      loaded <- result$dat_obj()
      expect_true(S7::S7_inherits(loaded, data_nacc))

      # The data slot should be a data.table with rows
      expect_s3_class(loaded@data, "data.table")
      expect_true(nrow(loaded@data) > 0)

      # Should contain expected NACC columns
      expect_true("NACCID" %in% colnames(loaded@data))
      expect_true("VISITDATE" %in% colnames(loaded@data))

      # default_methods should also be populated
      defaults <- result$default_methods()
      expect_type(defaults, "list")
      expect_true(length(defaults) > 0)
    }
  )
})

# ---------- Config save/load round-trip (no Shiny) ----------

test_that("safer round-trips a config list through save/retrieve", {
  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp), add = TRUE)

  params <- list(path = "/some/path", sheet = "Sheet1", extra = 42L)
  safer::save_object(params, key = "testpass", conn = tmp)

  expect_true(file.exists(tmp))

  result <- safer::retrieve_object(tmp, key = "testpass")
  expect_equal(result, params)
})

test_that("safer retrieve_object errors on wrong password", {
  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp), add = TRUE)

  safer::save_object(list(a = 1), key = "correct", conn = tmp)

  expect_error(safer::retrieve_object(tmp, key = "wrong"))
})

# ---------- Config save/load via testServer ----------
#
# The "Save Configuration" and "Load saved configuration" buttons only
# appear when the active data_source_server returns a `restore` element.
# Built-in sources (demo, csv) don't have this, so we register a fake
# extension source for testing.

# --- Fake extension source with restore support ---
fake_source <- new_data_source(
  name = "Fake (test)",
  id = "fake_test",
  package = "ntrd"
)

S7::method(data_source_ui, fake_source) <- function(source, ns) {
  shiny::tagList(
    shiny::textInput(ns("my_param"), label = "Param", value = "default_val")
  )
}

S7::method(data_source_server, fake_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    restored <- shiny::reactiveVal(NULL)

    params <- shiny::reactive({
      if (!is.null(restored())) {
        return(restored())
      }
      list(my_param = input$my_param)
    })

    restore <- function(saved_params) {
      restored(saved_params)
      shiny::updateTextInput(
        session = session,
        inputId = "my_param",
        value = saved_params$my_param
      )
    }

    list(params = params, restore = restore)
  })
}

S7::method(data_load, fake_source) <- function(source, my_param) {
  # Return a valid data_nacc to keep the module happy
  data_nacc(data = data.table::copy(demo_data))
}

test_that("dataSelectServer save/load config round-trips via testServer", {
  # Use a temp config dir to avoid polluting the real one
  tmp_config_dir <- tempfile("ntrd_config_")
  dir.create(tmp_config_dir, recursive = TRUE)
  on.exit(unlink(tmp_config_dir, recursive = TRUE), add = TRUE)

  # Temporarily override R_user_dir to return our temp dir
  local_mocked_bindings(
    R_user_dir = function(...) tmp_config_dir,
    .package = "tools"
  )

  shiny::testServer(
    dataSelectServer,
    args = list(),
    {
      # Select the fake source
      session$setInputs(data_source = "fake_test")
      session$flushReact()

      # The config file reactive should point into our temp dir
      expect_true(grepl("fake_test\\.bin$", config_file()))
      expect_false(file.exists(config_file()))

      # Set the param value in the child module (hyphen namespace)
      do.call(
        session$setInputs,
        setNames(
          list("my_custom_value"),
          "fake_test-my_param"
        )
      )
      session$setInputs(
        config_password = "secret123",
        config_password_repeated = "secret123"
      )

      # Trigger save
      session$setInputs(launch_save_config = 1)
      session$flushReact()
      session$setInputs(save_config = 1)
      session$flushReact()

      # Config file should now exist on disk
      expect_true(file.exists(config_file()))

      # Verify the saved content directly
      saved <- safer::retrieve_object(config_file(), key = "secret123")
      expect_equal(saved$my_param, "my_custom_value")

      # Now simulate loading back: change the param to something else first
      do.call(
        session$setInputs,
        setNames(
          list("changed_value"),
          "fake_test-my_param"
        )
      )
      session$flushReact()

      # Trigger load
      session$setInputs(load_config = 1)
      session$flushReact()
      session$setInputs(
        config_password = "secret123",
        retrieve_config = 1
      )
      session$flushReact()

      # After restore, the server's params should reflect the saved value
      srv <- data_source_servers[["fake_test"]]
      expect_equal(srv$params()$my_param, "my_custom_value")
    }
  )
})

test_that("dataSelectServer load config handles wrong password gracefully", {
  tmp_config_dir <- tempfile("ntrd_config_")
  dir.create(tmp_config_dir, recursive = TRUE)
  on.exit(unlink(tmp_config_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    R_user_dir = function(...) tmp_config_dir,
    .package = "tools"
  )

  shiny::testServer(
    dataSelectServer,
    args = list(),
    {
      session$setInputs(data_source = "fake_test")
      session$flushReact()

      # Save a config
      do.call(
        session$setInputs,
        setNames(
          list("saved_val"),
          "fake_test-my_param"
        )
      )
      session$setInputs(
        config_password = "goodpass",
        config_password_repeated = "goodpass"
      )
      session$setInputs(launch_save_config = 1)
      session$flushReact()
      session$setInputs(save_config = 1)
      session$flushReact()

      expect_true(file.exists(config_file()))

      # Attempt to load with wrong password — should not crash
      session$setInputs(load_config = 1)
      session$flushReact()
      session$setInputs(
        config_password = "wrongpass",
        retrieve_config = 1
      )

      # Should not error — the tryCatch shows a notification instead
      expect_no_error(session$flushReact())
    }
  )
})

test_that("dataSelectServer password mismatch check renders error message", {
  tmp_config_dir <- tempfile("ntrd_config_")
  dir.create(tmp_config_dir, recursive = TRUE)
  on.exit(unlink(tmp_config_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    R_user_dir = function(...) tmp_config_dir,
    .package = "tools"
  )

  shiny::testServer(
    dataSelectServer,
    args = list(),
    {
      session$setInputs(data_source = "fake_test")
      session$flushReact()

      # Set mismatched passwords
      session$setInputs(
        config_password = "abc",
        config_password_repeated = "xyz"
      )
      session$flushReact()

      # The check_passwords output should contain the mismatch warning
      check_html <- paste(as.character(output$check_passwords), collapse = "")
      expect_true(grepl("don.*t match", check_html, ignore.case = TRUE))
    }
  )
})

test_that("dataSelectServer password match renders save button", {
  tmp_config_dir <- tempfile("ntrd_config_")
  dir.create(tmp_config_dir, recursive = TRUE)
  on.exit(unlink(tmp_config_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    R_user_dir = function(...) tmp_config_dir,
    .package = "tools"
  )

  shiny::testServer(
    dataSelectServer,
    args = list(),
    {
      session$setInputs(data_source = "fake_test")
      session$flushReact()

      # Set matching passwords
      session$setInputs(
        config_password = "same",
        config_password_repeated = "same"
      )
      session$flushReact()

      check_html <- paste(as.character(output$check_passwords), collapse = "")
      expect_true(grepl("Save Data Sources", check_html))
    }
  )
})

# ---------- apply_extension_defaults tests ----------

test_that("apply_extension_defaults is a no-op for ntrd package", {
  result <- apply_extension_defaults("ntrd")
  expect_false(result)
})

test_that("apply_extension_defaults calls .set_defaults when present", {
  # Create a fake namespace with .set_defaults
  fake_ns <- new.env(parent = emptyenv())
  fake_ns$.set_defaults <- function() {
    fake_ns$.was_called <- TRUE
  }
  fake_ns$.was_called <- FALSE

  local_mocked_bindings(
    asNamespace = function(ns, ...) {
      if (ns == "fakepkg") fake_ns else base::asNamespace(ns, ...)
    },
    .package = "base"
  )

  result <- apply_extension_defaults("fakepkg")
  expect_true(result)
  expect_true(fake_ns$.was_called)
})

test_that("apply_extension_defaults returns FALSE when .set_defaults missing", {
  fake_ns <- new.env(parent = emptyenv())

  local_mocked_bindings(
    asNamespace = function(ns, ...) {
      if (ns == "nopkg") fake_ns else base::asNamespace(ns, ...)
    },
    .package = "base"
  )

  result <- apply_extension_defaults("nopkg")
  expect_false(result)
})

test_that("apply_extension_defaults ignores non-function .set_defaults", {
  fake_ns <- new.env(parent = emptyenv())
  fake_ns$.set_defaults <- "not a function"

  local_mocked_bindings(
    asNamespace = function(ns, ...) {
      if (ns == "badpkg") fake_ns else base::asNamespace(ns, ...)
    },
    .package = "base"
  )

  result <- apply_extension_defaults("badpkg")
  expect_false(result)
})

# ---------- dataSelectApp tests (shinytest2) ----------

test_that("dataSelectApp launches and shows data source selector", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    dataSelectApp(testing = TRUE),
    name = "dataSelectApp-init",
    timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  # Data source selector should be rendered
  vals <- app$get_values(input = TRUE)$input
  expect_true("dat_select-data_source" %in% names(vals))

  # Select demo and click Go
  app$set_inputs(`dat_select-data_source` = "demo")
  app$wait_for_idle()

  # The Go button should be present in the UI
  html <- app$get_html("#dat_select-data_source_ui")
  expect_true(grepl("Go", html))

  # Hit the Go button
  app$click(selector = "#dat_select-go")
  app$wait_for_idle(timeout = 20000)

  # dat_obj should now be populated via exportTestValues
  test_vals <- app$get_values(export = TRUE)$export
  loaded <- test_vals$dat_obj

  expect_true(S7::S7_inherits(loaded, data_nacc))
  expect_s3_class(loaded@data, "data.table")
  expect_true(nrow(loaded@data) > 0)
  expect_true("NACCID" %in% colnames(loaded@data))
  expect_true("VISITDATE" %in% colnames(loaded@data))

  # Should match the demo data produced by data_load(demo_source())
  expected <- data_load(demo_source())
  expect_equal(loaded@data, expected@data)
})
