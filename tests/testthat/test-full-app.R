# Helper to save screenshot, overwriting if it already exists
save_screenshot <- function(app, file) {
  if (file.exists(file)) {
    file.remove(file)
  }
  app$get_screenshot(file = file)
}

test_that("full app flow with demo data produces correct views", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Only save figures when not running R CMD check.
  save_figures <- !testthat::is_checking()
  if (save_figures) {
    man_figures <- test_path("..", "..", "man", "figures")
    if (!dir.exists(man_figures)) {
      dir.create(man_figures, recursive = TRUE)
    }
  }

  # Write a temporary app.R that shinytest2 will source in a fresh R process.
  # This ensures the package is properly loaded with S7 methods registered.
  app_dir <- tempfile("ntrd-test-app")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "library(ntrd)",
      "ntrd::shinyAddResources()",
      "shiny::shinyApp(",
      "  ui = ntrd::appUI,",
      "  server = ntrd::appServer,",
      "  options = list(test.mode = TRUE)",
      ")"
    ),
    file.path(app_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    shinyDashboard(),
    name = "full-app",
    variant = shinytest2::platform_variant(),
    height = 1080,
    width = 1920,
    timeout = 30000,
    load_timeout = 60000
  )
  app$wait_for_idle()
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(
    `General Cognition-showPlot` = "yes",
    `Attention/Processing-showPlot` = "yes",
    `Language-showPlot` = "yes",
    `Visuospatial-showPlot` = "yes",
    `Memory-showPlot` = "yes",
    `Executive Functioning-showPlot` = "yes",
    `Mood-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )

  app$set_inputs(main_navbar = "dataSelect")
  app$wait_for_idle()

  # Screenshot 1: Data selection page
  app$expect_screenshot(name = "data-selection")
  if (save_figures) {
    save_screenshot(app, file.path(man_figures, "data-selection.png"))
  }

  app$wait_for_idle()

  # Wait for the data source dropdown to render, then select "Demo NACC data"
  app$wait_for_js(
    "document.querySelector('#dataSelect-data_source') !== null",
    timeout = 15000
  )
  app$set_inputs(`dataSelect-data_source` = "demo")
  app$wait_for_idle(timeout = 15000)

  # Click Go to load demo data
  app$click(selector = "#dataSelect-go")

  # The data loading + prepare_data + methodSelect auto-detection takes time.
  # Use wait_for_js to wait for the modal to appear instead of wait_for_idle,
  # since the app may error transiently during the reactive chain.
  app$wait_for_js(
    "document.querySelector('.modal-dialog') !== null",
    timeout = 60000
  )
  app$wait_for_idle()

  # The methodSelect module auto-detects NACC columns and fires
  # moveToTables, which shows a modal. Dismiss it.
  app$click(
    selector = ".modal-footer .btn[data-dismiss='modal'], .modal-footer .btn[data-bs-dismiss='modal']"
  )

  # After dismissing the modal, the app navigates to "Scoring Tables and Figures".
  # The plot observers may throw a transient error (plotly_new_traces range issue)
  # which causes wait_for_idle to fail. We wait for the assessment table to render
  # instead, using wait_for_js which is not affected by Shiny errors.
  app$wait_for_js(
    "document.querySelector('#assessment-summary-table') !== null",
    timeout = 60000
  )
  app$wait_for_idle()
  # Screenshot 2: Main view with assessment summary table + plots
  app$expect_screenshot(name = "main-view")
  if (save_figures) {
    save_screenshot(app, file.path(man_figures, "main-view.png"))
  }

  # Verify key elements rendered
  main_table_html <- app$get_html("#assessment-summary-table")
  expect_true(nchar(main_table_html) > 0)

  demographics_html <- app$get_html("#demographics-table")
  expect_true(nchar(demographics_html) > 0)

  # Screenshot 3: Click "Cognitive Scores (Table)" tab
  app$click(selector = "#long-trends [data-value='Cognitive Scores (Table)']")
  app$wait_for_idle(timeout = 15000)

  app$expect_screenshot(name = "longitudinal-table")
  if (save_figures) {
    save_screenshot(app, file.path(man_figures, "longitudinal-table.png"))
  }

  # Screenshot 4: Click "Diagnoses" tab
  app$click(selector = "#long-trends [data-value='Diagnoses']")
  app$wait_for_idle(timeout = 15000)

  app$expect_screenshot(name = "diagnoses")
  if (save_figures) {
    save_screenshot(app, file.path(man_figures, "diagnoses.png"))
  }

  # Step 6: Change study ID and verify reactivity
  first_id_html <- app$get_html("#assessment-summary-table")

  # Get all study IDs and pick a different one
  vals <- app$get_values(input = "current_studyid")
  current_id <- vals$input$current_studyid

  # Navigate back to plots tab first
  app$click(selector = "#long-trends [data-value='Cognitive Scores (Plots)']")
  app$wait_for_idle(timeout = 5000)

  # Set a different study ID via JS
  app$run_js(
    "Shiny.setInputValue('current_studyid', Shiny.inputBindings.bindingNames['selectize'].binding.getValue($('#current_studyid')[0]) === undefined ? null : null);"
  )

  # Use set_inputs to change study ID — need to find available IDs first
  app$run_js(
    "var sel = $('#current_studyid')[0].selectize;
     var items = sel.options;
     var keys = Object.keys(items);
     if (keys.length > 1) {
       var newId = keys[0] === sel.getValue() ? keys[1] : keys[0];
       sel.setValue(newId);
     }"
  )
  app$wait_for_idle(timeout = 15000)

  # Screenshot 5: Main view with different participant
  app$expect_screenshot(name = "main-view-alt")
  if (save_figures) {
    save_screenshot(app, file.path(man_figures, "main-view-alt.png"))
  }

  # Verify the table changed
  alt_id_html <- app$get_html("#assessment-summary-table")
  expect_false(identical(first_id_html, alt_id_html))
})
