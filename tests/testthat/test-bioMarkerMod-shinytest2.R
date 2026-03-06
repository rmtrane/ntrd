library(shinytest2)

test_that("biomarkerMod works", {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("panda_api_key")))

  all_values <- get_all_values(
    api_key = getOption("panda_api_key"),
    base_query_file = system.file(
      "json/panda_template.json",
      package = "ntrd"
    )
  )

  app <- AppDriver$new(
    app_dir = biomarkerApp(
      adrc_ptid = c("adrc00006", "adrc00031"),
      biomarker_api = shiny::reactive(getOption("panda_api_key")),
      all_values = shiny::reactive(all_values),
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "biomarkerMods-1",
    height = 968,
    width = 1619
  )

  app$expect_screenshot()

  app$wait_for_idle(timeout = 60000)

  app$expect_screenshot()

  app$set_inputs(
    current_studyid = "adrc00031",
    wait_ = TRUE
  )

  # app$wait_for_idle(timeout = 60000)

  app$expect_screenshot()

  app$set_inputs(
    current_studyid = "adrc00006",
    wait_ = FALSE
  )

  app$wait_for_idle(timeout = 60000)

  app$expect_screenshot()

  app$stop()
})
