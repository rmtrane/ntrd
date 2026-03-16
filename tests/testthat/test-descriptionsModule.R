test_that("descriptionsServer returns fill_values and descriptions reactives", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()

    expect_type(desc, "list")
    expect_true("fill_values" %in% names(desc))
    expect_true("descriptions" %in% names(desc))
  })
})

test_that("descriptionsServer default descriptions have correct initial values", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()

    # Default: 7 description groups
    expect_length(desc$descriptions(), 7)
    expect_length(desc$fill_values(), 7)

    # Check names match
    expect_equal(names(desc$descriptions()), names(desc$fill_values()))

    # Check names are the default labels
    expect_equal(
      names(desc$descriptions()),
      c(
        "Impaired", "Borderline", "Low Average", "Average",
        "High Average", "Superior", "Very Superior"
      )
    )

    # Upper bound values are proportions (0-1)
    expect_true(all(desc$descriptions() >= 0))
    expect_true(all(desc$descriptions() <= 1))

    # Last group should have upper bound of 1
    expect_equal(unname(desc$descriptions()[7]), 1)
  })
})

test_that("descriptionsServer fill_values are valid hex colors", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()
    expect_true(check_colors(desc$fill_values()))
  })
})

test_that("descriptionsServer accepts custom default_descriptions", {
  custom <- c("Low" = 0.25, "Medium" = 0.75, "High" = 1.00)

  shiny::testServer(
    descriptionsServer,
    args = list(default_descriptions = custom),
    {
      session$flushReact()

      desc <- session$getReturned()

      expect_length(desc$descriptions(), 3)
      expect_equal(names(desc$descriptions()), c("Low", "Medium", "High"))
      expect_equal(unname(desc$descriptions()), c(0.25, 0.75, 1.00))
    }
  )
})

test_that("descriptionsServer reset restores defaults", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()
    initial_desc <- desc$descriptions()

    # Simulate a cell edit: change first row Upper_Bound from 3 to 5
    session$setInputs(
      descriptions_cell_edit = list(row = 1, col = 2, value = "5")
    )
    session$flushReact()

    # Descriptions should have changed
    expect_false(identical(desc$descriptions(), initial_desc))

    # Reset
    session$setInputs(reset = 1)
    session$flushReact()

    # Should be back to initial
    expect_equal(desc$descriptions(), initial_desc)
  })
})

test_that("descriptionsServer cell edit updates descriptions", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()

    # Edit the first row's Upper_Bound to 10 (which is 10% → 0.10)
    session$setInputs(
      descriptions_cell_edit = list(row = 1, col = 2, value = "10")
    )
    session$flushReact()

    # The first description should now have upper bound 0.10
    expect_equal(unname(desc$descriptions()[1]), 0.10)
  })
})

test_that("descriptionsServer add_row adds a new description group", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()
    initial_count <- length(desc$descriptions())

    # Add a row
    session$setInputs(add_row = 1)
    session$flushReact()

    expect_length(desc$descriptions(), initial_count + 1)
    # New row should have label "New Group"
    expect_true("New Group" %in% names(desc$descriptions()))
  })
})

test_that("descriptionsServer remove row via cell click works", {
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()
    initial_count <- length(desc$descriptions())

    # Click on the Remove column (column index 5, 0-based) of the first row
    session$setInputs(
      descriptions_cell_clicked = list(row = 1, col = 5)
    )
    session$flushReact()

    expect_length(desc$descriptions(), initial_count - 1)
  })
})

test_that("descriptionsServer enforces max Upper_Bound of 100 on removal", {
  # If we remove the row with Upper_Bound = 100, the next highest should be
  # bumped to 100.
  shiny::testServer(descriptionsServer, {
    session$flushReact()

    desc <- session$getReturned()

    # Remove the last row (Very Superior, Upper_Bound = 100)
    n <- nrow(descriptions())
    session$setInputs(
      descriptions_cell_clicked = list(row = n, col = 5)
    )
    session$flushReact()

    # The new last description should have upper bound = 1 (100%)
    expect_equal(max(desc$descriptions()), 1)
  })
})

# --- Input validation tests ---

test_that("descriptionsServer errors on non-numeric default_descriptions", {
  expect_error(
    shiny::testServer(
      descriptionsServer,
      args = list(default_descriptions = c("a" = "x", "b" = "y")),
      {}
    ),
    "numeric"
  )
})

test_that("descriptionsServer errors on unnamed default_descriptions", {
  expect_error(
    shiny::testServer(
      descriptionsServer,
      args = list(default_descriptions = c(0.5, 1.0)),
      {}
    ),
    "named"
  )
})

test_that("descriptionsServer errors on invalid fill_values colors", {
  expect_error(
    shiny::testServer(
      descriptionsServer,
      args = list(
        default_descriptions = c("Low" = 0.5, "High" = 1.0),
        default_fill_values = c("Low" = "not_a_color", "High" = "also_bad")
      ),
      {}
    ),
    "hex"
  )
})

test_that("descriptionsServer errors on mismatched lengths", {
  expect_error(
    shiny::testServer(
      descriptionsServer,
      args = list(
        default_descriptions = c("Low" = 0.5, "High" = 1.0),
        default_fill_values = c("Low" = "#FF0000")
      ),
      {}
    ),
    "Length"
  )
})

test_that("descriptionsServer errors on mismatched names", {
  expect_error(
    shiny::testServer(
      descriptionsServer,
      args = list(
        default_descriptions = c("Low" = 0.5, "High" = 1.0),
        default_fill_values = c("Low" = "#FF0000", "Medium" = "#00FF00")
      ),
      {}
    ),
    "names"
  )
})
