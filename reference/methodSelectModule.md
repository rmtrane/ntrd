# Shiny Module for Selecting Domains and Standardization Methods

This shiny module displays npsych_scores variables from the loaded data
and lets the user assign cognitive domains and standardization methods
using dropdown menus in an HTML table.

## Usage

``` r
methodSelectUI(id)

methodSelectServer(id, dat_obj, default_methods)

methodSelectApp(testing = FALSE)
```

## Arguments

- id:

  id to link shiny modules

- dat_obj:

  reactive data object containing the loaded data.

- default_methods:

  named list. Each entry should be named after a variable. The entry
  should be a named character vector with two elements: one named
  'method' to indicate the standardization method to use, and one named
  'version' to indicate the version to use for standardization.

- testing:

  logical; passed to `shiny::shinyApp(..., options = list(test.mode))`

## Value

A shiny app.
