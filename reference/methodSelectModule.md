# Shiny Module for Selecting Columns

This shiny module gives the user the option to manually map column names
to NACC variable names.

A short description...

## Usage

``` r
methodSelectUI(id)

methodSelectServer(
  id,
  dat_obj,
  default_methods,
  col_selection = c("enable", "disable", "hide")
)

methodSelectApp(col_selection = "enable", testing = FALSE)
```

## Arguments

- id:

  id to link shiny modules

- dat_obj:

  reactive data object containing the loaded data.

- default_methods:

  named list. Each entry should be named after a variable. The entry
  should be a named character vector with to elements: one named
  'method' to indicate the standardization method to use, and one name
  'version' to indicate the version to use for standardization.

- col_selection:

  string; one of 'enable', 'disable', or 'hide'. If 'enable', allow user
  to select which columns should be used for each variable. If
  'disable', show columns used, but without the option to select. If
  'hide', hide the column.

- testing:

  logical; passed to `shiny::shinyApp(..., options = list(test.mode))`

## Value

A shiny app.
