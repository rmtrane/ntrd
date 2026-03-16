# Plot Module

UI for displaying plots of standardized scores across visits.

Server logic to handle the creation and updating of plots to show
standardized scores across visits.

Shiny app using the plotUI and plotServer.

## Usage

``` r
plotUI(id)

plotServer(
  id = "Attention/Processing",
  dat,
  x_range = c("2016-05-01", "2020-08-01"),
  y_range = c(-2.5, 2.5),
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values = calc_fill_colors(n = 7),
  print_updating = T,
  shade_descriptions = TRUE,
  new_id = NULL
)

plotApp(dat_input = prepare_data(demo_data), studyids = NULL, testing = FALSE)
```

## Arguments

- id:

  A single string. Optional, defaults to `"Attention/Processing"`.
  Should be one of `nacc_var_groups`.

- dat:

  A data.table. Should be participant specific.

- x_range:

  A date range as a character vector with two elements.

- y_range:

  A numeric vector with two elements. Optional, defaults to
  `c(-2.5, 2.5)`.

- descriptions:

  A named numeric vector. Optional, defaults to the vector described in
  the function definition.

- fill_values:

  A vector of fill colors. Optional, defaults to
  `calc_fill_colors(n = 7)`.

- print_updating:

  A logical value. Optional, defaults to `TRUE`.

- shade_descriptions:

  A logical value indicating if the plots should be shaded according to
  the regions given by `descriptions` with colors given by
  `fill_values`. Optional, defaults to `TRUE`.

- new_id:

  Optional. String to be used for table ID. If `NULL` (default), random
  string assigned.

- dat_input:

  A data frame. Defaults to `prepare_data(demo_data)`.

- studyids:

  Optional. If `NULL` (default), will use unique values from
  `dat_input$NACCID`.

- testing:

  Logical, whether to run the app in testing mode.

## Value

A
[`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
object containing two
[`shiny::conditionalPanel`](https://rdrr.io/pkg/shiny/man/conditionalPanel.html)'s:
one with the plot, and one with text for when no standardized scores
were found. Both wrapped in a
[`bslib::accordion_panel`](https://rstudio.github.io/bslib/reference/accordion.html)
for inclusion in main app.

No return value.

A shiny app.
