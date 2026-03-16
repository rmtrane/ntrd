# Descriptions UI

UI to let the user select the groups used for coloring. Includes a table
where group labels can be specified, cutoffs set, and color selected.

A short description...

## Usage

``` r
descriptionsUI(id)

descriptionsServer(
  id,
  default_descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26,
    Average = 0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  default_fill_values = NULL
)

descriptionsApp(
  default_descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26,
    Average = 0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  testing = FALSE
)
```

## Arguments

- id:

  A string.

- default_descriptions:

  Optional. A named numeric vector with names for `Label` and values for
  `Upper_Bound`.

- default_fill_values:

  Optional. A named character vector with names for `Label` and values
  for colors (as HEX values)

- testing:

  Logical, whether to run the app in testing mode.

## Value

A UI definition.

A list containing `fill_values` and `descriptions`.
