# Shiny Module to Display Longitudinal Trends in Table

Table version of the plotCogVarModule.

A short description...

## Usage

``` r
longTableUI(id)

longTableServer(
  id,
  dat,
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  print_updating = F
)

longTableApp(
  dat,
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  print_updating = F
)
```

## Arguments

- id:

  string to tie UI to Server

- dat:

  A data.table object.

- descriptions:

  A named numeric vector. Default has names `"Impaired"`,
  `"Borderline"`, `"Low Average"`, `"Average"`, `"High Average"`,
  `"Superior"`, and `"Very Superior"` with corresponding values `0.03`,
  `0.10`, `0.26`, `0.76`, `0.92`, `0.97`, and `1`.

- fill_values:

  Optional.

- methods:

  Either a list containing methods used for standardization (each a
  character vector with named entried `method` and `version`), or
  `"infer"` (default). If `"infer"`, then methods are pulled from
  attributes of standardized columns.

- table_font_size:

  A numeric value passed to
  `gt::tab_options(table.font.size = gt::pct(table_font_size))`.
  Defaults to 100.

- print_updating:

  logical (default `FALSE`); should message be printed to let user know
  the table is updating. For debugging.

## Value

A shiny app.
