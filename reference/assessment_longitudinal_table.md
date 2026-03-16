# Assessment Longitudinal Table

Create a table of raw scores across multiple visits colored by the
corresponding standardized scores.

## Usage

``` r
assessment_longitudinal_table(
  dat,
  id = "NACCID",
  date = "VISITDATE",
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values,
  methods = list(),
  table_font_size = 100,
  table_id = NULL,
  show_all_visits = TRUE,
  stubhead_label = NULL
)
```

## Arguments

- dat:

  A data.table object.

- id:

  A character string specifying a column in the data frame `dat`.
  Default is `"NACCID"`.

- date:

  A character string specifying a column in the data frame `dat`.
  Default is `"VISITDATE"`.

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

- table_id:

  Optional. ID given to the table. If not provided, random string
  assigned.

- show_all_visits:

  A boolean. If `TRUE` (default), all visits present in the data are
  shown in the table. If `FALSE`, only visits with at least one
  standardized score are presented.

- stubhead_label:

  Optional. Passed to `gt::tab_stubhead(label = stubhead_label)`.

## Value

A
[`shiny::HTML`](https://rstudio.github.io/htmltools/reference/HTML.html)
table created from a
[`gt::gt`](https://gt.rstudio.com/reference/gt.html) object.
