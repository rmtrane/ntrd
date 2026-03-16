# Shiny Module to Display NACC T-Cog Neuropsychological Assessment Summary Table

Shiny Module to Display NACC T-Cog Neuropsychological Assessment Summary
Table

## Usage

``` r
mainTableUI(id)

mainTableServer(
  id,
  dat,
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values = NULL,
  methods = "infer",
  table_font_size = 100,
  include_caption = F,
  print_updating = F
)

mainTableApp(dat, testing = FALSE)
```

## Arguments

- id:

  An ID string to match module UI and server UI

- dat:

  Data to use. Must have exactly one row (data from one participant and
  one visit).

- descriptions:

  A named vector giving cutoffs to use for creating descriptions.

- fill_values:

  (optional) A named vector of same length (with same names) as
  `descriptions` with hex color values to use. If `NULL`, no colorcoding
  used. By default, evenly spread out colors from red through yellow to
  green are used.

- methods:

  (optional) either list of named entries specifying which model was
  used for standardizing cognitive scores, or the character string
  "infer". For the latter, methods are infered from the `dat` object
  using `NpsychBatteryNorms::methods_from_std_data`. If specified,
  footnotes are added to indicate the methods used.

- table_font_size:

  (optional) scalar indicating font size as a percentage.

- include_caption:

  Logical; should a caption be included above the table with age and
  IQCODEs at current visit?

- print_updating:

  logical (defualt `FALSE`); should a message be displayed when table is
  being updated? For debugging.

- testing:

  logical (default `FALSE`); should the app be run in testing mode?

## Value

a Shiny app object.
