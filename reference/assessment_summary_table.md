# Create NACC T-Cog Neuropsychological Assessment Summary Table

This function create a summary table of measures from the NACC T-Cog
Neuropsychological Assessment.

## Usage

``` r
assessment_summary_table(summary_dat, bar_height = 16)

assessment_summary_data(
  dat,
  id = "NACCID",
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values = NULL,
  include_caption = TRUE
)
```

## Arguments

- summary_dat:

  A list as returned by `assessment_summary_data()`.

- bar_height:

  In pixels. Height of the percentile bars. Default: 16

- dat:

  Data to use. Must have exactly one row (data from one participant and
  one visit).

- id:

  Column containing ID.

- descriptions:

  A named vector giving cutoffs to use for creating descriptions.

- fill_values:

  (optional) A named vector of same length (with same names) as
  `descriptions` with hex color values to use. If `NULL`, no colorcoding
  used. By default, evenly spread out colors from red through yellow to
  green are used.

- include_caption:

  Logical; should a caption be included above the table with age and
  IQCODEs at current visit?

## Value

An object of class `gt::gt_tbl`
