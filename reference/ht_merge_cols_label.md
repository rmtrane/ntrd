# Merge column labels into one cell.

Replaces the individual `<th>`s for the named columns with a single
`<th colspan=N>` in the column-label row. Use this when several columns
share a unified label (e.g., a `"Raw"` header spanning a value column
and a units column).

## Usage

``` r
ht_merge_cols_label(x, label, columns, align = c("center", "left", "right"))
```

## Arguments

- x:

  A `html_table`.

- label:

  String or tag for the merged cell.

- columns:

  Character vector of column names to merge (length \>= 2).

- align:

  One of `"left"`, `"center"`, `"right"`. Default `"center"`.

## Details

Differs from
[`ht_tab_spanner()`](https://rmtrane.github.io/ntrd/reference/ht_tab_spanner.md)
in that a spanner adds a *new row* above the column labels, whereas a
merge collapses cells *within* the column-label row.

The columns to merge must be contiguous in the rendered table after
hidden columns are removed; non-contiguous merges are ignored with a
warning, since they would produce invalid markup.

Any individual labels set via
[`ht_cols_label()`](https://rmtrane.github.io/ntrd/reference/ht_cols_label.md)
for columns inside the merge group are overridden by the merge's
`label`. Styles set via
[`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)
/
[`ht_cells_column_labels()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
on any of the covered columns combine onto the merged cell.

## Examples

``` r
if (FALSE) { # \dontrun{
ht_table(dat) |>
  ht_tab_spanner("Scores", c("raw", "raw_suffix", "units", "std")) |>
  ht_merge_cols_label("Raw", columns = c("raw", "raw_suffix"))
} # }
```
