# Locations for [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)

Each returns an `ht_loc` object naming a region of the table. `rows`
arguments use non-standard evaluation: write expressions referencing the
original data columns (e.g., `rows = is.na(value)`).

## Usage

``` r
ht_cells_body(columns = NULL, rows = NULL)

ht_cells_stub(rows = NULL)

ht_cells_column_labels(columns = NULL)

ht_cells_stubhead()

ht_cells_row_groups(groups = NULL)

ht_cells_footnotes()
```

## Arguments

- columns:

  Character vector of column names, or `NULL` for all.

- rows:

  Expression evaluated against the data. May yield a logical vector,
  integer indices, or `NULL` for all rows.

- groups:

  Character vector of group names, or `NULL` for all.

## Value

An object inheriting from `ht_loc`.

## Details

Free variables in `rows` (e.g., a loop-local `desc`) are resolved
against the calling frame at the moment
[`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)
runs — not deferred to render time — so writing loops like
`for (desc in ...) ht_tab_style(..., rows = Description == desc)`
behaves as written.
