# Suppress the entire `<thead>` (spanner row + column labels).

Equivalent to gt's `tab_options(column_labels.hidden = TRUE)` when no
spanners are present. For demographics-style key/value tables.

## Usage

``` r
ht_hide_header(x)
```

## Arguments

- x:

  A `html_table`.
