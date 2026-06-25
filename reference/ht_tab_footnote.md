# Append a table-level footnote.

Rendered in a `<tfoot>` below the body. Call multiple times to add
multiple footnotes; each appears on its own row in input order.

## Usage

``` r
ht_tab_footnote(x, footnote)
```

## Arguments

- x:

  A `html_table`.

- footnote:

  String or shiny tag.

## Details

Markers tied to specific cells are not supported yet — `locations` (à la
`gt::tab_footnote(locations = cells_body(...))`) is on the roadmap for
the
[`assessment_summary_table()`](https://rmtrane.github.io/ntrd/reference/assessment_summary_table.md)
migration.
