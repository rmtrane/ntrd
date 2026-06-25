# Build an HTML table (first-cut: structure only)

Constructs a `html_table` accumulator. Layered ops (column spanners,
hidden columns, etc.) are added via `ht_*` functions and the final
object is rendered with
[`ht_render()`](https://rmtrane.github.io/ntrd/reference/ht_render.md)
for use inside
[`shiny::renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html).

## Usage

``` r
ht_table(data, id = NULL, rowname_col = NULL, groupname_col = NULL)
```

## Arguments

- data:

  A data.frame / data.table.

- id:

  Optional string used as the `id=` attribute on the rendered `<table>`.

- rowname_col:

  Optional string naming the column to be rendered as the row stub
  (`<th scope="row">`).

- groupname_col:

  Optional string naming the column to be rendered as row group headers.

## Value

A `html_table` object.
