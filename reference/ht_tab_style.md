# Attach styles to specific cell locations.

Mirrors
[`gt::tab_style()`](https://gt.rstudio.com/reference/tab_style.html).
`style` is a named character vector of CSS properties (most easily built
with
[`ht_cell_text()`](https://rmtrane.github.io/ntrd/reference/ht_cell_text.md)
/
[`ht_cell_fill()`](https://rmtrane.github.io/ntrd/reference/ht_cell_fill.md)),
a raw CSS string (`"padding-left: 0;"`), or a list of any of those.
`locations` is a single `ht_cells_*()` object or a list of them.

## Usage

``` r
ht_tab_style(x, style, locations)
```

## Arguments

- x:

  A `html_table`.

- style:

  A style spec. See description.

- locations:

  One
  [`ht_cells_body()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  /
  [`ht_cells_stub()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  / etc., or a list of them.

## Details

`rows` expressions on body / stub locations are evaluated *here* against
`x$data`, not deferred to render time. Free variables in the expression
(e.g., a loop-local `desc`) are resolved against the caller's frame at
the moment `ht_tab_style()` runs, so loops like
`for (desc in ...) out <- ht_tab_style(out, ..., rows = X == desc)`
behave as written.

## Examples

``` r
if (FALSE) { # \dontrun{
ht_table(dat) |>
  ht_tab_style(
    style = ht_cell_text(weight = "bold", color = "red"),
    locations = ht_cells_body(columns = "value", rows = is.na(value))
  )
} # }
```
