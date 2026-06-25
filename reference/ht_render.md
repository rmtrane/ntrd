# Render a `html_table` to `shiny::tags`

Wrap the final builder result with `ht_render()` before passing it to
[`shiny::renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html):

## Usage

``` r
ht_render(x)
```

## Arguments

- x:

  A `html_table`.

## Value

A `shiny.tag` (the `<table>` element).

## Details

    output$tbl <- shiny::renderUI(ht_render(my_table_fn(...)))
