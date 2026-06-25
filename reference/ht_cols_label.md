# Relabel one or more columns.

Relabel one or more columns.

## Usage

``` r
ht_cols_label(x, ...)
```

## Arguments

- x:

  A `html_table`.

- ...:

  Named arguments mapping `column_name = label`. Labels may be strings
  or shiny tags (incl.
  [`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)).
