# Data Selection Server Module

A short description...

## Usage

``` r
dataSelectServer(id)
```

## Arguments

- id:

  A single string representing the module's identifier.

## Value

A
[`shiny::moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
function that returns a list containing:

- `dat_obj`: A reactive value holding the loaded data, which is expected
  to be a `data_nacc` object.

- `data_source_extras`: A reactive value holding a list of additional
  data source parameters or values.

Errors will be raised if the loaded data is not a `data_nacc` object.
