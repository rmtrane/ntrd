# Add Shiny resources

Adds resource paths for Shiny applications to access static files.

## Usage

``` r
shinyAddResources(development)
```

## Arguments

- development:

  logical; indicading if run in development mode. If `TRUE`, resources
  will be pulled from `inst/www` and `inst/qmd`. Otherwise, the path for
  the installed package will be used.

## Value

No return value. This function is called for its side effects of adding
resource paths for Shiny applications, mapping `"www"` and `"qmd"` to
their respective directories within the package installation or
development environment.
