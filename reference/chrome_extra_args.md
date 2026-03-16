# Return Chrome CLI arguments

This is a helper function which returns arguments to be passed to
Chrome. This function tests whether the code is running on shinyapps and
returns the appropriate Chrome extra arguments. Source:
https://github.com/RLesur/chrome_print_shiny

## Usage

``` r
chrome_extra_args(default_args = c("--disable-gpu"))
```

## Arguments

- default_args:

  Arguments to be used in any circumstances.

## Value

A character vector with CLI arguments to be passed to Chrome.
