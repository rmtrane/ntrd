# Clear the update-check cache

Primarily useful for testing and for a "check again" UI affordance.

## Usage

``` r
clear_update_cache(package = NULL)
```

## Arguments

- package:

  Optional package name. If supplied, only that package's cached result
  is cleared; otherwise the entire cache is cleared.

## Value

Invisible `NULL`.
