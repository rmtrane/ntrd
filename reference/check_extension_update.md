# Check whether an update is available for an extension package

Wraps the extension's exported `ntrd_update_available()` with caching,
error handling, and result validation. Safe to call from Shiny reactive
contexts; will never raise an error.

## Usage

``` r
check_extension_update(package, force = FALSE, ttl = 3600)
```

## Arguments

- package:

  Character string giving the extension package name.

- force:

  Logical. If `TRUE`, ignore any cached result and re-check. Default
  `FALSE`.

- ttl:

  Numeric. Cache time-to-live in seconds. Cached results older than
  `ttl` are refreshed on the next call. Default 3600 (one hour).

## Value

An `update_result` S7 object. If the extension does not opt in to
updates, or if the check errors, returns the safe
[`update_result()`](https://rmtrane.github.io/ntrd/reference/update_result.md)
default.
