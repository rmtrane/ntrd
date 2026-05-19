# Validate (or coerce) the result of an `ntrd_update_available()` call

Accepts either an `update_result` S7 instance (the canonical form) or a
plain list with the expected fields (a courtesy for extensions that
haven't migrated to the constructor yet). Returns a validated
`update_result`. On any error — wrong type, missing field, invalid value
— emits a warning and returns the safe "no update available" default,
i.e.
[`update_result()`](https://rmtrane.github.io/ntrd/reference/update_result.md).

## Usage

``` r
validate_update_check_result(x, package = NULL)
```

## Arguments

- x:

  Object to validate. Either an `update_result` or a list.

- package:

  Optional package name, used only to make warning messages more
  informative.

## Value

An `update_result` S7 object.

## Details

This wrapper exists because extension authors write the function whose
result this validates; the framework can never trust the value
completely and must downgrade gracefully on malformed input.
