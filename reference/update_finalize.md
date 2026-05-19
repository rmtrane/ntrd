# Dispatch to the success or failure message after an install attempt

Called from the second restart's command (see
[`build_update_restart_command()`](https://rmtrane.github.io/ntrd/reference/build_update_restart_command.md))
with the boolean result of the install and the package name. Validates
its inputs so that misuse fails loudly.

## Usage

``` r
update_finalize(ok, package)
```

## Arguments

- ok:

  Logical scalar. `TRUE` if the install succeeded, `FALSE` if it
  errored.

- package:

  Character string giving the extension package name.

## Value

Invisible `NULL`.
