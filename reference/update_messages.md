# Print formatted messages after an extension update attempt

These functions print a user-facing success or failure message in the
second restart of the in-app update flow (see
[`build_update_restart_command()`](https://rmtrane.github.io/ntrd/reference/build_update_restart_command.md)),
which runs in a fresh R session where `ntrd` is not yet attached. The
"installing..." message printed before the install itself is plain
[`cat()`](https://rdrr.io/r/base/cat.html), generated inline by
[`build_update_restart_command()`](https://rmtrane.github.io/ntrd/reference/build_update_restart_command.md),
so that step doesn't need to load `ntrd` either.

Each function takes the extension package name as its only argument and
returns invisibly.
[`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
dispatches to the appropriate message based on whether the install
succeeded.

Users do not normally call these directly.

## Usage

``` r
update_success_message(package)

update_failure_message(package)
```

## Arguments

- package:

  Character string giving the extension package name.

## Value

Invisible `NULL`.
