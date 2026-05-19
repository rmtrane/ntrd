# Print formatted messages for the extension update flow

These functions print user-facing messages during the in-app update
flow's restarted-session steps. They are exported so that they can be
invoked by fully qualified name from the strings built by
[`build_update_restart_command()`](https://rmtrane.github.io/ntrd/reference/build_update_restart_command.md),
which run in restarted R sessions where `ntrd` is not attached.

Each function takes the extension package name as its only argument and
returns invisibly.
[`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
dispatches to the appropriate message based on whether the install
succeeded.

Users do not normally call these directly.

## Usage

``` r
update_intro_message(package)

update_success_message(package)

update_failure_message(package)
```

## Arguments

- package:

  Character string giving the extension package name.

## Value

Invisible `NULL`.
