# Build the restart command that performs an extension update

Constructs the string passed to
`rstudioapi::restartSession(command = ...)`. The command, when executed
in a fresh R session:

1.  Prints an "Updating..." banner via
    [`update_intro_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md).

2.  Calls the extension's `ntrd_update_extension()` inside `tryCatch`,
    binding `ok` to `TRUE` on success or `FALSE` on failure. The error
    is surfaced via [`message()`](https://rdrr.io/r/base/message.html).

3.  Queues a second `restartSession()` whose command calls
    [`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
    with the resulting `ok` flag and the package name. That second
    restart lands in a fresh session at an idle R prompt, with a styled
    status message and instructions to relaunch the dashboard.

## Usage

``` r
build_update_restart_command(package)
```

## Arguments

- package:

  Character string giving the extension package name.

## Value

A character string suitable for
[`rstudioapi::restartSession()`](https://rstudio.github.io/rstudioapi/reference/restartSession.html)'s
`command` argument.

## Details

The user-facing messages live in
[`update_intro_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md),
[`update_success_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md),
[`update_failure_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md),
and the
[`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
dispatcher — keeping this function focused on orchestration rather than
copy.
