# Build the restart command that performs an extension update

Constructs the string passed to
`rstudioapi::restartSession(command = ...)`. The generated script prints
a short "installing..." message, attempts the install in a `tryCatch`,
and queues a second restart that runs
[`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
with the outcome. That leaves the user at an idle R prompt with a status
message and instructions to relaunch the dashboard; the dashboard does
not reopen by itself.

## Usage

``` r
build_update_restart_command(package, repo = extension_install_repo(package))
```

## Arguments

- package:

  Character string giving the extension package name.

- repo:

  GitHub repository to install from, or `NULL` to fall back to looking
  up the extension's `ntrd_update_extension()` at restart time. By
  default, read from the extension's `ntrd_update_extension()`.

## Value

A character string suitable for
[`rstudioapi::restartSession()`](https://rstudio.github.io/rstudioapi/reference/restartSession.html)'s
`command` argument.

## Details

How the script installs the update depends on the extension's
`ntrd_update_extension()`:

- If it was made by
  [`default_github_update_extension()`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  (so it carries an `"ntrd_github_repo"` attribute), the script calls
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)
  directly, by package name, without looking up the function at all.

- Otherwise, the script looks up `ntrd_update_extension()` with
  [`getNamespace()`](https://rdrr.io/r/base/ns-reflect.html) and calls
  it. This loads the extension's namespace (and whatever it imports),
  but it's base R only — no package lookup function from `ntrd` is
  involved.

Either way, nothing from `ntrd` itself is loaded until the second
restart, after the install has finished, so `ntrd` is never one of the
packages the install might be replacing.
