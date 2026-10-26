# Framework machinery for extension update checks

Internal helpers and a small public surface
([`check_extension_update()`](https://rmtrane.github.io/ntrd/reference/check_extension_update.md),
[`validate_update_check_result()`](https://rmtrane.github.io/ntrd/reference/validate_update_check_result.md),
[`clear_update_cache()`](https://rmtrane.github.io/ntrd/reference/clear_update_cache.md))
used by the ntrd Shiny app to discover whether an extension has opted in
to in-app updates, fetch (and cache) the update-availability
information, validate its shape, and build the restart command used by
the "Update" button. See also
[`update_success_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md),
[`update_failure_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md),
and the
[`update_finalize()`](https://rmtrane.github.io/ntrd/reference/update_finalize.md)
dispatcher for the user-facing copy printed during the restart cascade.

Extensions opt in by exporting two functions:

- `ntrd_update_available()` — returns an
  [`update_result`](https://rmtrane.github.io/ntrd/reference/update_result.md)
  S7 object. A plain list with the same fields is also accepted as a
  backwards-compatibility courtesy.

- `ntrd_update_extension()` — performs the install. Runs in a restarted
  R session. Functions made by
  [`default_github_update_extension`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  are not called at all: ntrd reads the repo they record and installs by
  package name. Any other function is looked up and called directly in
  that session, which loads the extension's namespace — but not
  `ntrd`'s, which isn't loaded until the install has finished.

See
[`default_github_update_available`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
and
[`default_github_update_extension`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
for factory functions that produce these two for GitHub-hosted
extensions.
