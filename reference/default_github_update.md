# Default update-check and install functions for GitHub-hosted extensions

Factory functions that produce the two functions an extension must
export to opt in to ntrd's in-app update mechanism
(`ntrd_update_available()` and `ntrd_update_extension()`). Extensions
hosted on GitHub can use these directly; extensions hosted elsewhere
(GitLab, CRAN, internal repos) should provide their own implementations
following the same contract.

## Usage

``` r
default_github_update_available(repo, package = NULL)

default_github_update_extension(repo)
```

## Arguments

- repo:

  A GitHub repository specification of the form `"user/repo"`. May
  optionally include a ref (`"user/repo@ref"`); the ref is stripped for
  purposes of deriving the package name but preserved in the install
  call.

- package:

  Optional package name. If `NULL` (the default), it is derived from
  `repo` by taking the part after `"/"` and stripping any `@ref` suffix.
  Provide explicitly when the repository name differs from the R package
  name.

## Value

For `default_github_update_available()`: a function taking no arguments
that returns an
[`update_result`](https://rmtrane.github.io/ntrd/reference/update_result.md)
S7 object.

For `default_github_update_extension()`: a function taking no arguments
that performs the install and returns invisibly.

## Details

`default_github_update_available()` returns a function that fetches the
`DESCRIPTION` file from the GitHub repository's default branch, parses
out the `Version:` field, and compares it against the locally installed
version. The result is wrapped in an
[`update_result`](https://rmtrane.github.io/ntrd/reference/update_result.md)
object.

`default_github_update_extension()` returns a function that calls
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)
with `upgrade = "always"` and `dependencies = TRUE`, so dependencies
(including `ntrd` itself) are also brought up to date.

Both factories call [`force()`](https://rdrr.io/r/base/force.html) on
their arguments so that captured values are safe to use even if the
factory is ever called in a loop.

## Examples

``` r
if (FALSE) { # \dontrun{
# In an extension package's R/updates.R:

#' @export
ntrd_update_available <- ntrd::default_github_update_available(
  "rmtrane/ntrdWisconsin"
)

#' @export
ntrd_update_extension <- ntrd::default_github_update_extension(
  "rmtrane/ntrdWisconsin"
)
} # }
```
