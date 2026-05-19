# Result of an extension update check

An S7 class representing the outcome of an `ntrd_update_available()`
call. Extension authors writing custom update checks should end their
function with a call to `update_result()` so that the framework receives
a well-typed value. The constructor's defaults make the "nothing to
report" case trivial: `update_result()` is a valid no-update-available
result.

## Usage

``` r
update_result(
  available = FALSE,
  current = NA_character_,
  latest = NA_character_,
  news_url = NA_character_
)
```

## Arguments

- available:

  Logical scalar. Defaults to `FALSE`.

- current:

  Character scalar. Defaults to `NA_character_`.

- latest:

  Character scalar. Defaults to `NA_character_`.

- news_url:

  Character scalar. Defaults to `NA_character_`. Use `NA` to indicate
  that no NEWS URL is available.

## Value

An `update_result` S7 object.

## Details

Properties:

- `available`:

  Logical scalar. `TRUE` when a newer version of the extension is known
  to exist. Defaults to `FALSE`, which is the safe default: a forgotten
  field will not produce a spurious update prompt.

- `current`:

  Character scalar. The installed version of the extension, or
  `NA_character_` if it could not be determined.

- `latest`:

  Character scalar. The latest available version, or `NA_character_` if
  it could not be determined.

- `news_url`:

  Character scalar. An optional URL pointing to a changelog or release
  notes to show alongside the update prompt; use `NA_character_` (the
  default) when none is available. Consistent with `current` and
  `latest`, `NA` represents "absent".

Validation is enforced by the class: invalid construction errors at the
call site. The framework wraps construction in error handling (see
[`validate_update_check_result()`](https://rmtrane.github.io/ntrd/reference/validate_update_check_result.md)),
so a misbehaving extension cannot crash the app — it will be downgraded
to a safe "no update" result with a warning.

## Examples

``` r
# The empty constructor — "no update available", safe default
update_result()
#> <ntrd::update_result>
#>  @ available: logi FALSE
#>  @ current  : chr NA
#>  @ latest   : chr NA
#>  @ news_url : chr NA

# Couldn't fetch the remote version, but we know what's installed
update_result(current = "0.1.0")
#> <ntrd::update_result>
#>  @ available: logi FALSE
#>  @ current  : chr "0.1.0"
#>  @ latest   : chr NA
#>  @ news_url : chr NA

# An update is available
update_result(
  available = TRUE,
  current = "0.1.0",
  latest = "0.2.0",
  news_url = "https://github.com/example/pkg/blob/HEAD/NEWS.md"
)
#> <ntrd::update_result>
#>  @ available: logi TRUE
#>  @ current  : chr "0.1.0"
#>  @ latest   : chr "0.2.0"
#>  @ news_url : chr "https://github.com/example/pkg/blob/HEAD/NEWS.md"

# No NEWS URL available — use NA, not NULL
update_result(
  available = TRUE,
  current = "0.1.0",
  latest = "0.2.0",
  news_url = NA_character_
)
#> <ntrd::update_result>
#>  @ available: logi TRUE
#>  @ current  : chr "0.1.0"
#>  @ latest   : chr "0.2.0"
#>  @ news_url : chr NA
```
