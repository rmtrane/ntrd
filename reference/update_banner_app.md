# Self-contained Shiny app for testing the update banner module

Mounts `dataSelectServer` and `update_banner_server` together with no
other app machinery, to verify the banner renders correctly from the
`update_info` reactive produced by the data-select module.

## Usage

``` r
update_banner_app(fake_update = NULL, testing = TRUE)
```

## Arguments

- fake_update:

  Optional override for the `update_info` reactive. When `NULL` (the
  default), the real `update_info` from `dataSelectServer` is used. When
  `TRUE`, a demo update is shown regardless of which source is selected.
  When an `update_result` object, that result is shown directly. Useful
  for iterating on the banner visuals without depending on a real
  version mismatch.

## Value

A [`shiny::shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object.
