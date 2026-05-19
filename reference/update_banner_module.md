# Shiny module for the in-app update banner and flow

UI: renders a `uiOutput` slot that the server fills with a banner when
an update is available. Server: takes a reactive yielding
`list(package, result)` and handles the full click-to-restart flow.

## Usage

``` r
update_banner_ui(id)

update_banner_server(id, update_info)
```

## Arguments

- id:

  Module id.

- update_info:

  A reactive returning `list(package, result)` where `result` is an
  `update_result`.
