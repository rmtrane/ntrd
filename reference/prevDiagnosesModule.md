# Shiny Module to Display Previous Diagnoses

Shiny Module to Display Previous Diagnoses

## Usage

``` r
prevDiagnosesUI(id)

prevDiagnosesServer(
  id,
  dat,
  print_updating = F,
  table_font_size = shiny::reactive(100)
)

prevDiagnosesApp(
  dat,
  print_updating = F,
  table_font_size = shiny::reactive(100)
)
```

## Arguments

- id:

  An ID string to match module UI and module server

- dat:

  Data to use. Should only refer to a single participant

- print_updating:

  logical (default `FALSE`); should a message be printed to indicate
  that the table is being updated? For debugging.

- table_font_size:

  Table font size as a percent. Default: 100
