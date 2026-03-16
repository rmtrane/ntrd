# Package index

- [`shinyDashboard()`](https://rmtrane.github.io/ntrd/reference/shinyDashboard.md)
  : Run Shiny App

## Extension Functions

Functions that enables third parties to implement new data sources.

- [`data_source()`](https://rmtrane.github.io/ntrd/reference/data_source.md)
  : data_source
- [`new_data_source()`](https://rmtrane.github.io/ntrd/reference/new_data_source.md)
  : Create a new data source class
- [`data_source_server()`](https://rmtrane.github.io/ntrd/reference/data_source_server.md)
  : data_source_server generic
- [`data_source_ui()`](https://rmtrane.github.io/ntrd/reference/data_source_ui.md)
  : Data source UI
- [`data_load()`](https://rmtrane.github.io/ntrd/reference/data_load.md)
  : Load data
- [`data_nacc()`](https://rmtrane.github.io/ntrd/reference/data_nacc.md)
  : NACC data class

## Tables

Functions to create tables

- [`assessment_longitudinal_table()`](https://rmtrane.github.io/ntrd/reference/assessment_longitudinal_table.md)
  : Assessment Longitudinal Table
- [`demographics_table()`](https://rmtrane.github.io/ntrd/reference/demographics_table.md)
  : Summary Table with Demographic Information
- [`prev_diagnoses_table()`](https://rmtrane.github.io/ntrd/reference/prev_diagnoses_table.md)
  : Table With Previous Diagnoses

## Shiny Modules

Shiny modules used for the main Shiny application

- [`dataSelectServer()`](https://rmtrane.github.io/ntrd/reference/dataSelectServer.md)
  : Data Selection Server Module
- [`dataSelectUI()`](https://rmtrane.github.io/ntrd/reference/dataSelectUI.md)
  : Data selection UI module
- [`methodSelectUI()`](https://rmtrane.github.io/ntrd/reference/methodSelectModule.md)
  [`methodSelectServer()`](https://rmtrane.github.io/ntrd/reference/methodSelectModule.md)
  [`methodSelectApp()`](https://rmtrane.github.io/ntrd/reference/methodSelectModule.md)
  : Shiny Module for Selecting Columns
- [`descriptionsUI()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  [`descriptionsServer()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  [`descriptionsApp()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  : Descriptions UI
- [`mainTableUI()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  [`mainTableServer()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  [`mainTableApp()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  : Shiny Module to Display NACC T-Cog Neuropsychological Assessment
  Summary Table
- [`plotUI()`](https://rmtrane.github.io/ntrd/reference/plotModule.md)
  [`plotServer()`](https://rmtrane.github.io/ntrd/reference/plotModule.md)
  [`plotApp()`](https://rmtrane.github.io/ntrd/reference/plotModule.md)
  : Plot Module
- [`longTableUI()`](https://rmtrane.github.io/ntrd/reference/longTableModule.md)
  [`longTableServer()`](https://rmtrane.github.io/ntrd/reference/longTableModule.md)
  [`longTableApp()`](https://rmtrane.github.io/ntrd/reference/longTableModule.md)
  : Shiny Module to Display Longitudinal Trends in Table
- [`prevDiagnosesUI()`](https://rmtrane.github.io/ntrd/reference/prevDiagnosesModule.md)
  [`prevDiagnosesServer()`](https://rmtrane.github.io/ntrd/reference/prevDiagnosesModule.md)
  [`prevDiagnosesApp()`](https://rmtrane.github.io/ntrd/reference/prevDiagnosesModule.md)
  : Shiny Module to Display Previous Diagnoses

## Shiny Helpers

- [`shinyAddResources()`](https://rmtrane.github.io/ntrd/reference/shinyAddResources.md)
  : Add Shiny resources
- [`shinyApp_header()`](https://rmtrane.github.io/ntrd/reference/shinyApp_header.md)
  : Shiny app header

## Variable Definitions

Vectors/lists used to map variable names to/from NACC variable names

- [`birth_vars`](https://rmtrane.github.io/ntrd/reference/birth_vars.md)
  : Variables necessary for Birth Dates
- [`critical_vars`](https://rmtrane.github.io/ntrd/reference/critical_vars.md)
  : Variables Necessary
- [`optional_vars`](https://rmtrane.github.io/ntrd/reference/optional_vars.md)
  : Optional Variables
- [`visit_vars`](https://rmtrane.github.io/ntrd/reference/visit_vars.md)
  : Variables necessary for Visit Dates
- [`nacc_to_wls`](https://rmtrane.github.io/ntrd/reference/nacc_to_wls.md)
  : Translate NACC variable names to WLS variable names
- [`wls_to_nacc`](https://rmtrane.github.io/ntrd/reference/wls_to_nacc.md)
  : Translate WLS variable names to NACC variable names
- [`nacc_var_groups`](https://rmtrane.github.io/ntrd/reference/nacc_var_groups.md)
  : NACC Variable Groupings
- [`nacc_groups`](https://rmtrane.github.io/ntrd/reference/nacc_groups.md)
  : NACC Variable Groups
- [`rdd`](https://rmtrane.github.io/ntrd/reference/rdd.md) : Researchers
  Data Dictionary in List Form
- [`visibility_defaults`](https://rmtrane.github.io/ntrd/reference/visibility_defaults.md)
  : Visibility Defaults
- [`diag_contr_pairs`](https://rmtrane.github.io/ntrd/reference/diag_contr_pairs.md)
  : Etiology Specific Variables
- [`nacc_var_labels`](https://rmtrane.github.io/ntrd/reference/nacc_var_labels.md)
  : Variable Labels for NACC Variables
- [`var_labels()`](https://rmtrane.github.io/ntrd/reference/var_labels.md)
  : Create Labels for Assessment Summary Table
- [`crosswalk_translations`](https://rmtrane.github.io/ntrd/reference/crosswalk_translations.md)
  : Crosswalk Variables
- [`demo_data`](https://rmtrane.github.io/ntrd/reference/demo_data.md) :
  Demo Data

## Misc Functions

- [`calc_fill_colors()`](https://rmtrane.github.io/ntrd/reference/calc_fill_colors.md)
  : Calculate Fill Colors from Number of Descriptions

- [`check_colors()`](https://rmtrane.github.io/ntrd/reference/check_colors.md)
  : Check colors

- [`chrome_extra_args()`](https://rmtrane.github.io/ntrd/reference/chrome_extra_args.md)
  : Return Chrome CLI arguments

- [`onRender()`](https://rmtrane.github.io/ntrd/reference/onRender.md) :

  See `help(htmlwidgets::onRender)`

- [`plotly_new_traces()`](https://rmtrane.github.io/ntrd/reference/plotly_new_traces.md)
  : New Traces to add to Plot

- [`prepare_data()`](https://rmtrane.github.io/ntrd/reference/prepare_data.md)
  : Wrapper to prepare raw data
