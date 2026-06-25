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
- [`default_github_update_available()`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  [`default_github_update_extension()`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  : Default update-check and install functions for GitHub-hosted
  extensions
- [`extension_updates`](https://rmtrane.github.io/ntrd/reference/extension_updates.md)
  : Framework machinery for extension update checks

## Tables

Functions to create tables

- [`assessment_longitudinal_table()`](https://rmtrane.github.io/ntrd/reference/assessment_longitudinal_table.md)
  : Assessment Longitudinal Table
- [`assessment_summary_table()`](https://rmtrane.github.io/ntrd/reference/assessment_summary_table.md)
  [`assessment_summary_data()`](https://rmtrane.github.io/ntrd/reference/assessment_summary_table.md)
  : Create NACC T-Cog Neuropsychological Assessment Summary Table
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
  : Shiny Module for Selecting Domains and Standardization Methods
- [`descriptionsUI()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  [`descriptionsServer()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  [`descriptionsApp()`](https://rmtrane.github.io/ntrd/reference/descriptionsModule.md)
  : Descriptions UI
- [`mainTableUI()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  [`mainTableServer()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  [`mainTableApp()`](https://rmtrane.github.io/ntrd/reference/mainTableModule.md)
  : Shiny Module to Display NACC T-Cog Neuropsychological Assessment
  Summary Table
- [`update_banner_ui()`](https://rmtrane.github.io/ntrd/reference/update_banner_module.md)
  [`update_banner_server()`](https://rmtrane.github.io/ntrd/reference/update_banner_module.md)
  : Shiny module for the in-app update banner and flow
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

- [`ht_cells_body()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_stub()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_column_labels()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_stubhead()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_row_groups()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_footnotes()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  :

  Locations for
  [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)

- [`nacc_var_labels`](https://rmtrane.github.io/ntrd/reference/nacc_var_labels.md)
  : Variable Labels for NACC Variables

- [`var_labels()`](https://rmtrane.github.io/ntrd/reference/var_labels.md)
  : Create Labels for Assessment Summary Table

- [`crosswalk_translations`](https://rmtrane.github.io/ntrd/reference/crosswalk_translations.md)
  : Crosswalk Variables

- [`demo_data`](https://rmtrane.github.io/ntrd/reference/demo_data.md) :
  Demo Data

## Custom Table Building Functions

Set of functions to replicate the functionality of
[gt](https://gt.rstudio.com) needed. Light weight and very specific to
the needs of this package.

- [`ht_table()`](https://rmtrane.github.io/ntrd/reference/ht_table.md) :
  Build an HTML table (first-cut: structure only)

- [`ht_add_css()`](https://rmtrane.github.io/ntrd/reference/ht_add_css.md)
  : Attach scoped CSS to a single table.

- [`ht_cell_fill()`](https://rmtrane.github.io/ntrd/reference/ht_cell_fill.md)
  :

  Build a fill-style (background-color) spec for
  [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md).

- [`ht_cell_text()`](https://rmtrane.github.io/ntrd/reference/ht_cell_text.md)
  :

  Build a text-style spec for use in
  [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md).

- [`ht_cells_body()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_stub()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_column_labels()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_stubhead()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_row_groups()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  [`ht_cells_footnotes()`](https://rmtrane.github.io/ntrd/reference/ht_cells.md)
  :

  Locations for
  [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)

- [`ht_cols_hide()`](https://rmtrane.github.io/ntrd/reference/ht_cols_hide.md)
  : Hide one or more columns.

- [`ht_cols_label()`](https://rmtrane.github.io/ntrd/reference/ht_cols_label.md)
  : Relabel one or more columns.

- [`ht_hide_header()`](https://rmtrane.github.io/ntrd/reference/ht_hide_header.md)
  :

  Suppress the entire `<thead>` (spanner row + column labels).

- [`ht_merge_cols_label()`](https://rmtrane.github.io/ntrd/reference/ht_merge_cols_label.md)
  : Merge column labels into one cell.

- [`ht_render()`](https://rmtrane.github.io/ntrd/reference/ht_render.md)
  :

  Render a `html_table` to
  [`shiny::tags`](https://rstudio.github.io/htmltools/reference/builder.html)

- [`ht_tab_footnote()`](https://rmtrane.github.io/ntrd/reference/ht_tab_footnote.md)
  : Append a table-level footnote.

- [`ht_tab_spanner()`](https://rmtrane.github.io/ntrd/reference/ht_tab_spanner.md)
  : Add a column spanner across one or more columns.

- [`ht_tab_stubhead()`](https://rmtrane.github.io/ntrd/reference/ht_tab_stubhead.md)
  :

  Set the stubhead label (top-left cell, only shown when `rowname_col`
  is set).

- [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md)
  : Attach styles to specific cell locations.

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

- [`update_intro_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md)
  [`update_success_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md)
  [`update_failure_message()`](https://rmtrane.github.io/ntrd/reference/update_messages.md)
  : Print formatted messages for the extension update flow

- [`clear_update_cache()`](https://rmtrane.github.io/ntrd/reference/clear_update_cache.md)
  : Clear the update-check cache

- [`try_update()`](https://rmtrane.github.io/ntrd/reference/try_update.md)
  : Try update extension

- [`update_result()`](https://rmtrane.github.io/ntrd/reference/update_result.md)
  : Result of an extension update check

- [`validate_update_check_result()`](https://rmtrane.github.io/ntrd/reference/validate_update_check_result.md)
  :

  Validate (or coerce) the result of an `ntrd_update_available()` call

- [`check_extension_update()`](https://rmtrane.github.io/ntrd/reference/check_extension_update.md)
  : Check whether an update is available for an extension package
