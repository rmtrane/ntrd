# Changelog

## ntrd 0.0.1

Initial release of `ntrd`, the Neuropsychological Test Results
Dashboard. The package provides a Shiny application — built on top of
`ntrs` — for presenting neuropsychological test scores in a form useful
for consensus diagnosis, plus an extension API for adding new data
sources.

### Shiny dashboard

- [`shinyDashboard()`](https://rmtrane.github.io/ntrd/reference/shinyDashboard.md)
  launches the application. Also available from the RStudio “Addins”
  menu as “Neuropsychological Test Result Dashboard”.
- Multi-panel layout built with `bslib` and Bootstrap 5.
- Per-participant view with study ID selector, demographics table, and
  visit-date selector.
- Asynchronous data loading for extension data (e.g. biomarkers) via
  `mirai` daemons, so the UI stays responsive while extensions fetch
  data in the background.

### Data sources

- Built-in **CSV upload** source (`csv_source`) for files in NACC
  format.
- Built-in **Demo** source (`demo_source`) that loads
  [`ntrs::demo_data`](https://rmtrane.github.io/ntrs/reference/demo_data.html)
  with derived variables (`REYTOTAL`, `REYAREC`, `FAS`, `MOCACLOCK`)
  pre-computed — useful for trying the app without real data.
- `data_nacc` S7 class validates input data on construction: checks for
  required columns (`NACCID`, `SEX`, `EDUC`, `BIRTHYR`), enforces column
  types, validates `SEX` and visit-date values, and auto-derives
  `VISITDATE` and `NACCAGE` when their components are present.

### Extension API

- Third parties can register new data sources by declaring
  `Config/ntrd/extension: true` in `DESCRIPTION` and providing S7
  methods for
  [`data_source_ui()`](https://rmtrane.github.io/ntrd/reference/data_source_ui.md),
  [`data_source_server()`](https://rmtrane.github.io/ntrd/reference/data_source_server.md),
  and
  [`data_load()`](https://rmtrane.github.io/ntrd/reference/data_load.md).
- [`new_data_source()`](https://rmtrane.github.io/ntrd/reference/new_data_source.md)
  constructor for defining a class that inherits from `data_source`.
- [`discover_data_sources()`](https://rmtrane.github.io/ntrd/reference/discover_data_sources.md)
  scans installed packages at app startup and populates the data-source
  dropdown.
- Extensions can ship additional UI panels and reactive helpers via the
  `extras` channel returned from
  [`data_source_server()`](https://rmtrane.github.io/ntrd/reference/data_source_server.md).
- Extensions can optionally provide a `restore` function so previously
  entered configuration (e.g. API tokens) can be repopulated.
- Conflict detection warns when multiple extensions define `std_using_*`
  generics with the same name.
- Full walkthrough available in the “Extension API” vignette, with
  [`ntrdWisconsin`](https://github.com/rmtrane/ntrdWisconsin) as a
  complete worked example.

### Tables and visualizations

- [`assessment_summary_table()`](https://rmtrane.github.io/ntrd/reference/assessment_summary_table.md)
  / `mainTableModule` — the main per-visit NACC T-Cog assessment summary
  table, rendered with `gt`.
- [`assessment_longitudinal_table()`](https://rmtrane.github.io/ntrd/reference/assessment_longitudinal_table.md)
  / `longTableModule` — longitudinal table of scores across visits.
- [`demographics_table()`](https://rmtrane.github.io/ntrd/reference/demographics_table.md)
  — compact `gt` demographics summary with flagging for missing or
  visit-to-visit-varying values.
- [`prev_diagnoses_table()`](https://rmtrane.github.io/ntrd/reference/prev_diagnoses_table.md)
  / `prevDiagnosesModule` — prior diagnoses table.
- `plotModule` — interactive `plotly` longitudinal trend plots, one
  accordion panel per cognitive domain, with crosswalk pairs colored
  consistently and trace-visibility state surfaced back to Shiny.
- “Generate PDF for Download” button on the main table (requires
  `pagedown`); falls back to a friendly install hint when `pagedown`
  isn’t available.

### Customization

- `descriptionsModule` — interactive table for editing score-region
  labels, upper-bound cutoffs, and fill colors. Rows can be added,
  edited, reordered (by upper bound), removed, or reset to defaults.
- Sidebar option to toggle shading of plots according to descriptions.
- Sidebar slider to adjust main-table font size.
- “Setup” tab (`methodSelectModule`) for choosing standardization
  methods and assigning variables to cognitive domains; defaults are
  auto-applied when available, with a notification pointing to the Setup
  tab for customization.

### In-app extension updates

- [`check_extension_update()`](https://rmtrane.github.io/ntrd/reference/check_extension_update.md)
  calls an extension’s exported `ntrd_update_available()` (cached for
  one hour by default; configurable via `ttl`);
  [`clear_update_cache()`](https://rmtrane.github.io/ntrd/reference/clear_update_cache.md)
  invalidates the cache.
- Update banner appears in the header when a newer version of the active
  extension is published, with a “What’s new?” link to the extension’s
  `NEWS.md` when provided.
- Two-stage restart cascade (install in one restart, relaunch in the
  next) for RStudio users; clear manual install instructions in the
  modal for Positron, plain R, Rscript, and the VS Code R extension,
  where the restart-with-command flow isn’t supported.
- `update_result` S7 class formalizes the contract between extensions
  and the framework;
  [`validate_update_check_result()`](https://rmtrane.github.io/ntrd/reference/validate_update_check_result.md)
  gracefully downgrades malformed results to a safe “no update” state
  instead of crashing the app.
- [`default_github_update_available()`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  and
  [`default_github_update_extension()`](https://rmtrane.github.io/ntrd/reference/default_github_update.md)
  factories provide ready-to-use implementations for GitHub-hosted
  extensions.

### Infrastructure

- MIT licensed.
- R (\>= 4.2.0).
- CI on GitHub Actions: R-CMD-check on macOS, Windows, and Ubuntu (devel
  / release / oldrel-1); `pkgdown` site build and deploy; Codecov
  coverage upload.
- `shinytest2`-based tests for the Shiny modules.
- `pkgdown` site at <https://rmtrane.github.io/ntrd/>.
