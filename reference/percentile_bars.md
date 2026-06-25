# Build inline percentile bars as HTML.

Replaces the gt-coupled
[`my_gt_plt_bar_pct()`](https://rmtrane.github.io/ntrd/reference/my_gt_plt_bar_pct.md):
returns a list of
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
values, one per input element, suitable for storing directly in a
`data.table` column that `html_table` then passes through verbatim.

## Usage

``` r
percentile_bars(
  values,
  scaled = TRUE,
  labels = TRUE,
  label_cutoff = 0.4,
  decimals = 1,
  height = 16,
  width = 100,
  fill = "purple",
  background = "#e1e1e1",
  font_style = "bold",
  font_size = "10px"
)
```

## Arguments

- values:

  Numeric vector. Percentiles in 0–100 when `scaled = TRUE`, or
  arbitrary nonnegative numbers when `scaled = FALSE` (in which case
  they're normalized against `max(values, na.rm = TRUE)`).

- scaled:

  If `TRUE`, treat `values` as already on a 0–100 scale. Otherwise
  rescale to the column maximum.

- labels:

  If `TRUE`, overlay a percentage label.

- label_cutoff:

  Fraction (0–1) of the bar length below which the label is rendered
  *outside* the bar rather than centered inside it.

- decimals:

  Decimal places shown in the label.

- height:

  Bar height in pixels.

- width:

  Track width in pixels.

- fill:

  Bar fill color.

- background:

  Track background color.

- font_style:

  Label `font-weight` (`"bold"`, `"normal"`, `"italic"`).

- font_size:

  Label `font-size`, as a CSS length string.

## Value

A list of
[`shiny::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
values, length `length(values)`.

## Details

The visual is the same as gt's: a fixed-width grey track containing a
colored bar sized to `value`. With `labels = TRUE`, a small percentage
label is overlaid — inside the bar if the bar is long enough, otherwise
outside it. Missing values render as an em-dash, matching the original.
