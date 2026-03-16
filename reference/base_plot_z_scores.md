# Create base plotly figure

Create a base plot where traces can later be added.

## Usage

``` r
base_plot_z_scores(
  x_range,
  y_range,
  descriptions = c(Impaired = 0.03, Borderline = 0.1, `Low Average` = 0.26, Average =
    0.76, `High Average` = 0.92, Superior = 0.97, `Very Superior` = 1),
  fill_values,
  shade_descriptions = T,
  fill_alpha = 0.2,
  source = "A",
  new_id = NULL
)
```

## Arguments

- x_range:

  vector with two elements giving the smallest and largest values to use
  on x-axis (in order)

- y_range:

  vector with two elements giving the smallest and largest values to use
  on y-axis (in order)

- descriptions:

  A named vector giving cutoffs to use for creating descriptions.

- fill_values:

  (optional) A named vector of same length (with same names) as
  `descriptions` with hex color values to use. If `NULL`, no colorcoding
  used. By default, evenly spread out colors from red through yellow to
  green are used.

- shade_descriptions:

  logical; should ranges for descriptions be shaded?

- fill_alpha:

  opacity value for shaded areas

- source:

  passed to
  [`plotly::plot_ly`](https://rdrr.io/pkg/plotly/man/plot_ly.html)

- new_id:

  Optional. String to use as the plotly source ID. If `NULL` (default),
  a random string is assigned.
