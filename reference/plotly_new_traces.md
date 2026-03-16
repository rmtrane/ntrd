# New Traces to add to Plot

A short description...

## Usage

``` r
plotly_new_traces(
  new_dat,
  visibility = visibility_defaults,
  legend_names,
  vars_colors
)
```

## Arguments

- new_dat:

  A data frame.

- visibility:

  A named list, where the names correspond to variables in `new_dat` and
  the values are one of `"TRUE"`, `"FALSE"`, or `"legendonly"`.

- legend_names:

  Legend names. Will be returned with added legends if any new ones were
  created. This only happens if a pair of crosswalk variables are both
  present.

- vars_colors:

  Named vector. Entries give colors to use for markers and liens for
  variable corresponding to the name of the entry.

## Value

A list containing new traces (a list of lines and markers), legend
names, the new x range, and the new y range. If there are no new traces
to add (i.e. no standardized scores in the data), the return is empty.
