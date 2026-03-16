# Wrapper to prepare raw data

This is a wrapper function that returns only the desired columns, adds
variables that can be derived to the data, and adds standardized scores.

## Usage

``` r
prepare_data(dat, methods = NULL)
```

## Arguments

- dat:

  data set similar to the NACC data. For an example, see
  [`?demo_data`](https://rmtrane.github.io/ntrd/reference/demo_data.md).

- methods:

  An optional named list keyed by `npsych_scores` **class name** (not
  column name). Each element is a named character vector of the form
  `c(method = "...", version = "...")`. Both elements are optional
  within each entry; omitted elements resolve to the registered default.
  Classes not listed use their defaults.

      methods = list(
        MOCATOTS = list(method = "regression", version = "nacc"),
        ANIMALS  = list(method = "norms")
      )
