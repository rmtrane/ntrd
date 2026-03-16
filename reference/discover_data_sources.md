# Discover all available data source extensions

Finds built-in data sources (defined in this package) and data sources
from extensions (from installed packages that declare themselves via
DESCRIPTION fields).

## Usage

``` r
discover_data_sources()
```

## Value

A named list of `data_source` objects, keyed by their `id`.
