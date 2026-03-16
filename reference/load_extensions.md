# Load extension packages

Scans installed packages for those declaring themselves as ntrd
extensions and loads their namespaces. Loading registers their S7
methods (via S7::methods_register() in .onLoad), making them visible to
discover_data_sources().

## Usage

``` r
load_extensions()
```

## Value

NULL (called for side effects)
