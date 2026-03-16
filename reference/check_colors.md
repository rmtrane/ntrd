# Check colors

Check if all elements of a vector or list are valid hex colors.

## Usage

``` r
check_colors(x, return_non_colors = F)
```

## Arguments

- x:

  A vector or list of elements

- return_non_colors:

  Optional. If `TRUE`, return all entries that are not recognized as hex
  colors as a named character vector where names give the entry number.
  If `FALSE` (default), return logical indicating if all entries are
  recognized as hex colors or not.

## Value

Either a logical (if `return_non_colors` is `FALSE`) where `TRUE`
indicates that all elements of `x` are valid hex colors, or a named
character vector.
