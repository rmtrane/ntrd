# Check for conflicting generic definitions across extensions

Warns when two loaded packages both define an S7 generic with the same
`std_using_*` name, which indicates an extension design issue.

## Usage

``` r
.check_generic_conflicts()
```

## Value

NULL (called for side effects)
