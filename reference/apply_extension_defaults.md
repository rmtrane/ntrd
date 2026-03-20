# Apply extension defaults

Looks up and calls `.set_defaults()` in the namespace of an extension
package. This is a no-op when `ext_pkg` is `"ntrd"` (the base package).

## Usage

``` r
apply_extension_defaults(ext_pkg)
```

## Arguments

- ext_pkg:

  Character string. The package name of the data source.

## Value

`TRUE` (invisibly) if `.set_defaults()` was found and called, `FALSE`
otherwise.
