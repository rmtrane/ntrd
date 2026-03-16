# Create a new data source class

Convenience constructor that creates an S7 class inheriting from
[data_source](https://rmtrane.github.io/ntrd/reference/data_source.md).
The returned object is a **class** (not an instance) that can be
instantiated with no arguments.

## Usage

``` r
new_data_source(name, id, package, ...)
```

## Arguments

- name:

  Character. Human-readable display name shown in the UI dropdown.

- id:

  Character. Unique identifier used internally as a key.

- package:

  Character. The name of the package defining this data source.

- ...:

  Additional arguments passed to
  [`data_source()`](https://rmtrane.github.io/ntrd/reference/data_source.md).

## Value

An S7 class that inherits from `data_source`.

## Examples

``` r
my_source <- new_data_source(name = "My Source", id = "my_source", package = "ntrd")
my_source()
#> <ntrd::my_source>
#>  @ name   : chr "My Source"
#>  @ id     : chr "my_source"
#>  @ package: chr "ntrd"
```
