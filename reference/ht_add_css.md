# Attach scoped CSS to a single table.

Every selector in `css` is prepended with `#<id>` (where `id` is the
value passed to
[`ht_table()`](https://rmtrane.github.io/ntrd/reference/ht_table.md)),
so the styles only affect this table. `@media`/`@supports`/`@container`
blocks are recursed into; other `@`-rules pass through untouched.

## Usage

``` r
ht_add_css(x, css)
```

## Arguments

- x:

  A `html_table`. Must have been constructed with a non-empty `id`.

- css:

  A single character string of CSS rules.

## Details

To style the `<table>` element itself, use `&` (SCSS / native CSS
nesting convention) — it is replaced with `#<id>` directly, without a
descendant combinator. So `& { border: 1px solid; }` becomes
`#<id> { border: 1px solid; }`, whereas plain `tr { ... }` becomes
`#<id> tr { ... }`.

## Examples

``` r
if (FALSE) { # \dontrun{
ht_table(dat, id = "demographics-table") |>
  ht_hide_header() |>
  ht_add_css("
    & {
      border: 1px solid #333;
    }
    tr:first-child { font-weight: bold; }
    td { padding: 2px 8px; }
  ")
} # }
```
