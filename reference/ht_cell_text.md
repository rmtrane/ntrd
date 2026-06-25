# Build a text-style spec for use in [`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md).

Returns a named character vector of CSS properties. Multiple style specs
may be combined by passing them as a list to
[`ht_tab_style()`](https://rmtrane.github.io/ntrd/reference/ht_tab_style.md).

## Usage

``` r
ht_cell_text(
  weight = NULL,
  color = NULL,
  style = NULL,
  align = NULL,
  v_align = NULL,
  whitespace = NULL,
  decorate = NULL,
  size = NULL,
  transform = NULL
)
```

## Arguments

- weight:

  `font-weight` (e.g., `"bold"`, `400`).

- color:

  `color`.

- style:

  `font-style` (e.g., `"italic"`).

- align:

  `text-align`.

- v_align:

  `vertical-align`.

- whitespace:

  `white-space` (e.g., `"nowrap"`, `"normal"`).

- decorate:

  `text-decoration-line` (e.g., `"underline"`).

- size:

  `font-size` (e.g., `"14px"`).

- transform:

  `text-transform`.
