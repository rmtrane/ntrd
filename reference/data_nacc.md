# NACC data class

A short description...

## Usage

``` r
data_nacc(data = NULL)
```

## Arguments

- data:

  A `data.table` containing NACC data. If the provided value is not a
  `data.table`, it will be coerced to one by the setter, which also adds
  `VISITDATE` and `NACCAGE` if these are not already present.

## Value

An S7 object of class `data_nacc`. Object construction will fail if the
provided data is missing required columns, has incorrect column types
(`NACCID` must be character; either `VISITYR`, `VISITMO`, and `VISITDAY`
as numerics, or `VISITDATE` as `"YYYY-MM-DD"`; `SEX` and `EDUC` must be
numeric; `BIRTHYR`and `BIRTHMO` or `NACCAGE` must be present as
numerics), or contains invalid values (e.g., `SEX` not 1/2/NA, `VISITYR`
\< 2005, `VISITMO` not 1-12, `VISITDAY` not 1-31, or an invalid date
combination from year/month/day).
