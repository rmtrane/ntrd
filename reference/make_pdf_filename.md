# Generate PDF filename for download

Generate PDF filename for download

## Usage

``` r
make_pdf_filename(naccid, visitdate, time = Sys.time())
```

## Arguments

- naccid:

  Character string with the NACC ID.

- visitdate:

  Character string or Date with the visit date.

- time:

  POSIXct timestamp used for the "created-on" portion of the filename.
  Defaults to [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

## Value

A character string suitable for use as a PDF filename.
