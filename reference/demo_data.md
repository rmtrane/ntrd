# Demo Data

A completely made up demo data set meant to mimic the structure of the
NACC data. While IDs look like valid NACC IDs, these are simply the
characters "NACC" with six random digits. Similarly, birth and visit
dates are randomly created, and so are all scores with the only
constraint that -4's are believable. (For example, if OTRAILA is -4, so
is OTRAILB.)

## Usage

``` r
demo_data
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 344
rows and 159 columns.
