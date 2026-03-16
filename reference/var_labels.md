# Create Labels for Assessment Summary Table

Helper to create row names for `assessment_summary_table` based on
values of various scores.

## Usage

``` r
var_labels(
  CDRSUM = NA,
  TRAILARR = NA,
  TRAILALI = NA,
  TRAILBRR = NA,
  TRAILBLI = NA,
  UDSBENRS = NA,
  UDSBENTD = NA,
  UDSBENTC = NA,
  REY1REC = NA,
  REY2REC = NA,
  REY3REC = NA,
  REY4REC = NA,
  REY5REC = NA,
  REYTCOR = NA,
  REYFPOS = NA,
  CRAFTURS = NA,
  CRAFTVRS = NA,
  CRAFTDVR = NA,
  CRAFTDRE = NA
)
```

## Arguments

- CDRSUM:

  Standard CDR sum of boxes

- TRAILARR:

  Number of commission errors for Trail Making Part A

- TRAILALI:

  Number of correct lines for Trail Making Part A

- TRAILBRR:

  Number of commission errors for Trail Making Part B

- TRAILBLI:

  Number of correct lines for Trail Making Part B

- UDSBENRS:

  Benson Complex Figure Recall - Recognized original stimulus among four
  options

- UDSBENTD:

  Total score for 10- to 15-minute delayed drawing of Benson figure

- UDSBENTC:

  Total Score for copy of Benson figure

- REY1REC:

  Rey AVLT Trail 1 total recall

- REY2REC:

  Rey AVLT Trail 2 total recall

- REY3REC:

  Rey AVLT Trail 3 total recall

- REY4REC:

  Rey AVLT Trail 4 total recall

- REY5REC:

  Rey AVLT Trail 5 total recall

- REYTCOR:

  Rey AVLT delayed recognition; total correct

- REYFPOS:

  Rey AVLT delayed recognition; total false positives

- CRAFTURS:

  Craft Story 21 Recall (Immediate) - Total story units recalled,
  paraphrase scoring

- CRAFTVRS:

  Craft Story 21 Recall (Immediate) - Total story units recalled,
  verbatim scoring

- CRAFTDVR:

  Craft Story 21 Recall (Delayed) - Total story units recalled, verbatim
  scoring

- CRAFTDRE:

  Craft Story 21 Recall (Delayed) - Total story units recalled,
  paraphrase scoring

## Examples

``` r
var_labels(
  CDRSUM = 14,
  TRAILARR = 0,
  TRAILALI = 24,
  TRAILBRR = 2,
  TRAILBLI = 24,
  UDSBENRS = 1,
  UDSBENTD = 14,
  UDSBENTC = 14,
  REY1REC = -4,
  REY2REC = -4,
  REY3REC = -4,
  REY4REC = -4,
  REY5REC = -4,
  REYTCOR = -4,
  REYFPOS = -4,
  CRAFTURS = 20,
  CRAFTVRS = 12,
  CRAFTDVR = 11,
  CRAFTDRE = 13
)
#>               CDRGLOB MOCATOTS   MOCBTOTS                             NACCMMSE
#> 1 CDR Global (14 SOB)     MoCA MoCA Blind Mini Mental State Examination (MMSE)
#>                                    TRAILA
#> 1 Trailmaking Part A (0 errors; 24/24 CL)
#>                                     OTRAILA                          OTRLARR
#> 1 Oral Trailmaking Part A - Completion Time Oral Trailmaking Part A - Errors
#>                      DIGFORCT                          DIGFORSL
#> 1 Number Span Forward - Total Number Span Forward - Span Length
#>                       DIGBACCT                           DIGBACLS
#> 1 Number Span Backward - Total Number Span Backward - Span Length
#>                        DIGIF                         DIGIFLEN
#> 1 Digit Span Forward - Total Digit Span Forward - Span Length
#>                         DIGIB                          DIGIBLEN
#> 1 Digit Span Backward - Total Digit Span Backward - Span Length
#>                  WAIS MINTTOTS             BOSTON        ANIMALS
#> 1 WAIS-R Digit Symbol     MINT Boston Naming Test Animal Fluency
#>                 VEG  UDSVERTN UDSVERFC UDSVERLC           UDSBENTC
#> 1 Vegetable Fluency F+L Words  F Words  L Words Benson Figure Copy
#>                                  UDSBENTD                   CRAFTVRS
#> 1 Benson Delay (100% retained; Recog = 1) Craft Immediate - Verbatim
#>                        LOGIMEM                    MEMUNITS
#> 1 Logical Memory IA, Immediate Logical Memory IIA, Delayed
#>                       CRAFTURS                              CRAFTDVR
#> 1 Craft Immediate - Paraphrase Craft Delay - Verbatim (91% retained)
#>                                  CRAFTDRE                              REYTOTAL
#> 1 Craft Delay - Paraphrase (65% retained) RAVLT Total Learning (-4,-4,-4,-4,-4)
#>                REYDLIST           REY6REC          REYDREC
#> 1 RAVLT Distractor List RAVLT Short Delay RAVLT Long Delay
#>                                         REYAREC
#> 1 RAVLT Recognition (Correct: -4/15, FP: -4/15)
#>                                    TRAILB          MOCACLOCK
#> 1 Trailmaking Part B (2 errors; 24/24 CL) Clock Drawing Test
#>                                     OTRAILB                          OTRLBRR
#> 1 Oral Trailmaking Part B - Completion Time Oral Trailmaking Part B - Errors
#>                        NACCGDS           CESDTOTAL
#> 1 GDS-15 (Depression Symptoms) CES-D (Total Score)
```
