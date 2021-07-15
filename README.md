CREDI Multi-dimensional Item Factor Analysis Scoring
================
Jonathan Seiden
7/15/2021

## Introduction

The [Caregiver Reported Early Development
Instruments](https://credi.gse.harvard.edu/) is a measure of early
development that is administered through an interview or survey
completion by a child’s parent or primary caregiver. This program is
designed to process the raw response data to calculate scores.

This package produces several scores based on input data, and supports
both data collected on the Long Form and Short Form versions of CREDI.
Most users will prefer to use the [online scoring
app](https://credi.shinyapps.io/Scoring_App) which does not require the
use of R in order to calculate scores, but this package is made
publically available to advanced users and those who wish to understand
the multi-dimensional factor analysis process used to calculate overal
and domain-level scores.

See the [CREDI scoring
manual](https://projects.iq.harvard.edu/files/credi/files/credi-scoring-manual-8-jun-20181.pdf)
for more details about score calculation.

## Installation

Installing the CREDI scoring app is currently only available through
GitHub. Easily download and install with the below lines of code:

``` r
require(devtools)
devtools::install_github("https://github.com/marcus-waldman/credi")
```

## Example

Most users will only use the `score` function, which converts raw item
responses into overall, short-form, domain, and normalized reference
scores. All other functions included in the `credi` package are called
by this function. These other functions are exported provided for
reference, explanation, and clarity, but are not designed for standalone
use.

Below is a very simple example with three simulated children, each of
whom only had 5 Long Form questions. Note that one child is missing a
value for LF1, resulting in no score being calculated for the child
because the `min_items` parameter was set to 5.

``` r
library(credi)

#Create a sample dataframe
dat <- data.frame(
  ID = 1:3,
  AGE = c(3, 5, 4),
  LF1 = c(1, 0, NA),
  LF2 = c(0, 0, 0),
  LF3 = c(1, 0, 1),
  LF4 = c(1, 1, 1),
  LF5 = c(1, 0, 0)
)

#Score the dataframe
scored_dat <- credi::score(
  data = dat,
  reverse_code = FALSE,
  interactive = FALSE,
  min_items = 5
)
```

    ## 
    ## Scoring  2  observations:
    ##   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%

``` r
#Print out domain scores:
scored_dat$scores[, c("SF", "MOT", "LANG", "SEM", "COG", "OVERALL")]
```

    ##       SF    MOT   LANG    SEM    COG OVERALL
    ## 1 41.204 43.489 45.968 44.626 45.091  40.079
    ## 2 42.917 42.058 45.049 43.755 44.250  38.160
