
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DutchDayDummies

## Overview

The goal of the DutchDayDummies package is to provide functions that can
quickly generate regression dummies for official and non-official dutch
holidays. It includes virtually all (non-official) holidays you can
imagine, varying from Christmas, Easter and Ramadan to Mother’s day,
Black Friday and Summer vacation. The functions that create the holiday
dummies use algorithms to convert dates to dummies and thus don’t
require any input besides the to-be-converted date.

## Installation

``` r
# Install the cutting edge development version from GitHub:
# install.packages("devtools")
devtools::install_github("Glender/DutchDayDummies")
```

## Usage

Start by creating daily dates that conform to our required format and
then run a function like `xmas_day`:

``` r
library(DutchDayDummies)
# create sequence of dates
dates <- seq_days(from="2021-12-24", to="2021-12-27")
# print both results
dates
#> [1] "2021-12-24" "2021-12-25" "2021-12-26" "2021-12-27"
xmas_day(dates)
#> [1] 0 1 0 0
```

We can gather results of multiple holidays with a tibble dataframe:

``` r
library(tibble)
tibble(
  dates = seq_days(from="2021-12-24", to="2022-1-2"),
  xmas = xmas_day(dates),
  boxing = boxing_day(dates),
  nyd = newyears_day(dates),
  nye = newyears_eve(dates)
)
#> # A tibble: 10 x 5
#>    dates       xmas boxing   nyd   nye
#>    <date>     <dbl>  <dbl> <dbl> <dbl>
#>  1 2021-12-24     0      0     0     0
#>  2 2021-12-25     1      0     0     0
#>  3 2021-12-26     0      1     0     0
#>  4 2021-12-27     0      0     0     0
#>  5 2021-12-28     0      0     0     0
#>  6 2021-12-29     0      0     0     0
#>  7 2021-12-30     0      0     0     0
#>  8 2021-12-31     0      0     0     1
#>  9 2022-01-01     0      0     1     0
#> 10 2022-01-02     0      0     0     0
```

## Overview

The DutchDayDummies package provides four categories of important
functions: official holiday dummies, non-official holidays dummies,
vacation dummies, and helper functions.

### Functions to create official dutch holiday dummies:

  - `newyears_day()`
  - `easter_sunday()`
  - `easter_monday()`
  - `kings_day()`
  - `ascension_thursday()`
  - `white_monday()`
  - `white_sunday()`
  - `xmas_day()`
  - `boxing_day()`

### Functions to create non-official dutch holiday dummies:

  - `carnival_sunday()`
  - `valentines_day()`
  - `good_friday()`
  - `ramadan()`
  - `kings_day()`
  - `remembrance_day()`
  - `liberation_day()`
  - `mothers_day()`
  - `sugar_feast()`
  - `fathers_day()`
  - `sacrificial_feast()`
  - `prinsjesdag()`
  - `world_animal_day()`
  - `thanksgiving_day()`
  - `halloween()`
  - `st_martins_day()`
  - `black_friday()`
  - `cyber_monday()`
  - `st_nicholas_day()`
  - `newyears_eve()`

### Functions to create dummies for vacations, or holiday weekends:

  - `autumn_vacation()`

  - `xmas_vacation()`

  - `spring_vacation()`

  - `may_vacation()`

  - `summer_vacation()`

  - `xmas_and_boxing_day()`

  - `pentecost_days()`

  - `easter_weekend()`

### Helper functions:

  - `seq_days(from, to)` to create a vector of daily dates of class
    date.
  - `create_dummies(dates, holiday_dates)` to generate dummies for a
    given holiday that is not included in the package.
  - `nth_weekday_of_a_month(dates, nth, weekday, month)` to find the nth
    weekday in a certain month.
  - `get_wday_before_dates()` tells you the dates of a weekday before a
    specified date.
  - `ummalqura_algorithm(date)` converts gregorian dates to Islamic
    dates.
  - `gauss_easter_algorithm(year)` tells you when easter falls on a
    given year.

## Help

The documentation of all functions can be accessed by `?<function-name>`
or navigate via the package documentation page
`package?DutchDayDummies`.

    # For example:
    ?ummalqura_algorithm
