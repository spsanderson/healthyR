# Make a Time Enhanced Tibble

Returns a tibble that adds the time series signature from the
[`timetk::tk_augment_timeseries_signature()`](https://business-science.github.io/timetk/reference/tk_augment_timeseries.html)
function. All added from a chosen date column defined by the `.date_col`
parameter.

## Usage

``` r
ts_signature_tbl(.data, .date_col, .pad_time = TRUE, ...)
```

## Arguments

- .data:

  The data that is being analyzed.

- .date_col:

  The column that holds the date.

- .pad_time:

  Boolean TRUE/FALSE. If TRUE then the
  [`timetk::pad_by_time()`](https://business-science.github.io/timetk/reference/pad_by_time.html)
  function is called and used on the data.frame before the modification.
  The default is TRUE.

- ...:

  Grouping variables to be used by
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  before using
  [`timetk::pad_by_time()`](https://business-science.github.io/timetk/reference/pad_by_time.html)

## Value

A tibble

## Details

- Supply data with a date column and this will add the year, month,
  week, week day and hour to the tibble. The original date column is
  kept.

- Returns a time-series signature tibble.

- You must know the data going into the function and if certain columns
  should be dropped or kept when using further functions

## See also

Other Data Table Functions:
[`category_counts_tbl()`](https://www.spsanderson.com/healthyR/reference/category_counts_tbl.md),
[`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md),
[`named_item_list()`](https://www.spsanderson.com/healthyR/reference/named_item_list.md),
[`top_n_tbl()`](https://www.spsanderson.com/healthyR/reference/top_n_tbl.md),
[`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(timetk)

ts_signature_tbl(
  .data       = m4_daily
  , .date_col = date
  , .pad_time = TRUE
  , id
)
#> pad applied on the interval: day
#> # A tibble: 9,743 × 31
#>    id    date       value index.num  diff  year year.iso  half quarter month
#>    <fct> <date>     <dbl>     <dbl> <dbl> <int>    <int> <int>   <int> <int>
#>  1 D410  1978-06-23 9109. 267408000    NA  1978     1978     1       2     6
#>  2 D410  1978-06-24 9103. 267494400 86400  1978     1978     1       2     6
#>  3 D410  1978-06-25 9116. 267580800 86400  1978     1978     1       2     6
#>  4 D410  1978-06-26 9116. 267667200 86400  1978     1978     1       2     6
#>  5 D410  1978-06-27 9106. 267753600 86400  1978     1978     1       2     6
#>  6 D410  1978-06-28 9094. 267840000 86400  1978     1978     1       2     6
#>  7 D410  1978-06-29 9094. 267926400 86400  1978     1978     1       2     6
#>  8 D410  1978-06-30 9084. 268012800 86400  1978     1978     1       2     6
#>  9 D410  1978-07-01 9081. 268099200 86400  1978     1978     2       3     7
#> 10 D410  1978-07-02 9047. 268185600 86400  1978     1978     2       3     7
#> # ℹ 9,733 more rows
#> # ℹ 21 more variables: month.xts <int>, month.lbl <ord>, day <int>, hour <int>,
#> #   minute <int>, second <int>, hour12 <int>, am.pm <int>, wday <int>,
#> #   wday.xts <int>, wday.lbl <ord>, mday <int>, qday <int>, yday <int>,
#> #   mweek <int>, week <int>, week.iso <int>, week2 <int>, week3 <int>,
#> #   week4 <int>, mday7 <int>
```
