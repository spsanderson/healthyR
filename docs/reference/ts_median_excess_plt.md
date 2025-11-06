# Create a plot showing the excess of the median value

Plot out the excess +/- of the median value grouped by certain time
parameters.

## Usage

``` r
ts_median_excess_plt(
  .data,
  .date_col,
  .value_col,
  .x_axis,
  .ggplot_group_var,
  .years_back
)
```

## Arguments

- .data:

  The data that is being analyzed, data must be a tibble/data.frame.

- .date_col:

  The column of the tibble that holds the date.

- .value_col:

  The column that holds the value of interest.

- .x_axis:

  What is the be the x-axis, day, week, etc.

- .ggplot_group_var:

  The variable to group the ggplot on.

- .years_back:

  How many yeas back do you want to go in order to compute the median
  value.

## Value

A `ggplot2` plot

## Details

- Supply data that you want to view and you will see the excess +/- of
  the median values over a specified time series tibble.

## See also

Other Plotting Functions:
[`diverging_bar_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_bar_plt.md),
[`diverging_lollipop_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_lollipop_plt.md),
[`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md),
[`los_ra_index_plt()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_plt.md),
[`ts_alos_plt()`](https://www.spsanderson.com/healthyR/reference/ts_alos_plt.md),
[`ts_plt()`](https://www.spsanderson.com/healthyR/reference/ts_plt.md),
[`ts_readmit_rate_plt()`](https://www.spsanderson.com/healthyR/reference/ts_readmit_rate_plt.md)

## Examples

``` r
suppressPackageStartupMessages(library(timetk))

ts_signature_tbl(
  .data       = m4_daily
  , .date_col = date
) %>%
ts_median_excess_plt(
  .date_col           = date
  , .value_col        = value
  , .x_axis           = month
  , .ggplot_group_var = year
  , .years_back       = 1
)
#> pad applied on the interval: day
#> Joining with `by = join_by(month)`
#> Warning: Removed 261 rows containing missing values or values outside the scale range
#> (`geom_line()`).

```
