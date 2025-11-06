# Plot Readmit Rate

Plot Readmit Rate

## Usage

``` r
ts_readmit_rate_plt(.data, .date_col, .value_col, .by_grouping, .interactive)
```

## Arguments

- .data:

  The data you need to pass.

- .date_col:

  The date column.

- .value_col:

  The value column.

- .by_grouping:

  How you want the data summarized - "sec", "min", "hour", "day",
  "week", "month", "quarter" or "year".

- .interactive:

  TRUE or FALSE. TRUE returns a `plotly` plot and FALSE returns a static
  `ggplot2` plot.

## Value

A `timetk` time series plot that is interactive

## Details

- Expects a tibble with a date time column and a value column

- Uses `timetk` for underlying sumarization and plot

- If .by_grouping is missing it will default to "day"

## See also

Other Plotting Functions:
[`diverging_bar_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_bar_plt.md),
[`diverging_lollipop_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_lollipop_plt.md),
[`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md),
[`los_ra_index_plt()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_plt.md),
[`ts_alos_plt()`](https://www.spsanderson.com/healthyR/reference/ts_alos_plt.md),
[`ts_median_excess_plt()`](https://www.spsanderson.com/healthyR/reference/ts_median_excess_plt.md),
[`ts_plt()`](https://www.spsanderson.com/healthyR/reference/ts_plt.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
set.seed(123)

suppressPackageStartupMessages(library(timetk))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

ts_tbl <- tk_make_timeseries(
  start = "2019-01-01"
  , by = "day"
  , length_out = "1 year 6 months"
)
values <- arima.sim(
  model = list(
    order = c(0, 1, 0))
    , n = 547
    , mean = 1
    , sd = 5
)

df_tbl <- tibble(
  x = ts_tbl
  , y = values
  ) %>%
  set_names("Date","Values")

ts_readmit_rate_plt(
  .data = df_tbl
  , .date_col = Date
  , .value_col = Values
  , .by = "month"
  , .interactive = FALSE
)
#> Ignoring unknown labels:
#> • colour : "Legend"

```
