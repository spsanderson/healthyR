# Plot ALOS - Average Length of Stay

Plot ALOS - Average Length of Stay

## Usage

``` r
ts_alos_plt(.data, .date_col, .value_col, .by_grouping, .interactive)
```

## Arguments

- .data:

  The time series data you need to pass

- .date_col:

  The date column

- .value_col:

  The value column

- .by_grouping:

  How you want the data summarized - "sec", "min", "hour", "day",
  "week", "month", "quarter" or "year"

- .interactive:

  TRUE or FALSE. TRUE returns a `plotly` plot and FALSE returns a static
  `ggplot2` plot

## Value

A timetk time series plot

## Details

- Expects a tibble with a date time column and a value column

- Uses `timetk` for underlying sumarization and plot

- If .by_grouping is missing it will default to "day"

- A static ggplot2 object is return if the .interactive function is
  FALSE otherwise a `plotly` plot is returned.

## See also

Other Plotting Functions:
[`diverging_bar_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_bar_plt.md),
[`diverging_lollipop_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_lollipop_plt.md),
[`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md),
[`los_ra_index_plt()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_plt.md),
[`ts_median_excess_plt()`](https://www.spsanderson.com/healthyR/reference/ts_median_excess_plt.md),
[`ts_plt()`](https://www.spsanderson.com/healthyR/reference/ts_plt.md),
[`ts_readmit_rate_plt()`](https://www.spsanderson.com/healthyR/reference/ts_readmit_rate_plt.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR)
library(healthyR.data)
library(timetk)
library(dplyr)
library(purrr)

# Make A Series of Dates ----
data_tbl <- healthyR_data

df_tbl <- data_tbl %>%
    filter(ip_op_flag == "I") %>%
    select(visit_end_date_time, length_of_stay) %>%
    summarise_by_time(
        .date_var = visit_end_date_time
        , .by     = "day"
        , visits  = mean(length_of_stay, na.rm = TRUE)
    ) %>%
    filter_by_time(
        .date_var     = visit_end_date_time
        , .start_date = "2012"
        , .end_date   = "2019"
    ) %>%
    set_names("Date","Values")

ts_alos_plt(
  .data = df_tbl
  , .date_col = Date
  , .value_col = Values
  , .by = "month"
  , .interactive = FALSE
)
#> Ignoring unknown labels:
#> • colour : "Legend"

```
