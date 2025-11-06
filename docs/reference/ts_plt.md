# Time Series Plot

This is a warpper function to the
[`timetk::plot_time_series()`](https://business-science.github.io/timetk/reference/plot_time_series.html)
function with a limited functionality parameter set. To see the full
reference please visit the `timetk` package site.

## Usage

``` r
ts_plt(
  .data,
  .date_col,
  .value_col,
  .color_col = NULL,
  .facet_col = NULL,
  .facet_ncol = NULL,
  .interactive = FALSE
)
```

## Arguments

- .data:

  The data to pass to the function, must be a tibble/data.frame.

- .date_col:

  The column holding the date.

- .value_col:

  The column holding the value.

- .color_col:

  The column holding the variable for color.

- .facet_col:

  The column holding the variable for faceting.

- .facet_ncol:

  How many columns do you want.

- .interactive:

  Return a `plotly` plot if set to TRUE and a static `ggplot2` plot if
  set to FALSE. The default is FALSE.

## Value

A `plotly` plot or a `ggplot2` static plot

## Details

This function takes only a few of the arguments in the function and
presets others while choosing the defaults on others. The smoother
functionality is turned off.

## See also

<https://business-science.github.io/timetk/reference/plot_time_series.html>

Other Plotting Functions:
[`diverging_bar_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_bar_plt.md),
[`diverging_lollipop_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_lollipop_plt.md),
[`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md),
[`los_ra_index_plt()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_plt.md),
[`ts_alos_plt()`](https://www.spsanderson.com/healthyR/reference/ts_alos_plt.md),
[`ts_median_excess_plt()`](https://www.spsanderson.com/healthyR/reference/ts_median_excess_plt.md),
[`ts_readmit_rate_plt()`](https://www.spsanderson.com/healthyR/reference/ts_readmit_rate_plt.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
library(timetk)
library(healthyR.data)

healthyR.data::healthyR_data %>%
  filter(ip_op_flag == "I") %>%
  select(visit_end_date_time, service_line) %>%
  filter_by_time(
    .date_var = visit_end_date_time
    , .start_date = "2020"
    ) %>%
  group_by(service_line) %>%
  summarize_by_time(
    .date_var = visit_end_date_time
    , .by = "month"
    , visits = n()
  ) %>%
 ungroup() %>%
 ts_plt(
   .date_col = visit_end_date_time
   , .value_col = visits
   , .color_col = service_line
 )

```
