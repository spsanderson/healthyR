# Plot LOS and Readmit Index with Variance

Plot the index of the length of stay and readmit rate against each other
along with the variance

## Usage

``` r
los_ra_index_plt(.data)
```

## Arguments

- .data:

  The data supplied from
  [`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md)

## Value

A `patchwork` `ggplot2` plot

## Details

- Expects a tibble

- Expects a Length of Stay and Readmit column, must be numeric

- Uses `cowplot` to stack plots

## See also

Other Plotting Functions:
[`diverging_bar_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_bar_plt.md),
[`diverging_lollipop_plt()`](https://www.spsanderson.com/healthyR/reference/diverging_lollipop_plt.md),
[`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md),
[`ts_alos_plt()`](https://www.spsanderson.com/healthyR/reference/ts_alos_plt.md),
[`ts_median_excess_plt()`](https://www.spsanderson.com/healthyR/reference/ts_median_excess_plt.md),
[`ts_plt()`](https://www.spsanderson.com/healthyR/reference/ts_plt.md),
[`ts_readmit_rate_plt()`](https://www.spsanderson.com/healthyR/reference/ts_readmit_rate_plt.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))

data_tbl <- tibble(
  "alos"                 = runif(186, 1, 20)
  , "elos"               = runif(186, 1, 17)
  , "readmit_rate"       = runif(186, 0, .25)
  , "readmit_rate_bench" = runif(186, 0, .2)
)

los_ra_index_summary_tbl(
  .data = data_tbl
  , .max_los       = 15
  , .alos_col      = alos
  , .elos_col      = elos
  , .readmit_rate  = readmit_rate
  , .readmit_bench = readmit_rate_bench
) %>%
  los_ra_index_plt()


los_ra_index_summary_tbl(
  .data = data_tbl
  , .max_los       = 10
  , .alos_col      = alos
  , .elos_col      = elos
  , .readmit_rate  = readmit_rate
  , .readmit_bench = readmit_rate_bench
) %>%
  los_ra_index_plt()

```
