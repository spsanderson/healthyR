# Make LOS and Readmit Index Summary Tibble

Create the length of stay and readmit index summary tibble

## Usage

``` r
los_ra_index_summary_tbl(
  .data,
  .max_los = 15,
  .alos_col,
  .elos_col,
  .readmit_rate,
  .readmit_bench
)
```

## Arguments

- .data:

  The data you are going to analyze.

- .max_los:

  You can give a maximum LOS value. Lets say you typically do not see
  los over 15 days, you would then set .max_los to 15 and all values
  greater than .max_los will be grouped to .max_los

- .alos_col:

  The Average Length of Stay column

- .elos_col:

  The Expected Length of Stay column

- .readmit_rate:

  The Actual Readmit Rate column

- .readmit_bench:

  The Expected Readmit Rate column

## Value

A tibble

## Details

- Expects a tibble

- Expects the following columns and there should only be these 4

  - Length Of Stay Actual - Should be an integer

  - Length Of Stacy Benchmark - Should be an integer

  - Readmit Rate Actual - Should be 0/1 for each record, 1 = readmitted,
    0 did not.

  - Readmit Rate Benchmark - Should be a percentage from the benchmark
    file.

- This will add a column called visits that will be the count of records
  per length of stay from 1 to .max_los

- The .max_los param can be left blank and the function will default
  to 15. If this is not a good default and you don't know what it should
  be then set it to 75 percentile from the
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) function
  using the defaults, like so .max_los =
  `stats::quantile(data_tbl$alos)[[4]]`

- Uses all data to compute variance, if you want it for a particular
  time frame you will have to filter the data that goes into the .data
  argument. It is suggested to use
  [`timetk::filter_by_time()`](https://business-science.github.io/timetk/reference/filter_by_time.html)

- The index is computed as the excess of the length of stay or readmit
  rates over their respective expectations.

## See also

Other Data Table Functions:
[`category_counts_tbl()`](https://www.spsanderson.com/healthyR/reference/category_counts_tbl.md),
[`named_item_list()`](https://www.spsanderson.com/healthyR/reference/named_item_list.md),
[`top_n_tbl()`](https://www.spsanderson.com/healthyR/reference/top_n_tbl.md),
[`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md),
[`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))

data_tbl <- tibble(
  "alos"            = runif(186, 1, 20)
  , "elos"          = runif(186, 1, 17)
  , "readmit_rate"  = runif(186, 0, .25)
  , "readmit_bench" = runif(186, 0, .2)
)

los_ra_index_summary_tbl(
  .data = data_tbl
  , .max_los       = 15
  , .alos_col      = alos
  , .elos_col      = elos
  , .readmit_rate  = readmit_rate
  , .readmit_bench = readmit_bench
)
#> # A tibble: 15 × 4
#>    los_group los_index rar_index los_ra_var
#>        <dbl>     <dbl>     <dbl>      <dbl>
#>  1         1     0.100     1.4        1.30 
#>  2         2     0.405     0.583      1.01 
#>  3         3     0.353     1.75       1.40 
#>  4         4     0.491     1.25       0.759
#>  5         5     0.551     1.5        0.949
#>  6         6     0.685     1.09       0.406
#>  7         7     0.880     2.17       1.29 
#>  8         8     0.786     1.56       0.769
#>  9         9     0.989     1.11       0.122
#> 10        10     2.00      1.5        1.50 
#> 11        11     1.32      1          0.317
#> 12        12     1.29      1.36       0.656
#> 13        13     1.18      1.6        0.780
#> 14        14     1.65      0.667      0.986
#> 15        15     1.91      1.3        1.21 

los_ra_index_summary_tbl(
  .data = data_tbl
  , .max_los       = 10
  , .alos_col      = alos
  , .elos_col      = elos
  , .readmit_rate  = readmit_rate
  , .readmit_bench = readmit_bench
)
#> # A tibble: 10 × 4
#>    los_group los_index rar_index los_ra_var
#>        <dbl>     <dbl>     <dbl>      <dbl>
#>  1         1     0.100     1.4        1.30 
#>  2         2     0.405     0.583      1.01 
#>  3         3     0.353     1.75       1.40 
#>  4         4     0.491     1.25       0.759
#>  5         5     0.551     1.5        0.949
#>  6         6     0.685     1.09       0.406
#>  7         7     0.880     2.17       1.29 
#>  8         8     0.786     1.56       0.769
#>  9         9     0.989     1.11       0.122
#> 10        10     1.63      1.18       0.808
```
