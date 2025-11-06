# Get the optimal binwidth for a histogram

Gives the optimal binwidth for a histogram given a data set, it's value
and the desired amount of bins

## Usage

``` r
opt_bin(.data, .value_col, .iters = 30)
```

## Arguments

- .data:

  The data set in question

- .value_col:

  The column that holds the values

- .iters:

  How many times the cost function loop should run

## Value

A tibble of histogram breakpoints

## Details

- Supply a data.frame/tibble with a value column. from this an optimal
  binwidth will be computed for the amount of binds desired

## See also

Other Utilities:
[`save_to_excel()`](https://www.spsanderson.com/healthyR/reference/save_to_excel.md),
[`sql_left()`](https://www.spsanderson.com/healthyR/reference/sql_left.md),
[`sql_mid()`](https://www.spsanderson.com/healthyR/reference/sql_mid.md),
[`sql_right()`](https://www.spsanderson.com/healthyR/reference/sql_right.md)

## Author

Steven P. Sanderson II, MPH

Modified from Hideaki Shimazaki Department of Physics, Kyoto University
shimazaki at ton.scphys.kyoto-u.ac.jp Feel free to modify/distribute
this program.

## Examples

``` r
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))

df_tbl <- rnorm(n = 1000, mean = 0, sd = 1)
df_tbl <- df_tbl %>%
  as_tibble() %>%
  set_names("value")

df_tbl %>%
  opt_bin(
    .value_col = value
    , .iters = 100
  )
#> # A tibble: 11 × 1
#>      value
#>      <dbl>
#>  1 -2.95  
#>  2 -2.34  
#>  3 -1.74  
#>  4 -1.13  
#>  5 -0.526 
#>  6  0.0800
#>  7  0.686 
#>  8  1.29  
#>  9  1.90  
#> 10  2.50  
#> 11  3.11  
```
