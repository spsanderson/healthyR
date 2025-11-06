# Counts by Category

Get the counts of a column by a particular grouping if supplied,
otherwise just get counts of a column.

## Usage

``` r
category_counts_tbl(.data, .count_col, .arrange_value = TRUE, ...)
```

## Arguments

- .data:

  The data.frame/tibble supplied.

- .count_col:

  The column that has the values you want to count.

- .arrange_value:

  Defaults to true, this will arrange the resulting tibble in descending
  order by .count_col

- ...:

  Place the values you want to pass in for grouping here.

## Details

- Requires a data.frame/tibble.

- Requires a value column, a column that is going to counted.

## See also

Other Data Table Functions:
[`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md),
[`named_item_list()`](https://www.spsanderson.com/healthyR/reference/named_item_list.md),
[`top_n_tbl()`](https://www.spsanderson.com/healthyR/reference/top_n_tbl.md),
[`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md),
[`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)
#> 
#> == Welcome to healthyR.data ===========================================================================
#> If you find this package useful, please leave a star: 
#>    https://github.com/spsanderson/healthyR.data'
#> 
#> If you encounter a bug or want to request an enhancement please file an issue at:
#>    https://github.com/spsanderson/healthyR.data/issues
#> 
#> Thank you for using healthyR.data
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

healthyR_data %>%
  category_counts_tbl(
    .count_col = payer_grouping
    , .arrange = TRUE
    , ip_op_flag
  )
#> # A tibble: 25 × 3
#>    ip_op_flag payer_grouping     n
#>    <chr>      <chr>          <int>
#>  1 I          Medicare A     52621
#>  2 O          Medicare B     22270
#>  3 I          Medicaid HMO   15466
#>  4 I          Medicare HMO   13572
#>  5 O          Blue Cross     13560
#>  6 I          Blue Cross     10797
#>  7 O          Medicaid HMO   10018
#>  8 O          HMO             9331
#>  9 I          HMO             8113
#> 10 I          Medicaid        7131
#> # ℹ 15 more rows

healthyR_data %>%
  category_counts_tbl(
    .count_col = ip_op_flag
    , .arrange_value = TRUE
    , service_line
  )
#> # A tibble: 30 × 3
#>    service_line                                 ip_op_flag     n
#>    <chr>                                        <chr>      <int>
#>  1 Medical                                      I          64435
#>  2 General Outpatient                           O          50526
#>  3 Surgical                                     I          14916
#>  4 Colonoscopy/Endoscopy                        O          11486
#>  5 Cataract Removal                             O           4930
#>  6 COPD                                         I           4398
#>  7 CHF                                          I           3871
#>  8 Pneumonia                                    I           3323
#>  9 Cellulitis                                   I           3311
#> 10 Major Depression/Bipolar Affective Disorders I           2866
#> # ℹ 20 more rows
```
