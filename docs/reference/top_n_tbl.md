# Top N tibble

Get a tibble returned with n records sorted either by descending order
(default) or ascending order.

## Usage

``` r
top_n_tbl(.data, .n_records, .arrange_value = TRUE, ...)
```

## Arguments

- .data:

  The data you want to pass to the function

- .n_records:

  How many records you want returned

- .arrange_value:

  A boolean with TRUE as the default. TRUE sorts data in descending
  order

- ...:

  The columns you want to pass to the function.

## Details

- Requires a data.frame/tibble

- Requires at least one column to be chosen inside of the ...

- Will return the tibble in sorted order that is chosen with descending
  as the default

## See also

Other Data Table Functions:
[`category_counts_tbl()`](https://www.spsanderson.com/healthyR/reference/category_counts_tbl.md),
[`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md),
[`named_item_list()`](https://www.spsanderson.com/healthyR/reference/named_item_list.md),
[`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md),
[`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)

df <- healthyR_data

df_tbl <- top_n_tbl(
  .data = df
  , .n_records = 3
  , .arrange_value = TRUE
  , service_line
  , payer_grouping
)

print(df_tbl)
#> # A tibble: 3 × 3
#>   service_line       payer_grouping     n
#>   <chr>              <chr>          <int>
#> 1 Medical            Medicare A     30250
#> 2 General Outpatient Medicare B     13646
#> 3 General Outpatient Blue Cross     10607
```
