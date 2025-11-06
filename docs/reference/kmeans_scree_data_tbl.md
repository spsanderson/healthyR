# K-Means Scree Plot Data Table

Take data from the
[`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md)
and unnest it into a tibble for inspection and for use in the
[`kmeans_scree_plt()`](https://www.spsanderson.com/healthyR/reference/kmeans_scree_plt.md)
function.

## Usage

``` r
kmeans_scree_data_tbl(.data)
```

## Arguments

- .data:

  You must have a tibble in the working environment from the
  [`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md)

## Value

A nested tibble

## Details

Takes in a single parameter of .data from
[`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md)
and transforms it into a tibble that is used for
[`kmeans_scree_plt()`](https://www.spsanderson.com/healthyR/reference/kmeans_scree_plt.md).
It will show the values (tot.withinss) at each center.

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)
library(dplyr)

data_tbl <- healthyR_data%>%
   filter(ip_op_flag == "I") %>%
   filter(payer_grouping != "Medicare B") %>%
   filter(payer_grouping != "?") %>%
   select(service_line, payer_grouping) %>%
   mutate(record = 1) %>%
   as_tibble()

ui_tbl <-  kmeans_user_item_tbl(
   .data           = data_tbl
   , .row_input    = service_line
   , .col_input    =  payer_grouping
   , .record_input = record
 )

kmm_tbl <- kmeans_mapped_tbl(ui_tbl)

kmeans_scree_data_tbl(kmm_tbl)
#> # A tibble: 15 × 2
#>    centers tot.withinss
#>      <int>        <dbl>
#>  1       1       1.41  
#>  2       2       0.592 
#>  3       3       0.372 
#>  4       4       0.276 
#>  5       5       0.202 
#>  6       6       0.159 
#>  7       7       0.124 
#>  8       8       0.0922
#>  9       9       0.0722
#> 10      10       0.0576
#> 11      11       0.0461
#> 12      12       0.0363
#> 13      13       0.0272
#> 14      14       0.0231
#> 15      15       0.0160
```
