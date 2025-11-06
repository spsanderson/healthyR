# K-Means Mapper

Create a tibble that maps the
[`kmeans_obj()`](https://www.spsanderson.com/healthyR/reference/kmeans_obj.md)
using [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
to create a nested data.frame/tibble that holds n centers. This tibble
will be used to help create a scree plot.

## Usage

``` r
kmeans_mapped_tbl(.data, .centers = 15)
```

## Arguments

- .data:

  You must have a tibble in the working environment from the
  [`kmeans_user_item_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_user_item_tbl.md)

- .centers:

  How many different centers do you want to try

## Value

A nested tibble

## Details

Takes in a single parameter of .centers. This is used to create the
tibble and map the
[`kmeans_obj()`](https://www.spsanderson.com/healthyR/reference/kmeans_obj.md)
function down the list creating a nested tibble.

## See also

<https://en.wikipedia.org/wiki/Scree_plot>

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

kmeans_mapped_tbl(ui_tbl)
#> # A tibble: 15 × 3
#>    centers k_means  glance          
#>      <int> <list>   <list>          
#>  1       1 <kmeans> <tibble [1 × 4]>
#>  2       2 <kmeans> <tibble [1 × 4]>
#>  3       3 <kmeans> <tibble [1 × 4]>
#>  4       4 <kmeans> <tibble [1 × 4]>
#>  5       5 <kmeans> <tibble [1 × 4]>
#>  6       6 <kmeans> <tibble [1 × 4]>
#>  7       7 <kmeans> <tibble [1 × 4]>
#>  8       8 <kmeans> <tibble [1 × 4]>
#>  9       9 <kmeans> <tibble [1 × 4]>
#> 10      10 <kmeans> <tibble [1 × 4]>
#> 11      11 <kmeans> <tibble [1 × 4]>
#> 12      12 <kmeans> <tibble [1 × 4]>
#> 13      13 <kmeans> <tibble [1 × 4]>
#> 14      14 <kmeans> <tibble [1 × 4]>
#> 15      15 <kmeans> <tibble [1 × 4]>
```
