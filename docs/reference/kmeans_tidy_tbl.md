# K-Means tidy Functions

K-Means tidy functions

## Usage

``` r
kmeans_tidy_tbl(.kmeans_obj, .data, .tidy_type = "tidy")
```

## Arguments

- .kmeans_obj:

  A [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) object

- .data:

  The user item tibble created from
  [`kmeans_user_item_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_user_item_tbl.md)

- .tidy_type:

  "tidy","glance", or "augment"

## Value

A tibble

## Details

Takes in a k-means object and its associated user item tibble and then
returns one of the items asked for. Either:
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html),
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
or
[`broom::augment()`](https://broom.tidymodels.org/reference/reexports.html).
The function defaults to
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)
library(dplyr)
library(broom)

data_tbl <- healthyR_data%>%
   filter(ip_op_flag == "I") %>%
   filter(payer_grouping != "Medicare B") %>%
   filter(payer_grouping != "?") %>%
   select(service_line, payer_grouping) %>%
   mutate(record = 1) %>%
   as_tibble()

 uit_tbl <- kmeans_user_item_tbl(
   .data           = data_tbl
   , .row_input    = service_line
   , .col_input    =  payer_grouping
   , .record_input = record
 )

 km_obj  <- kmeans_obj(uit_tbl)

 kmeans_tidy_tbl(
   .kmeans_obj  = km_obj
   , .data      = uit_tbl
   , .tidy_type = "augment"
 )
#> # A tibble: 23 × 2
#>    service_line                  cluster
#>    <chr>                         <fct>  
#>  1 Alcohol Abuse                 5      
#>  2 Bariatric Surgery For Obesity 1      
#>  3 CHF                           3      
#>  4 COPD                          3      
#>  5 CVA                           3      
#>  6 Carotid Endarterectomy        3      
#>  7 Cellulitis                    4      
#>  8 Chest Pain                    4      
#>  9 GI Hemorrhage                 3      
#> 10 Joint Replacement             3      
#> # … with 13 more rows
#> # ℹ Use `print(n = ...)` to see more rows

 kmeans_tidy_tbl(
   .kmeans_obj  = km_obj
   , .data      = uit_tbl
   , .tidy_type = "glance"
 )
#> # A tibble: 1 × 4
#>   totss tot.withinss betweenss  iter
#>   <dbl>        <dbl>     <dbl> <int>
#> 1  1.41        0.202      1.21     2

 kmeans_tidy_tbl(
   .kmeans_obj  = km_obj
   , .data      = uit_tbl
   , .tidy_type = "tidy"
 ) %>%
   glimpse()
#> Rows: 5
#> Columns: 14
#> $ `Blue Cross`     <dbl> 0.27188303, 0.00000000, 0.07837450, 0.13375082, 0.079…
#> $ Commercial       <dbl> 0.05712358, 0.00000000, 0.02182129, 0.03542694, 0.027…
#> $ Compensation     <dbl> 0.0003293808, 0.0000000000, 0.0043244347, 0.012199847…
#> $ `Exchange Plans` <dbl> 0.039065198, 0.000000000, 0.006202137, 0.016160901, 0…
#> $ HMO              <dbl> 0.18065096, 0.27272727, 0.04493860, 0.10724914, 0.077…
#> $ Medicaid         <dbl> 0.04246134, 0.18181818, 0.03684344, 0.05150211, 0.214…
#> $ `Medicaid HMO`   <dbl> 0.24760799, 0.45454545, 0.08001653, 0.13107693, 0.282…
#> $ `Medicare A`     <dbl> 0.10958146, 0.09090909, 0.56250366, 0.35217108, 0.236…
#> $ `Medicare HMO`   <dbl> 0.03584494, 0.00000000, 0.15152338, 0.11769769, 0.043…
#> $ `No Fault`       <dbl> 0.000000000, 0.000000000, 0.003475542, 0.008242686, 0…
#> $ `Self Pay`       <dbl> 0.015452115, 0.000000000, 0.009976485, 0.034521844, 0…
#> $ size             <int> 2, 1, 12, 5, 3
#> $ withinss         <dbl> 0.03549821, 0.00000000, 0.09625399, 0.02592247, 0.044…
#> $ cluster          <fct> 1, 2, 3, 4, 5
```
