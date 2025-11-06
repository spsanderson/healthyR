# K-Means Functions

Takes in a data.frame/tibble and transforms it into an
aggregated/normalized user-item tibble of proportions. The user will
need to input the parameters for the rows/user and the columns/items.

## Usage

``` r
kmeans_user_item_tbl(.data, .row_input, .col_input, .record_input)
```

## Arguments

- .data:

  The data that you want to transform

- .row_input:

  The column that is going to be the row (user)

- .col_input:

  The column that is going to be the column (item)

- .record_input:

  The column that is going to be summed up for the aggregattion and
  normalization process.

## Value

A aggregated/normalized user item tibble

## Details

This function should be used before using a k-mean model. This is
commonly referred to as a user item matrix because "users" tend to be on
the rows and "items" (e.g. orders) on the columns. You must supply a
column that can be summed for the aggregation and normalization process
to occur.

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

 kmeans_user_item_tbl(
   .data           = data_tbl
   , .row_input    = service_line
   , .col_input    =  payer_grouping
   , .record_input = record
 )
#> # A tibble: 23 × 12
#>    service_line   Blue …¹ Comme…² Compe…³ Excha…⁴    HMO Medic…⁵ Medic…⁶ Medic…⁷
#>    <chr>            <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
#>  1 Alcohol Abuse   0.0941 0.0321  5.25e-4 0.0116  0.0788 0.158    0.367   0.173 
#>  2 Bariatric Sur…  0.317  0.0583  0       0.0518  0.168  0.00324  0.343   0.0485
#>  3 CHF             0.0295 0.00958 5.18e-4 0.00414 0.0205 0.0197   0.0596  0.657 
#>  4 COPD            0.0493 0.0228  2.28e-4 0.00548 0.0342 0.0461   0.172   0.520 
#>  5 CVA             0.0647 0.0246  1.07e-3 0.0107  0.0524 0.0289   0.0764  0.555 
#>  6 Carotid Endar…  0.0845 0.0282  0       0       0.0141 0        0.0282  0.648 
#>  7 Cellulitis      0.110  0.0339  1.18e-2 0.00847 0.0805 0.0869   0.192   0.355 
#>  8 Chest Pain      0.144  0.0391  2.90e-3 0.00543 0.112  0.0522   0.159   0.324 
#>  9 GI Hemorrhage   0.0542 0.0175  1.25e-3 0.00834 0.0480 0.0350   0.0855  0.588 
#> 10 Joint Replace…  0.139  0.0179  3.36e-2 0.00673 0.0516 0        0.0874  0.5   
#> # … with 13 more rows, 3 more variables: `Medicare HMO` <dbl>,
#> #   `No Fault` <dbl>, `Self Pay` <dbl>, and abbreviated variable names
#> #   ¹​`Blue Cross`, ²​Commercial, ³​Compensation, ⁴​`Exchange Plans`, ⁵​Medicaid,
#> #   ⁶​`Medicaid HMO`, ⁷​`Medicare A`
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```
