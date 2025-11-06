# UMAP Projection

Create a umap object from the
[`uwot::umap()`](https://rdrr.io/pkg/uwot/man/umap.html) function.

## Usage

``` r
umap_list(.data, .kmeans_map_tbl, .k_cluster = 5)
```

## Arguments

- .data:

  The data from the
  [`kmeans_user_item_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_user_item_tbl.md)
  function.

- .kmeans_map_tbl:

  The data from the
  [`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md).

- .k_cluster:

  Pick the desired amount of clusters from your analysis of the scree
  plot.

## Value

A list of tibbles and the umap object

## Details

This takes in the user item table/matix that is produced by
[`kmeans_user_item_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_user_item_tbl.md)
function. This function uses the defaults of
[`uwot::umap()`](https://rdrr.io/pkg/uwot/man/umap.html).

## See also

- <https://cran.r-project.org/package=uwot> (CRAN)

- <https://github.com/jlmelville/uwot> (GitHub)

- <https://github.com/jlmelville/uwot> (arXiv paper)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)
library(healthyR)
library(dplyr)
library(broom)

data_tbl <- healthyR_data %>%
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

kmm_tbl <- kmeans_mapped_tbl(uit_tbl)

umap_list(.data = uit_tbl, kmm_tbl, 3)
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
#> Using compatibility `.name_repair`.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
#> Joining, by = "service_line"
#> $umap_obj
#>              [,1]        [,2]
#>  [1,] -1.20154126 -1.47929355
#>  [2,] -1.85642035 -0.92673132
#>  [3,]  2.06673794  0.43277195
#>  [4,]  0.50777595  0.88411787
#>  [5,]  1.39594083  0.91600016
#>  [6,]  2.07423394  0.07035807
#>  [7,] -1.58056147  0.54257897
#>  [8,] -1.28963168 -0.07908270
#>  [9,]  1.64515095  0.56890276
#> [10,]  0.51813190  0.06985691
#> [11,] -1.65750105 -0.38041183
#> [12,]  1.28338330 -0.23311302
#> [13,] -1.14359362 -0.99453851
#> [14,] -1.57960684  0.21351746
#> [15,]  0.07514446  0.57518891
#> [16,] -0.79473717  0.35539225
#> [17,]  1.41217868  0.20038059
#> [18,] -0.87235720 -0.62273332
#> [19,] -1.12270386  0.55439070
#> [20,]  0.89454477  0.56136778
#> [21,]  0.92169182  0.11029461
#> [22,] -1.39727528 -1.20793316
#> [23,]  1.70101528 -0.13128158
#> attr(,"scaled:center")
#> [1] -3.380066  5.532106
#> 
#> $umap_results_tbl
#> # A tibble: 23 × 3
#>         x       y service_line                 
#>     <dbl>   <dbl> <chr>                        
#>  1 -1.20  -1.48   Alcohol Abuse                
#>  2 -1.86  -0.927  Bariatric Surgery For Obesity
#>  3  2.07   0.433  CHF                          
#>  4  0.508  0.884  COPD                         
#>  5  1.40   0.916  CVA                          
#>  6  2.07   0.0704 Carotid Endarterectomy       
#>  7 -1.58   0.543  Cellulitis                   
#>  8 -1.29  -0.0791 Chest Pain                   
#>  9  1.65   0.569  GI Hemorrhage                
#> 10  0.518  0.0699 Joint Replacement            
#> # … with 13 more rows
#> # ℹ Use `print(n = ...)` to see more rows
#> 
#> $kmeans_obj
#> K-means clustering with 3 clusters of sizes 6, 12, 5
#> 
#> Cluster means:
#>   Blue Cross Commercial Compensation Exchange Plans        HMO   Medicaid
#> 1  0.1170278 0.03141187 0.0101665392    0.013865190 0.09822472 0.08557952
#> 2  0.0783745 0.02182129 0.0043244347    0.006202137 0.04493860 0.03684344
#> 3  0.1495475 0.03679700 0.0003066332    0.020729565 0.16252855 0.13072521
#>   Medicaid HMO Medicare A Medicare HMO    No Fault    Self Pay
#> 1   0.14652195  0.3535395   0.10524131 0.007067791 0.031353724
#> 2   0.08001653  0.5625037   0.15152338 0.003475542 0.009976485
#> 3   0.31446157  0.1318675   0.03192357 0.001364577 0.019748398
#> 
#> Clustering vector:
#>  [1] 3 3 2 2 2 2 1 1 2 2 3 2 3 1 2 1 2 1 1 2 2 3 2
#> 
#> Within cluster sum of squares by cluster:
#> [1] 0.08456928 0.09625399 0.19152559
#>  (between_SS / total_SS =  73.6 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"      
#> 
#> $kmeans_cluster_tbl
#> # A tibble: 23 × 2
#>    service_line                  .cluster
#>    <chr>                         <fct>   
#>  1 Alcohol Abuse                 3       
#>  2 Bariatric Surgery For Obesity 3       
#>  3 CHF                           2       
#>  4 COPD                          2       
#>  5 CVA                           2       
#>  6 Carotid Endarterectomy        2       
#>  7 Cellulitis                    1       
#>  8 Chest Pain                    1       
#>  9 GI Hemorrhage                 2       
#> 10 Joint Replacement             2       
#> # … with 13 more rows
#> # ℹ Use `print(n = ...)` to see more rows
#> 
#> $umap_kmeans_cluster_results_tbl
#> # A tibble: 23 × 4
#>         x       y service_line                  .cluster
#>     <dbl>   <dbl> <chr>                         <fct>   
#>  1 -1.20  -1.48   Alcohol Abuse                 3       
#>  2 -1.86  -0.927  Bariatric Surgery For Obesity 3       
#>  3  2.07   0.433  CHF                           2       
#>  4  0.508  0.884  COPD                          2       
#>  5  1.40   0.916  CVA                           2       
#>  6  2.07   0.0704 Carotid Endarterectomy        2       
#>  7 -1.58   0.543  Cellulitis                    1       
#>  8 -1.29  -0.0791 Chest Pain                    1       
#>  9  1.65   0.569  GI Hemorrhage                 2       
#> 10  0.518  0.0699 Joint Replacement             2       
#> # … with 13 more rows
#> # ℹ Use `print(n = ...)` to see more rows
#> 
```
