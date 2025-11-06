# K-Means Scree Plot

Create a scree-plot from the
[`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md)
function.

## Usage

``` r
kmeans_scree_plt(.data)
```

## Arguments

- .data:

  The data from the
  [`kmeans_mapped_tbl()`](https://www.spsanderson.com/healthyR/reference/kmeans_mapped_tbl.md)
  function

## Value

A ggplot2 plot

## Details

Outputs a scree-plot

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

kmm_tbl <- kmeans_mapped_tbl(ui_tbl)

kmeans_scree_plt(.data = kmm_tbl)

```
