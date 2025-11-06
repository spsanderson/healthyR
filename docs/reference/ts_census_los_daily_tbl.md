# Time Series - Census and LOS by Day

Sometimes it is important to know what the census was on any given day,
or what the average length of stay is on given day, including for those
patients that are not yet discharged. This can be easily achieved. This
will return one record for every account so the data will still need to
be summarized. If there are multiple entries per day then those records
will show up and you will therefore have multiple entries in the column
`date` in the resulting `tibble`. If you want to aggregate from there
you should be able to do so easily.

If you have a record where the `.start_date_col` is filled in but the
corresponding `end_date` is null then the end date will be set equal to
[`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

If a record has a `start_date` that is `NA` then it will be discarded.

**This function can take a little bit of time to run while the join
comparison runs.**

## Usage

``` r
ts_census_los_daily_tbl(
  .data,
  .keep_nulls_only = FALSE,
  .start_date_col,
  .end_date_col,
  .by_time = "day"
)
```

## Arguments

- .data:

  The data you want to pass to the function

- .keep_nulls_only:

  A boolean that will keep only those records that have a NULL end date,
  meaning the patient is still admitted. The default is FALSE which
  brings back all records.

- .start_date_col:

  The column containing the start date for the record

- .end_date_col:

  The column containing the end date for the record.

- .by_time:

  How you want the data presented, defaults to day and should remain
  that way unless you need more granular data.

## Value

A tibble object

## Details

- Requires a dataset that has at least a start date column and an end
  date column

- Takes a single boolean parameter

## See also

Other Data Table Functions:
[`category_counts_tbl()`](https://www.spsanderson.com/healthyR/reference/category_counts_tbl.md),
[`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md),
[`named_item_list()`](https://www.spsanderson.com/healthyR/reference/named_item_list.md),
[`top_n_tbl()`](https://www.spsanderson.com/healthyR/reference/top_n_tbl.md),
[`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR)
library(healthyR.data)
library(dplyr)

df <- healthyR_data

df_tbl <- df %>%
  filter(ip_op_flag == "I") %>%
  select(visit_start_date_time, visit_end_date_time) %>%
  timetk::filter_by_time(.date_var = visit_start_date_time, .start_date = "2020")

ts_census_los_daily_tbl(
   .data              = df_tbl
   , .keep_nulls_only = FALSE
   , .start_date_col  = visit_start_date_time
   , .end_date_col    = visit_end_date_time
)
#> # A tibble: 45,572 × 5
#>    date       visit_start_date_time visit_end_date_time   los census
#>    <date>     <date>                <date>              <int>  <dbl>
#>  1 2020-01-01 2020-01-01            2020-01-02              1      1
#>  2 2020-01-01 2020-01-01            2020-01-02              1      1
#>  3 2020-01-01 2020-01-01            2020-01-02              1      1
#>  4 2020-01-01 2020-01-01            2020-01-03              2      1
#>  5 2020-01-01 2020-01-01            2020-01-03              2      1
#>  6 2020-01-01 2020-01-01            2020-01-03              2      1
#>  7 2020-01-01 2020-01-01            2020-01-04              3      1
#>  8 2020-01-01 2020-01-01            2020-01-04              3      1
#>  9 2020-01-01 2020-01-01            2020-01-04              3      1
#> 10 2020-01-01 2020-01-01            2020-01-05              4      1
#> # ℹ 45,562 more rows
```
