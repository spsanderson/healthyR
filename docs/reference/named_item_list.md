# Tibble to named list

Takes in a data.frame/tibble and creates a named list from a supplied
grouping variable. Can be used in conjunction with
[`save_to_excel()`](https://www.spsanderson.com/healthyR/reference/save_to_excel.md)
to create a new sheet for each group of data.

## Usage

``` r
named_item_list(.data, .group_col)
```

## Arguments

- .data:

  The data.frame/tibble.

- .group_col:

  The column that contains the groupings.

## Details

- Requires a data.frame/tibble and a grouping column.

## See also

Other Data Table Functions:
[`category_counts_tbl()`](https://www.spsanderson.com/healthyR/reference/category_counts_tbl.md),
[`los_ra_index_summary_tbl()`](https://www.spsanderson.com/healthyR/reference/los_ra_index_summary_tbl.md),
[`top_n_tbl()`](https://www.spsanderson.com/healthyR/reference/top_n_tbl.md),
[`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md),
[`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
library(healthyR.data)

df <- healthyR_data
df_list <- named_item_list(.data = df, .group_col = service_line)
df_list
#> <list_of<
#>   tbl_df<
#>     mrn                     : character
#>     visit_id                : character
#>     visit_start_date_time   : datetime<UTC>
#>     visit_end_date_time     : datetime<UTC>
#>     total_charge_amount     : double
#>     total_amount_due        : double
#>     total_adjustment_amount : double
#>     payer_grouping          : character
#>     total_payment_amount    : double
#>     ip_op_flag              : character
#>     service_line            : character
#>     length_of_stay          : double
#>     expected_length_of_stay : logical
#>     length_of_stay_threshold: logical
#>     los_outlier_flag        : double
#>     readmit_flag            : double
#>     readmit_expectation     : logical
#>   >
#> >[29]>
#> $`Alcohol Abuse`
#> # A tibble: 1,904 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 66681… 1027422… 2011-09-18 18:45:00   2011-09-21 15:24:00              20650.
#>  2 85712… 1715006… 2011-09-24 14:23:00   2011-09-27 22:54:00              19632.
#>  3 45086… 1463793… 2011-09-25 17:22:00   2011-09-30 18:48:00              27028.
#>  4 53136… 1087046… 2011-10-01 08:58:00   2011-10-03 11:10:00              12214.
#>  5 79908… 1933551… 2011-10-02 00:04:00   2011-10-06 13:51:00              30124.
#>  6 29323… 1651882… 2011-10-06 15:08:00   2011-10-07 16:00:00               8571.
#>  7 97809… 1375044… 2011-10-06 17:23:00   2011-10-08 14:19:00              13139.
#>  8 88765… 1040286… 2011-10-09 00:36:00   2011-10-09 15:29:00              37944.
#>  9 13303… 1814203… 2011-09-26 20:00:00   2011-10-10 11:59:00             110695.
#> 10 50646… 1177441… 2011-10-11 01:00:00   2011-10-13 10:16:00              18791.
#> # ℹ 1,894 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Bariatric Surgery For Obesity`
#> # A tibble: 309 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 33197… 1959572… 2012-12-03 13:35:00   2012-12-05 14:42:00              42533.
#>  2 85747… 1935901… 2012-12-03 07:30:00   2012-12-06 13:48:00              47450.
#>  3 89559… 1250962… 2012-12-03 08:39:00   2012-12-06 16:18:00              50382.
#>  4 31228… 1482137… 2013-01-16 06:32:00   2013-01-18 16:07:00              43665.
#>  5 15445… 1953192… 2013-01-16 09:18:00   2013-01-20 18:33:00              52763.
#>  6 22630… 1950977… 2013-02-19 10:28:00   2013-02-21 15:25:00              36038.
#>  7 37398… 1518672… 2013-02-26 06:53:00   2013-03-01 12:38:00              56381.
#>  8 14268… 1903948… 2013-04-09 06:45:00   2013-04-11 18:12:00              43263.
#>  9 78338… 1196711… 2013-04-23 07:16:00   2013-04-26 16:41:00              52300.
#> 10 80152… 1595499… 2013-05-21 06:15:00   2013-05-24 15:59:00              44746.
#> # ℹ 299 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Bariatric Surgery for Obesity Outpatient`
#> # A tibble: 19 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 77785… 3780939… 2011-05-23 05:00:00   2011-05-24 00:00:00              37167.
#>  2 68458… 3778860… 2011-07-18 05:00:00   2011-07-19 00:00:00              26037.
#>  3 47955… 3675064… 2011-07-18 05:00:00   2011-07-19 00:00:00              41552.
#>  4 92298… 3218733… 2011-08-15 05:00:00   2011-08-16 00:00:00              34332.
#>  5 29160… 3547868… 2011-08-30 05:00:00   2011-08-31 00:00:00              37992.
#>  6 45420… 3189581… 2011-09-12 05:00:00   2011-09-13 00:00:00              32744.
#>  7 59818… 3834855… 2011-09-19 05:00:00   2011-09-20 00:00:00              34354.
#>  8 57649… 3921176… 2011-10-17 05:00:00   2011-10-18 00:00:00              31338 
#>  9 11995… 3411333… 2011-11-07 05:00:00   2011-11-08 00:00:00              34795 
#> 10 62541… 3554137… 2011-11-08 05:00:00   2011-11-08 00:00:00              33251.
#> 11 83600… 3562481… 2011-11-15 05:00:00   2011-11-15 00:00:00              33955.
#> 12 10604… 3411030… 2011-11-28 05:00:00   2011-11-29 00:00:00              35638.
#> 13 84084… 3342120… 2012-03-12 05:00:00   2012-03-12 00:00:00              33758.
#> 14 37253… 3461965… 2012-04-23 05:00:00   2012-04-24 00:00:00              36768.
#> 15 85288… 3078841… 2012-06-12 05:00:00   2012-06-13 00:00:00              39139.
#> 16 73239… 3350427… 2012-07-09 05:00:00   2012-07-10 00:00:00              35642.
#> 17 52577… 3074630… 2012-08-20 05:00:00   2012-08-21 00:00:00              37140.
#> 18 62286… 3659346… 2012-08-27 05:00:00   2012-08-28 00:00:00              35030.
#> 19 39164… 3146567… 2012-12-18 05:00:00   2012-12-19 00:00:00              36899.
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $CHF
#> # A tibble: 3,871 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 51458… 1759626… 2011-09-19 09:17:00   2011-09-22 12:32:00              24021.
#>  2 66595… 1901836… 2011-09-19 07:06:00   2011-09-22 18:58:00              33724.
#>  3 21445… 1913857… 2011-09-22 00:21:00   2011-09-27 16:49:00              47369.
#>  4 33735… 1710022… 2011-09-15 12:48:00   2011-09-27 19:52:00              76611.
#>  5 67542… 1377329… 2011-09-17 13:49:00   2011-09-28 13:45:00              81523.
#>  6 69492… 1811099… 2011-09-24 20:33:00   2011-09-29 11:59:00              37112.
#>  7 93559… 1609237… 2011-09-22 04:23:00   2011-10-03 14:21:00              84470.
#>  8 14723… 1713302… 2011-09-15 11:50:00   2011-10-04 19:34:00             158923.
#>  9 49842… 1052618… 2011-09-29 10:40:00   2011-10-05 14:20:00              50061.
#> 10 98654… 1022864… 2011-10-01 01:47:00   2011-10-05 18:50:00              42312.
#> # ℹ 3,861 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $COPD
#> # A tibble: 4,398 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 53222… 1698106… 2011-09-23 00:18:00   2011-09-25 14:18:00              21843.
#>  2 54515… 1461588… 2011-09-18 17:22:00   2011-09-28 17:20:00              98014.
#>  3 91474… 1437392… 2011-09-19 09:01:00   2011-09-30 14:44:00              96463.
#>  4 38370… 1452507… 2011-09-25 18:44:00   2011-09-30 20:42:00              31050.
#>  5 30712… 1117831… 2011-09-26 19:26:00   2011-10-01 09:46:00              33841.
#>  6 11184… 1308449… 2011-09-30 17:41:00   2011-10-01 16:32:00              13971.
#>  7 88190… 1670417… 2011-09-27 22:58:00   2011-10-01 17:03:00              44981.
#>  8 33840… 1957228… 2011-09-26 08:08:00   2011-10-02 15:53:00              44635.
#>  9 20960… 1168285… 2011-09-28 11:09:00   2011-10-03 15:53:00              36926.
#> 10 95644… 1711510… 2011-09-24 23:59:00   2011-10-03 18:10:00              69779.
#> # ℹ 4,388 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $CVA
#> # A tibble: 1,876 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 41238… 1543704… 2011-09-19 15:31:00   2011-09-27 13:11:00             103948.
#>  2 80514… 1330628… 2011-09-21 14:52:00   2011-09-29 17:38:00              94170.
#>  3 12307… 1747094… 2011-09-24 16:34:00   2011-09-30 17:21:00              66481.
#>  4 20903… 1777797… 2011-09-26 12:19:00   2011-10-04 15:38:00              79281.
#>  5 54938… 1173347… 2011-09-30 17:47:00   2011-10-05 16:50:00              49108.
#>  6 93886… 1877891… 2011-09-19 17:47:00   2011-10-05 17:25:00             105698.
#>  7 42704… 1654264… 2011-09-28 22:00:00   2011-10-06 17:08:00              65041.
#>  8 40653… 1576434… 2011-10-05 15:31:00   2011-10-11 14:26:00              81910.
#>  9 26260… 1818390… 2011-09-27 13:43:00   2011-10-13 14:14:00              86323.
#> 10 28646… 1984495… 2011-10-12 10:10:00   2011-10-14 15:15:00              82165.
#> # ℹ 1,866 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Cardiac Catheterization`
#> # A tibble: 2,339 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 66583… 3578143… 2011-01-04 05:00:00   2011-01-04 00:00:00               20852
#>  2 10022… 3911589… 2011-01-04 05:00:00   2011-01-04 00:00:00               18334
#>  3 28724… 3226717… 2011-01-05 05:00:00   2011-01-05 00:00:00               19353
#>  4 54076… 3773686… 2011-01-05 05:00:00   2011-01-05 00:00:00               18334
#>  5 89111… 3082518… 2011-01-13 05:00:00   2011-01-11 00:00:00                1377
#>  6 91877… 3385295… 2011-01-13 05:00:00   2011-01-13 00:00:00               18405
#>  7 89111… 3040689… 2011-01-13 05:00:00   2011-01-13 00:00:00               18587
#>  8 75773… 3380647… 2011-01-14 05:00:00   2011-01-14 00:00:00               20069
#>  9 22385… 3740963… 2011-01-17 05:00:00   2011-01-17 00:00:00               18428
#> 10 91434… 3628134… 2011-01-17 05:00:00   2011-01-17 00:00:00               20056
#> # ℹ 2,329 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Carotid Endarterectomy`
#> # A tibble: 72 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 91886… 1012723… 2011-12-02 11:51:00   2011-12-04 15:32:00              29553.
#>  2 84879… 1998441… 2012-02-02 06:37:00   2012-02-05 19:38:00              35181.
#>  3 35685… 1235700… 2012-03-16 16:26:00   2012-03-23 11:22:00              87273.
#>  4 84879… 1412023… 2012-03-30 09:31:00   2012-04-02 17:57:00              36213.
#>  5 76802… 1501987… 2012-04-03 09:43:00   2012-04-05 13:11:00              27293.
#>  6 21571… 1253613… 2012-03-29 21:26:00   2012-04-05 14:53:00              82206.
#>  7 88958… 1446390… 2012-03-27 19:35:00   2012-04-05 16:35:00              89350.
#>  8 25304… 1257726… 2012-05-29 08:52:00   2012-06-04 11:53:00              57470.
#>  9 85997… 1406791… 2012-05-29 06:40:00   2012-06-04 12:40:00              52616.
#> 10 19040… 1247816… 2012-05-31 06:57:00   2012-06-04 14:30:00              37105.
#> # ℹ 62 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Cataract Removal`
#> # A tibble: 4,930 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 24432… 3904707… 2010-01-05 05:00:00   2010-01-05 00:00:00              31283.
#>  2 42767… 3950850… 2010-01-05 05:00:00   2010-01-05 00:00:00              12833.
#>  3 27436… 3391956… 2010-01-05 05:00:00   2010-01-05 00:00:00              11770.
#>  4 16795… 3399694… 2010-01-05 05:00:00   2010-01-05 00:00:00              12729.
#>  5 26318… 3554661… 2010-01-05 05:00:00   2010-01-05 00:00:00              12833.
#>  6 61280… 3763722… 2010-01-06 05:00:00   2010-01-06 00:00:00              15748.
#>  7 80833… 3967666… 2010-01-06 05:00:00   2010-01-06 00:00:00              14165.
#>  8 11824… 3033075… 2010-01-06 05:00:00   2010-01-06 00:00:00              13065.
#>  9 87504… 3162148… 2010-01-06 05:00:00   2010-01-06 00:00:00              12735.
#> 10 87077… 3940813… 2010-01-07 05:00:00   2010-01-07 00:00:00              11514.
#> # ℹ 4,920 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Cellulitis
#> # A tibble: 3,311 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 91937… 1963079… 2011-09-21 16:35:00   2011-09-23 13:42:00              20322.
#>  2 14415… 1523695… 2011-09-23 13:59:00   2011-09-23 18:00:00               6860.
#>  3 30115… 1278133… 2011-09-21 05:31:00   2011-09-24 10:50:00              28990.
#>  4 11190… 1462795… 2011-09-22 16:32:00   2011-09-25 16:47:00              17947.
#>  5 20122… 1114115… 2011-09-25 23:58:00   2011-09-26 13:30:00               8604.
#>  6 66023… 1101427… 2011-09-25 02:04:00   2011-09-27 14:18:00              26726.
#>  7 36903… 1842522… 2011-09-25 16:14:00   2011-09-27 18:10:00              13143.
#>  8 94675… 1694578… 2011-09-28 19:23:00   2011-09-29 10:40:00              12203 
#>  9 64313… 1204713… 2011-09-19 21:06:00   2011-09-30 16:54:00              95473.
#> 10 81980… 1108154… 2011-09-30 22:52:00   2011-10-01 12:28:00               8901.
#> # ℹ 3,301 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Chest Pain`
#> # A tibble: 2,766 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 62048… 1900262… 2011-09-19 16:53:00   2011-09-20 19:20:00              10426.
#>  2 24825… 1518935… 2011-09-19 21:51:00   2011-09-21 10:55:00              19261.
#>  3 98859… 1774049… 2011-09-19 18:47:00   2011-09-22 12:50:00              27736.
#>  4 60653… 1933930… 2011-09-21 23:51:00   2011-09-22 18:03:00              12116.
#>  5 55103… 1113333… 2011-09-22 04:39:00   2011-09-23 14:54:00              18740.
#>  6 95431… 1167648… 2011-09-22 20:11:00   2011-09-24 16:30:00              17625.
#>  7 87695… 1365338… 2011-09-25 21:52:00   2011-09-26 01:47:00               8509.
#>  8 36204… 1460612… 2011-09-24 20:00:00   2011-09-26 21:47:00              31526.
#>  9 60438… 1812202… 2011-09-27 22:31:00   2011-09-28 11:10:00              11644.
#> 10 84478… 1030571… 2011-09-28 16:29:00   2011-09-28 19:28:00               7463.
#> # ℹ 2,756 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Colonoscopy/Endoscopy`
#> # A tibble: 11,486 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 55897… 3869688… 2010-01-04 05:00:00   2010-01-04 00:00:00               8788.
#>  2 13463… 3990577… 2010-01-05 05:00:00   2010-01-05 00:00:00               7607.
#>  3 80092… 3361584… 2010-01-06 05:00:00   2010-01-06 00:00:00               9567.
#>  4 53897… 3961488… 2010-01-06 05:00:00   2010-01-06 00:00:00               8671.
#>  5 51791… 3022697… 2010-01-07 05:00:00   2010-01-07 00:00:00               7966.
#>  6 94563… 3247268… 2010-01-08 05:00:00   2010-01-08 00:00:00               8695.
#>  7 97321… 3978227… 2010-01-08 05:00:00   2010-01-08 00:00:00               7277.
#>  8 10341… 3542481… 2010-01-08 05:00:00   2010-01-08 00:00:00               8783.
#>  9 40727… 3471312… 2010-01-08 05:00:00   2010-01-08 00:00:00               8988.
#> 10 96631… 3784146… 2010-01-10 20:54:00   2010-01-10 00:00:00               7656.
#> # ℹ 11,476 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`GI Hemorrhage`
#> # A tibble: 2,404 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 45079… 1076298… 2011-09-24 07:08:00   2011-09-26 18:00:00              18964.
#>  2 78632… 1595472… 2011-09-24 15:09:00   2011-09-27 12:06:00              27776.
#>  3 80547… 1555948… 2011-09-20 01:23:00   2011-09-27 21:20:00              87188.
#>  4 21014… 1246530… 2011-09-25 20:58:00   2011-09-28 15:50:00              36404.
#>  5 53480… 1976271… 2011-09-20 02:55:00   2011-09-30 19:32:00             115091.
#>  6 84440… 1899305… 2011-09-28 20:00:00   2011-10-01 12:06:00              26018.
#>  7 55748… 1658072… 2011-09-28 23:44:00   2011-10-02 13:23:00              64608.
#>  8 22340… 1938596… 2011-09-30 20:00:00   2011-10-03 14:00:00              25310.
#>  9 80292… 1911753… 2011-09-30 09:09:00   2011-10-05 15:34:00              55967.
#> 10 33494… 1977982… 2011-08-25 13:20:00   2011-10-05 16:38:00             301838.
#> # ℹ 2,394 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`General Outpatient`
#> # A tibble: 50,526 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 86069… 3519249… 2010-01-04 05:00:00   2010-01-04 00:00:00              25984.
#>  2 60856… 3602225… 2010-01-04 05:00:00   2010-01-04 00:00:00              22774.
#>  3 80673… 3125290… 2010-01-04 05:00:00   2010-01-04 00:00:00              10690.
#>  4 99766… 3372388… 2010-01-04 05:00:00   2010-01-04 00:00:00               7325.
#>  5 23979… 3139267… 2010-01-04 05:00:00   2010-01-04 00:00:00               9119.
#>  6 56640… 3346223… 2010-01-04 05:00:00   2010-01-04 00:00:00              10628 
#>  7 43395… 3323331… 2010-01-04 05:00:00   2010-01-04 00:00:00               8677.
#>  8 54736… 3159812… 2010-01-05 05:00:00   2010-01-05 00:00:00               9604.
#>  9 71143… 3070269… 2010-01-05 05:00:00   2010-01-05 00:00:00               8839.
#> 10 27377… 3185100… 2010-01-05 05:00:00   2010-01-05 00:00:00              10185.
#> # ℹ 50,516 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Joint Replacement`
#> # A tibble: 447 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 35741… 1963731… 2011-09-20 06:42:00   2011-09-23 18:27:00              70655.
#>  2 45785… 1385680… 2011-09-27 07:51:00   2011-09-30 17:30:00              76480.
#>  3 96001… 1647184… 2011-09-27 05:30:00   2011-09-30 18:20:00              90495.
#>  4 78560… 1955538… 2011-09-22 09:00:00   2011-10-04 17:05:00             128587.
#>  5 24244… 1536456… 2011-10-04 11:09:00   2011-10-07 15:13:00              80185.
#>  6 45372… 1900289… 2011-10-04 06:33:00   2011-10-07 17:15:00              73753.
#>  7 68583… 1223451… 2011-10-04 05:00:00   2011-10-07 17:15:00              86199.
#>  8 76901… 1228116… 2011-10-04 08:25:00   2011-10-07 17:20:00              70719.
#>  9 84765… 1344408… 2011-10-13 05:25:00   2011-10-18 20:21:00              82033.
#> 10 13510… 1879596… 2011-10-18 05:00:00   2011-10-21 16:18:00              97454.
#> # ℹ 437 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Laparoscopic Cholecystectomy`
#> # A tibble: 2,456 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 44174… 3094545… 2010-01-05 05:00:00   2010-01-05 00:00:00              13263.
#>  2 42989… 3855977… 2010-01-12 05:00:00   2010-01-13 00:00:00              13717.
#>  3 80834… 3647383… 2010-01-19 05:00:00   2010-01-20 00:00:00              13915.
#>  4 24888… 3164824… 2010-01-28 05:00:00   2010-01-29 00:00:00              12403.
#>  5 25026… 3107332… 2010-01-28 05:00:00   2010-01-29 00:00:00              13995.
#>  6 43792… 3606774… 2010-01-29 05:00:00   2010-01-30 00:00:00              13725.
#>  7 31761… 3103216… 2010-02-02 05:00:00   2010-02-02 00:00:00              13763.
#>  8 12868… 3439301… 2010-02-04 05:00:00   2010-02-05 00:00:00              13602.
#>  9 86010… 3136153… 2010-02-05 05:00:00   2010-02-06 00:00:00              15136.
#> 10 40209… 3923213… 2010-02-11 05:00:00   2010-02-11 00:00:00              12798.
#> # ℹ 2,446 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $MI
#> # A tibble: 2,253 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 13778… 1779125… 2011-09-22 15:44:00   2011-09-23 12:48:00              13819.
#>  2 75904… 1290112… 2011-09-29 11:57:00   2011-09-29 15:15:00              23384.
#>  3 56932… 1127730… 2011-09-29 21:58:00   2011-09-30 16:37:00              11806.
#>  4 33544… 1537839… 2011-09-18 13:21:00   2011-10-04 15:52:00             165563.
#>  5 80141… 1426155… 2011-09-18 15:50:00   2011-10-06 15:25:00             137769.
#>  6 44194… 1588054… 2011-09-30 09:14:00   2011-10-07 09:00:00              64195 
#>  7 15917… 1023432… 2011-10-07 04:19:00   2011-10-08 16:30:00              29082.
#>  8 45753… 1636854… 2011-10-02 02:37:00   2011-10-11 14:40:00              89552.
#>  9 15982… 1131846… 2011-10-09 07:14:00   2011-10-16 15:50:00              46561.
#> 10 75745… 1816201… 2011-10-16 16:39:00   2011-10-17 19:56:00              10487.
#> # ℹ 2,243 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Major Depression/Bipolar Affective Disorders`
#> # A tibble: 2,866 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 83028… 1773215… 2011-09-19 16:52:00   2011-09-22 13:18:00              12668.
#>  2 52413… 1344738… 2011-09-17 15:46:00   2011-09-28 15:13:00              41043.
#>  3 76863… 1963121… 2011-09-20 20:26:00   2011-09-28 17:11:00              30736.
#>  4 31780… 1094724… 2011-09-26 00:15:00   2011-09-30 13:55:00              17790.
#>  5 51449… 1225742… 2011-09-28 22:44:00   2011-10-03 14:35:00              19806.
#>  6 16347… 1925531… 2011-09-30 16:55:00   2011-10-03 15:06:00              13748.
#>  7 95753… 1886595… 2011-09-24 20:06:00   2011-10-03 15:07:00              34606.
#>  8 52126… 1594861… 2011-09-22 19:56:00   2011-10-03 15:21:00              40757.
#>  9 75670… 1235696… 2011-09-27 16:18:00   2011-10-03 15:26:00              24597.
#> 10 42724… 1433227… 2011-09-19 10:39:00   2011-10-04 12:40:00              55108.
#> # ℹ 2,856 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Mastectomy
#> # A tibble: 58 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 80731… 1129236… 2011-11-07 04:43:00   2011-11-09 13:05:00              33908.
#>  2 37638… 1018182… 2011-11-14 06:43:00   2011-11-15 14:20:00              28817.
#>  3 57062… 1503680… 2011-11-28 04:49:00   2011-11-29 15:00:00              29532.
#>  4 57598… 1330344… 2011-12-12 09:22:00   2011-12-13 18:00:00              22163.
#>  5 67748… 1066637… 2011-12-12 05:32:00   2011-12-14 14:00:00              36931.
#>  6 81680… 1751336… 2012-01-16 06:00:00   2012-01-16 11:25:00              19637.
#>  7 26174… 1748684… 2012-01-30 05:15:00   2012-01-31 18:30:00              25352.
#>  8 59559… 1265426… 2012-02-06 05:00:00   2012-02-07 16:51:00              58899.
#>  9 79465… 1419822… 2012-04-02 05:45:00   2012-04-03 13:50:00             104772.
#> 10 27380… 1719685… 2012-04-16 05:57:00   2012-04-17 14:35:00              86889.
#> # ℹ 48 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Medical
#> # A tibble: 64,435 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 15915… 1588023… 2011-09-11 19:55:00   2011-09-13 16:01:00              23983.
#>  2 24790… 1351333… 2011-09-18 22:00:00   2011-09-21 14:10:00              29385.
#>  3 40945… 1350530… 2011-09-20 22:23:00   2011-09-21 16:15:00              12161.
#>  4 67359… 1906290… 2011-09-19 23:09:00   2011-09-21 17:27:00              16550.
#>  5 36661… 1162679… 2011-09-21 03:14:00   2011-09-21 17:30:00              17444.
#>  6 59649… 1179016… 2011-09-20 22:57:00   2011-09-21 22:30:00              27419 
#>  7 65736… 1669246… 2011-09-21 09:45:00   2011-09-22 11:35:00              38719.
#>  8 27099… 1828863… 2011-09-21 20:19:00   2011-09-22 11:35:00               9612.
#>  9 78487… 1754195… 2011-09-20 15:47:00   2011-09-22 12:30:00              21077.
#> 10 17896… 1716209… 2011-09-21 15:44:00   2011-09-22 12:42:00              11714.
#> # ℹ 64,425 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $PTCA
#> # A tibble: 1,343 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 65802… 1922614… 2013-12-16 18:42:00   2013-12-18 15:54:00              95300.
#>  2 47960… 1712037… 2013-12-17 09:24:00   2013-12-20 11:44:00             126548.
#>  3 88081… 1416143… 2013-12-16 15:57:00   2013-12-21 18:29:00             165931.
#>  4 47327… 1242762… 2013-12-20 23:49:00   2013-12-24 16:27:00              72849.
#>  5 68288… 1687746… 2013-12-08 13:12:00   2013-12-24 18:40:00             277715.
#>  6 65853… 1602546… 2013-12-24 12:16:00   2013-12-27 17:21:00             129947.
#>  7 22470… 1919563… 2014-01-02 22:02:00   2014-01-05 12:45:00             122591.
#>  8 70383… 1542050… 2014-01-02 17:37:00   2014-01-08 15:18:00             155744.
#>  9 30699… 1996197… 2014-01-07 15:17:00   2014-01-14 15:51:00             280973.
#> 10 15526… 1350260… 2014-01-11 22:29:00   2014-01-15 12:13:00             100158.
#> # ℹ 1,333 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`PTCA Outpatient`
#> # A tibble: 369 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 40037… 3514125… 2013-12-18 05:00:00   2013-12-19 00:00:00             107785.
#>  2 65938… 3525540… 2013-12-19 05:00:00   2013-12-20 00:00:00              99169.
#>  3 64137… 3456566… 2013-12-19 05:00:00   2013-12-20 00:00:00             104680.
#>  4 32157… 3927503… 2013-12-20 05:00:00   2013-12-20 00:00:00              85622.
#>  5 97406… 3404098… 2014-01-08 05:00:00   2014-01-08 00:00:00              54834.
#>  6 52799… 3644914… 2014-01-09 05:00:00   2014-01-10 00:00:00              38735.
#>  7 31448… 3922625… 2014-01-28 05:00:00   2014-01-28 00:00:00              73034.
#>  8 69180… 3654599… 2014-01-29 05:00:00   2014-01-30 00:00:00              69213.
#>  9 79461… 3382468… 2014-01-30 05:00:00   2014-01-31 00:00:00              73589.
#> 10 35852… 3344079… 2014-02-18 05:00:00   2014-02-18 00:00:00              73071.
#> # ℹ 359 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Pneumonia
#> # A tibble: 3,323 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 41168… 1339933… 2011-09-19 00:15:00   2011-09-20 18:15:00              13585.
#>  2 87344… 1581344… 2011-09-21 09:39:00   2011-09-24 18:21:00              38143.
#>  3 64810… 1511895… 2011-09-23 20:30:00   2011-09-26 12:45:00              23686.
#>  4 23339… 1813741… 2011-09-23 15:18:00   2011-09-26 13:23:00              26700.
#>  5 43861… 1935030… 2011-09-22 14:03:00   2011-09-26 20:03:00              32690.
#>  6 35356… 1551465… 2011-09-21 20:50:00   2011-09-27 16:34:00              45089.
#>  7 49815… 1349707… 2011-09-23 02:48:00   2011-09-30 18:10:00              50528.
#>  8 77558… 1443601… 2011-09-30 15:42:00   2011-10-01 11:28:00              12873.
#>  9 72612… 1249400… 2011-09-30 19:05:00   2011-10-03 18:47:00              18688.
#> 10 36317… 1539295… 2011-10-01 10:44:00   2011-10-04 13:59:00              24887.
#> # ℹ 3,313 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Schizophrenia
#> # A tibble: 1,681 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 78864… 1578423… 2011-09-10 10:37:00   2011-09-16 14:08:00              25230.
#>  2 90780… 1948172… 2011-09-12 20:18:00   2011-09-22 13:19:00              37537.
#>  3 24439… 1541902… 2011-09-17 20:52:00   2011-09-23 14:34:00              24523.
#>  4 17550… 1239946… 2011-09-15 19:56:00   2011-10-04 10:50:00              70223.
#>  5 90449… 1067597… 2011-09-26 10:16:00   2011-10-06 15:08:00              40697.
#>  6 21252… 1222746… 2011-10-05 10:32:00   2011-10-11 14:28:00              25232.
#>  7 47554… 1507806… 2011-09-24 19:18:00   2011-10-12 15:41:00              62677.
#>  8 34234… 1699816… 2011-10-06 19:05:00   2011-10-13 12:57:00              34400.
#>  9 70046… 1132874… 2011-10-07 20:28:00   2011-10-14 15:26:00              26578.
#> 10 74820… 1025221… 2011-10-11 20:57:00   2011-10-19 13:46:00              30806.
#> # ℹ 1,671 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Surgical
#> # A tibble: 14,916 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 58762… 1957238… 2011-09-19 20:12:00   2011-09-21 11:17:00              33911.
#>  2 66908… 1572987… 2011-09-20 09:09:00   2011-09-22 10:27:00              16933.
#>  3 55391… 1020021… 2011-09-21 23:20:00   2011-09-22 15:44:00              13050.
#>  4 28900… 1121152… 2011-09-20 05:00:00   2011-09-22 17:51:00              24769.
#>  5 46455… 1937756… 2011-09-22 20:09:00   2011-09-23 18:07:00              32807.
#>  6 86883… 1781570… 2011-09-23 05:00:00   2011-09-24 11:44:00              20214.
#>  7 74525… 1390406… 2011-09-25 07:23:00   2011-09-26 17:45:00              37743.
#>  8 91913… 1827659… 2011-09-23 13:36:00   2011-09-27 11:47:00              53769.
#>  9 95797… 1767983… 2011-09-11 20:12:00   2011-09-27 15:47:00             161126.
#> 10 68422… 1124488… 2011-09-26 07:18:00   2011-09-28 14:42:00             105689.
#> # ℹ 14,906 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $Syncope
#> # A tibble: 1,854 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 23076… 1461398… 2011-09-20 01:18:00   2011-09-20 10:38:00              13422.
#>  2 84855… 1170655… 2011-09-19 18:23:00   2011-09-21 15:51:00              21865.
#>  3 47360… 1041458… 2011-09-20 23:24:00   2011-09-22 13:34:00              17635 
#>  4 29909… 1798395… 2011-09-22 15:38:00   2011-09-25 15:12:00              33088.
#>  5 86969… 1347663… 2011-09-25 15:28:00   2011-09-26 16:43:00              14623.
#>  6 16815… 1367750… 2011-09-23 17:02:00   2011-09-26 18:51:00              27595.
#>  7 74671… 1467173… 2011-09-25 02:07:00   2011-09-26 22:00:00              34697.
#>  8 58540… 1224109… 2011-09-22 23:12:00   2011-09-27 18:47:00              29705.
#>  9 44743… 1018572… 2011-09-27 18:07:00   2011-09-28 18:40:00              16385.
#> 10 69830… 1136058… 2011-09-27 10:55:00   2011-09-28 21:16:00               5867.
#> # ℹ 1,844 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $TIA
#> # A tibble: 1,490 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 11900… 1817373… 2011-09-26 20:51:00   2011-09-27 14:07:00              15764.
#>  2 40157… 1365680… 2011-09-26 22:49:00   2011-09-29 18:35:00              27759.
#>  3 81648… 1909292… 2011-10-08 15:57:00   2011-10-11 16:46:00              44648.
#>  4 35853… 1358966… 2011-10-06 19:41:00   2011-10-13 11:24:00              66200.
#>  5 35309… 1512759… 2011-10-10 17:33:00   2011-10-13 15:46:00              32168.
#>  6 60037… 1754357… 2011-10-09 21:27:00   2011-10-13 16:03:00              37201.
#>  7 24439… 1805134… 2011-10-12 14:56:00   2011-10-17 14:21:00              56238.
#>  8 95326… 1662410… 2011-10-13 14:44:00   2011-10-20 16:27:00              85644.
#>  9 97257… 1096965… 2011-10-19 23:02:00   2011-10-23 14:11:00              37159.
#> 10 19835… 1741208… 2011-10-16 19:00:00   2011-10-24 16:56:00              65126.
#> # ℹ 1,480 more rows
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Vaginal Delivery`
#> # A tibble: 11 × 17
#>    mrn    visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>    <chr>  <chr>    <dttm>                <dttm>                            <dbl>
#>  1 97356… 1473544… 2011-12-14 15:30:00   2011-12-14 17:37:00               5707 
#>  2 54205… 1412111… 2012-01-31 07:10:00   2012-01-31 09:00:00               7885.
#>  3 78836… 1257448… 2012-03-05 13:51:00   2012-03-05 16:40:00              11019.
#>  4 51141… 1989466… 2012-06-12 12:02:00   2012-06-12 13:17:00               7702 
#>  5 63911… 1743794… 2012-07-06 08:59:00   2012-07-06 11:07:00               7386.
#>  6 16156… 1997707… 2012-07-13 09:15:00   2012-07-13 11:59:00              15250.
#>  7 25535… 1161988… 2012-09-22 18:35:00   2012-09-22 22:00:00               8301.
#>  8 88987… 1605284… 2012-11-04 00:50:00   2012-11-11 14:14:00              46743.
#>  9 88866… 1936904… 2012-12-14 07:10:00   2012-12-15 20:37:00              12017.
#> 10 93192… 1506189… 2013-03-06 12:01:00   2013-03-06 13:31:00               8192.
#> 11 79312… 1724518… 2013-09-08 22:32:00   2013-09-08 23:21:00               6599.
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
#> $`Valve Procedure`
#> # A tibble: 8 × 17
#>   mrn     visit_id visit_start_date_time visit_end_date_time total_charge_amount
#>   <chr>   <chr>    <dttm>                <dttm>                            <dbl>
#> 1 698201… 1919502… 2014-10-01 02:44:00   2014-10-01 10:30:00              61967.
#> 2 825784… 1181134… 2014-11-19 15:29:00   2014-11-20 22:42:00              95348.
#> 3 697728… 1925626… 2015-06-12 22:07:00   2015-06-13 02:00:00             210729.
#> 4 497633… 1911039… 2016-01-12 02:12:00   2016-01-13 12:25:00             209630.
#> 5 586969… 1572755… 2016-01-12 09:06:00   2016-01-14 14:55:00             244695.
#> 6 729922… 1565557… 2016-04-06 10:38:00   2016-04-15 16:33:00             215360.
#> 7 507774… 1715639… 2018-06-05 00:37:00   2018-06-05 13:58:00             295358.
#> 8 444943… 1920905… 2020-07-13 10:21:00   2020-07-13 15:00:00             341798.
#> # ℹ 12 more variables: total_amount_due <dbl>, total_adjustment_amount <dbl>,
#> #   payer_grouping <chr>, total_payment_amount <dbl>, ip_op_flag <chr>,
#> #   service_line <chr>, length_of_stay <dbl>, expected_length_of_stay <lgl>,
#> #   length_of_stay_threshold <lgl>, los_outlier_flag <dbl>, readmit_flag <dbl>,
#> #   readmit_expectation <lgl>
#> 
```
