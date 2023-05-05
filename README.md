
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthyR <img src="man/figures/logo.png" width="147" height="170" align="right" />

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/healthyR)](https://cran.r-project.org/package=healthyR)
![](https://cranlogs.r-pkg.org/badges/healthyR)
![](https://cranlogs.r-pkg.org/badges/grand-total/healthyR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html##experimental)
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](https://makeapullrequest.com)
<!-- badges: end -->

The goal of healthyR is to help quickly analyze common data problems in
the Administrative and Clincial spaces.

## Installation

You can install the released version of healthyR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("healthyR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("spsanderson/healthyR")
```

## Example

This is a basic example of using the ts_median_excess_plt() function\`:

``` r
library(healthyR)
library(timetk)
library(dplyr)

ts_signature_tbl(.data = m4_daily, .date_col = date, .pad_time = TRUE, id) %>%
    ts_median_excess_plt(
        .date_col           = date
        , .value_col        = value
        , .x_axis           = week
        , .ggplot_group_var = year
        , .years_back       = 5
    )
```

<img src="man/figures/README-example-1.png" width="100%" />

Here is a simple example of using the ts_signature_tbl() function:

``` r
library(healthyR)
library(timetk)

ts_signature_tbl(.data = m4_daily, .date_col = date)
#> # A tibble: 17,578 × 31
#>    id    date       value index.num  diff  year year.iso  half quarter month
#>    <fct> <date>     <dbl>     <dbl> <dbl> <int>    <int> <int>   <int> <int>
#>  1 D410  1978-06-23 9109. 267408000    NA  1978     1978     1       2     6
#>  2 D410  1978-06-24 9103. 267494400 86400  1978     1978     1       2     6
#>  3 D410  1978-06-25 9116. 267580800 86400  1978     1978     1       2     6
#>  4 D410  1978-06-26 9116. 267667200 86400  1978     1978     1       2     6
#>  5 D410  1978-06-27 9106. 267753600 86400  1978     1978     1       2     6
#>  6 D410  1978-06-28 9094. 267840000 86400  1978     1978     1       2     6
#>  7 D410  1978-06-29 9094. 267926400 86400  1978     1978     1       2     6
#>  8 D410  1978-06-30 9084. 268012800 86400  1978     1978     1       2     6
#>  9 D410  1978-07-01 9081. 268099200 86400  1978     1978     2       3     7
#> 10 D410  1978-07-02 9047. 268185600 86400  1978     1978     2       3     7
#> # ℹ 17,568 more rows
#> # ℹ 21 more variables: month.xts <int>, month.lbl <ord>, day <int>, hour <int>,
#> #   minute <int>, second <int>, hour12 <int>, am.pm <int>, wday <int>,
#> #   wday.xts <int>, wday.lbl <ord>, mday <int>, qday <int>, yday <int>,
#> #   mweek <int>, week <int>, week.iso <int>, week2 <int>, week3 <int>,
#> #   week4 <int>, mday7 <int>
```

Here is a simple example of using the plt_gartner_magic_chart()
function:

``` r

suppressPackageStartupMessages(library(healthyR))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))

gartner_magic_chart_plt(
  .data = tibble(x = rnorm(100, 0, 1), y = rnorm(100, 0, 1))
  , .x_col = x
  , .y_col = y
  , .y_lab = "los"
  , .x_lab = "RA"
  , .plt_title = "Test Title"
  , .tl_lbl = "Top Left lbl"
  , .tr_lbl = "Top Right lbl"
  , .bl_lbl = "Bottom Left lbl"
  , .br_lbl = "Bottom Right lbl"
)
```

<img src="man/figures/README-gartner_chart-1.png" width="100%" />
