# Changelog

## healthyR (development version)

## healthyR 0.2.2

CRAN release: 2024-07-01

### Breaking Changes

1.  Fix [\#168](https://github.com/spsanderson/healthyR/issues/168) -
    Minor fixes to
    [`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md)
    parameter names.

### New Features

None

### Minor Fixes and Improvements

1.  Fix [\#153](https://github.com/spsanderson/healthyR/issues/153) -
    Additions to check logic by [@alcrosby](https://github.com/alcrosby)
    for
    [`save_to_excel()`](https://www.spsanderson.com/healthyR/reference/save_to_excel.md)
2.  Fix [\#168](https://github.com/spsanderson/healthyR/issues/168) -
    Minor fixes to
    [`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md)
3.  Fix [\#167](https://github.com/spsanderson/healthyR/issues/167) -
    Fix `<scale>` argument of `<guides>`
4.  Fix [\#169](https://github.com/spsanderson/healthyR/issues/169) -
    Break out data table functions into their own files.

## healthyR 0.2.1

CRAN release: 2023-04-06

### Breaking Changes

1.  Fix [\#141](https://github.com/spsanderson/healthyR/issues/141) -
    Drop support for kmeans functions and umap functions as they were
    moved to `healthyR.ai`
2.  Fix [\#152](https://github.com/spsanderson/healthyR/issues/152) -
    Updated Depends to R \>= 3.3

### New Features

None

### Minor Fixes and Improvements

None

## healthyR 0.2.0

CRAN release: 2022-07-18

### Breaking Changes

None

### New Features

1.  Fix [\#139](https://github.com/spsanderson/healthyR/issues/139) -
    Add functions
    [`color_blind()`](https://www.spsanderson.com/healthyR/reference/color_blind.md)
    [`hr_scale_fill_colorblind()`](https://www.spsanderson.com/healthyR/reference/hr_scale_fill_colorblind.md)
    and
    [`hr_scale_color_colorblind()`](https://www.spsanderson.com/healthyR/reference/hr_scale_color_colorblind.md)
2.  Fix [\#138](https://github.com/spsanderson/healthyR/issues/138) -
    Add parameter to
    [`gartner_magic_chart_plt()`](https://www.spsanderson.com/healthyR/reference/gartner_magic_chart_plt.md)
    function in order to size points accoring to a column in a
    data.frame/tibble.

### Minor Fixes and Improvements

None

## healthyR 0.1.9

CRAN release: 2022-04-25

### Breaking Changes

None

### New Features

None

### Minor Fixes and Improvements

1.  Fix [\#129](https://github.com/spsanderson/healthyR/issues/129) -
    Default to
    [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
    for all plots.

## healthyR 0.1.8

CRAN release: 2022-02-27

### Breaking Changes

None

### New Features

None

### Minor Fixes and Improvements

1.  Fix [\#116](https://github.com/spsanderson/healthyR/issues/116) -
    Drop need for `cli`, `crayon`, and `rstudioapi`
2.  F-x [\#105](https://github.com/spsanderson/healthyR/issues/105) -
    correct code and make minor updates to
    [`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md)

## healthyR 0.1.7

CRAN release: 2022-01-05

### Breaking Changes

None

### New Features

1.  Fix [\#107](https://github.com/spsanderson/healthyR/issues/107) -
    Add
    [`service_line_vec()`](https://www.spsanderson.com/healthyR/reference/service_line_vec.md)
2.  Fix [\#108](https://github.com/spsanderson/healthyR/issues/108) -
    Add
    [`service_line_augment()`](https://www.spsanderson.com/healthyR/reference/service_line_augment.md)

### Minor Fixes and Improvements

None

## healthyR 0.1.6

CRAN release: 2021-08-20

### Breaking Changes

None

### New Features

1.  Fix [\#92](https://github.com/spsanderson/healthyR/issues/92) - Add
    `diverging_lollipop_plt`
2.  Fix [\#93](https://github.com/spsanderson/healthyR/issues/93) - Add
    `diverging_bar_plt`

### Minor Fixes and Improvements

1.  Fix [\#90](https://github.com/spsanderson/healthyR/issues/90) - Add
    startup message at library attachment.

## healthyR 0.1.5

CRAN release: 2021-06-22

### Breaking Changes

1.  Fix [\#70](https://github.com/spsanderson/healthyR/issues/70) -
    `ts_ymwdh_tbl()` renamed to
    [`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)

### New Features

1.  Fix [\#70](https://github.com/spsanderson/healthyR/issues/70) - Add
    `timetk::tk_augment_time_series_signature()` to tibble that gets
    returned.
2.  Fix [\#59](https://github.com/spsanderson/healthyR/issues/59) - Add
    boolean to
    [`ts_signature_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_signature_tbl.md)
    for
    [`timetk::pad_by_time()`](https://business-science.github.io/timetk/reference/pad_by_time.html)
    functionality.
3.  Fix [\#55](https://github.com/spsanderson/healthyR/issues/55) - Add
    `uwot::umap()` functionality with `umap_list()`.
4.  Fix [\#56](https://github.com/spsanderson/healthyR/issues/56) - Add
    `umap_plt()` functionality to plot out the UMAP Projection with
    K-Means cluster assignments.

### Minor Fixes and Improvments

1.  Fix [\#76](https://github.com/spsanderson/healthyR/issues/76) -
    Require user to select a column to be aggregated and normalized in
    the `kmeans_user_item_tbl()` function.
2.  Fix [\#62](https://github.com/spsanderson/healthyR/issues/62) - Add
    `lifecycle` to project.

## healthyR 0.1.4

CRAN release: 2021-06-13

### Breaking Changes

None

### New Features

- Added Functions

1.  `kmeans_tidy_tbl()` Fix
    [\#40](https://github.com/spsanderson/healthyR/issues/40) - A broom
    style function to get `tidy`, `augment` and `glance` of the
    `kmeans_obj()` output
2.  `kmeans_scree_data_tbl()` Fix
    [\#41](https://github.com/spsanderson/healthyR/issues/41) - Creates
    a tibble with the data that underlies the `kmeans_scree_plot()`
3.  `kmeans_scree_plt()` Fix
    [\#42](https://github.com/spsanderson/healthyR/issues/42) - A
    `ggplot2` plot of the Scree data to help find the optimal elbow.
    Otherwise known as the elbow plot.
4.  `kmeans_mapped_tbl()` - Fix
    [\#43](https://github.com/spsanderson/healthyR/issues/43) - This
    generates the data that will help create the elbow ploat data. It
    maps the `kmeans_obj()` function across the stated amount of
    centers. The default is set to 15.

### Minor Fixes and Improvments

Fix [\#45](https://github.com/spsanderson/healthyR/issues/45) - drop the
`.row_col` parameter from the `kmeans_tidy_tbl()` function as it is not
needed, this saves the user from needing to remember what they selected
as the `user` column.

## healthyR 0.1.3

CRAN release: 2021-06-03

### Breaking Changes

None

### New Features

- Added functions

1.  `kmeans_user_item_tbl()` Fix
    [\#37](https://github.com/spsanderson/healthyR/issues/37) - Creates
    a user item tibble from preprocessed data.
2.  `kmeans_obj()` Fix
    [\#37](https://github.com/spsanderson/healthyR/issues/37) - Creates
    a stats kmeans object, a simple wrapper.

### Minor Fixes and Improvments

- Minor fixes and improvements

1.  [`ts_census_los_daily_tbl()`](https://www.spsanderson.com/healthyR/reference/ts_census_los_daily_tbl.md)
    Fix [\#37](https://github.com/spsanderson/healthyR/issues/37) -
    Correctly arranges by date and gets correct counts by date column.

### healthyR 0.1.2

- Add function

1.  ts_census_los_daily_tbl - Can get the census or length of stay given
    just two columns of data, admit/arrival date and discharge/departure
    date

### healthyR 0.1.1

- Add function(s)

1.  category_counts_tbl - Get counts of a specified column with a user
    specified grouping if provided
2.  named_item_list - Good for saving a tibble with many groups to
    different sheets of an excel file
3.  top_n_tbl - Good for getting a tibble of n records by some column
    selection
4.  ts_census_los_daily_tbl - Can compute the alos and census for data
    given simply the Admit Date and Discharge Date (discharge date can
    be null). Sometimes you want to see the average length of stay for
    those patients that are currently admitted

### healthyR 0.1.0

- Released to CRAN 12-03-2020

### healthyR 0.0.0.9006

- Add data files dx_cc_mapping and px_cc_mapping
- Added fucntion(s)

1.  save_to_excel()
2.  los_ra_index_summary_tbl
3.  los_ra_index_plt

### healthyR 0.0.0.9005

- Add parameters to gartner magic chart for interior lables

### healthyR 0.0.0.9004

- Update sql substring ike functions

### healthyR 0.0.0.9003

- Added plt_gartner_magic_chart()

### healthyR 0.0.0.9002

- Added the function opt_bin()

### healthyR 0.0.0.9001

- Added the function ts_ymwdh_tbl()

### healthyR 0.0.0.9000

- Added a NEWS.md file to track changes to the package.
