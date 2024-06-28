# healthyR 0.2.2

## Breaking Changes
1. Fix #168 - Minor fixes to `gartner_magic_chart_plt()` parameter names.

## New Features
None

## Minor Fixes and Improvements
1. Fix #153 - Additions to check logic by @alcrosby for `save_to_excel()`
2. Fix #168 - Minor fixes to `gartner_magic_chart_plt()`
3. Fix #167 - Fix `<scale>` argument of `<guides>`
4. Fix #169 - Break out data table functions into their own files.

# healthyR 0.2.1

## Breaking Changes
1. Fix #141 - Drop support for kmeans functions and umap functions as they were
moved to `healthyR.ai`
2. Fix #152 - Updated Depends to R >= 3.3

## New Features
None

## Minor Fixes and Improvements
None

# healthyR 0.2.0

## Breaking Changes
None

## New Features
1. Fix #139 - Add functions `color_blind()` `hr_scale_fill_colorblind()` 
and `hr_scale_color_colorblind()`
2. Fix #138 - Add parameter to `gartner_magic_chart_plt()` function in order to 
size points accoring to a column in a data.frame/tibble.

## Minor Fixes and Improvements
None

# healthyR 0.1.9

## Breaking Changes
None

## New Features
None

## Minor Fixes and Improvements
1. Fix #129 - Default to `ggplot2::theme_minimal()` for all plots.

# healthyR 0.1.8

## Breaking Changes
None

## New Features
None

## Minor Fixes and Improvements
1. Fix #116 - Drop need for `cli`, `crayon`, and `rstudioapi`
2. F-x #105 - correct code and make minor updates to `ts_census_los_daily_tbl()`

# healthyR 0.1.7

## Breaking Changes
None

## New Features
1. Fix #107 - Add `service_line_vec()`
2. Fix #108 - Add `service_line_augment()`

## Minor Fixes and Improvements
None

# healthyR 0.1.6

## Breaking Changes
None

## New Features
1. Fix #92 - Add `diverging_lollipop_plt`
2. Fix #93 - Add `diverging_bar_plt`

## Minor Fixes and Improvements
1. Fix #90 - Add startup message at library attachment.

# healthyR 0.1.5

## Breaking Changes
1. Fix #70 - `ts_ymwdh_tbl()` renamed to `ts_signature_tbl()`

## New Features
1. Fix #70 - Add `timetk::tk_augment_time_series_signature()` to tibble that gets
returned.
2. Fix #59 - Add boolean to `ts_signature_tbl()` for `timetk::pad_by_time()` functionality.
3. Fix #55 - Add `uwot::umap()` functionality with `umap_list()`.
4. Fix #56 - Add `umap_plt()` functionality to plot out the UMAP Projection with K-Means cluster
assignments.

## Minor Fixes and Improvments
1. Fix #76 - Require user to select a column to be aggregated and normalized in the
`kmeans_user_item_tbl()` function.
2. Fix #62 - Add `lifecycle` to project.

# healthyR 0.1.4

## Breaking Changes
None

## New Features
* Added Functions
1. `kmeans_tidy_tbl()` Fix #40 - A broom style function to get `tidy`, `augment` and `glance`
of the `kmeans_obj()` output
2. `kmeans_scree_data_tbl()` Fix #41 - Creates a tibble with the data that underlies the 
`kmeans_scree_plot()`
3. `kmeans_scree_plt()` Fix #42 - A `ggplot2` plot of the Scree data to help find
the optimal elbow. Otherwise known as the elbow plot.
4. `kmeans_mapped_tbl()` - Fix #43 - This generates the data that will help create the 
elbow ploat data. It maps the `kmeans_obj()` function across the stated amount of centers.
The default is set to 15.

## Minor Fixes and Improvments
Fix #45 - drop the `.row_col` parameter from the `kmeans_tidy_tbl()` function as
it is not needed, this saves the user from needing to remember what they selected
as the `user` column.

# healthyR 0.1.3

## Breaking Changes
None

## New Features
* Added functions
1. `kmeans_user_item_tbl()` Fix #37 - Creates a user item tibble from preprocessed data.
2. `kmeans_obj()` Fix #37 - Creates a stats kmeans object, a simple wrapper.

## Minor Fixes and Improvments
* Minor fixes and improvements
1. `ts_census_los_daily_tbl()` Fix #37 - Correctly arranges by date and gets
correct counts by date column.

## healthyR 0.1.2
* Add function
1. ts_census_los_daily_tbl - Can get the census or length of stay given just two
columns of data, admit/arrival date and discharge/departure date

## healthyR 0.1.1
* Add function(s)
1. category_counts_tbl - Get counts of a specified column with a user specified
grouping if provided
2. named_item_list - Good for saving a tibble with many groups to different sheets
of an excel file
3. top_n_tbl - Good for getting a tibble of n records by some column selection
4. ts_census_los_daily_tbl - Can compute the alos and census for data given simply
the Admit Date and Discharge Date (discharge date can be null). Sometimes you want
to see the average length of stay for those patients that are currently admitted

## healthyR 0.1.0
* Released to CRAN 12-03-2020

## healthyR 0.0.0.9006
* Add data files dx_cc_mapping and px_cc_mapping
* Added fucntion(s)
1. save_to_excel()
2. los_ra_index_summary_tbl
3. los_ra_index_plt

## healthyR 0.0.0.9005

* Add parameters to gartner magic chart for interior lables

## healthyR 0.0.0.9004

* Update sql substring ike functions

## healthyR 0.0.0.9003

* Added plt_gartner_magic_chart()

## healthyR 0.0.0.9002

* Added the function opt_bin()

## healthyR 0.0.0.9001

* Added the function ts_ymwdh_tbl()

## healthyR 0.0.0.9000

* Added a NEWS.md file to track changes to the package.
