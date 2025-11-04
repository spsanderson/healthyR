# Changelog

All notable changes to healthyR are documented here.

## Current Version: 0.2.2

For the most up-to-date changelog, see [NEWS.md](https://github.com/spsanderson/healthyR/blob/master/NEWS.md) in the repository.

---

## Version History

### healthyR 0.2.2

**Release Date:** Latest stable release

#### Breaking Changes
- Fix #168 - Minor fixes to `gartner_magic_chart_plt()` parameter names

#### Minor Fixes and Improvements
- Fix #153 - Additions to check logic by @alcrosby for `save_to_excel()`
- Fix #168 - Minor fixes to `gartner_magic_chart_plt()`
- Fix #167 - Fix `<scale>` argument of `<guides>`
- Fix #169 - Break out data table functions into their own files

---

### healthyR 0.2.1

#### Breaking Changes
- Fix #141 - Drop support for kmeans functions and umap functions as they were moved to `healthyR.ai`
- Fix #152 - Updated Depends to R >= 3.3

---

### healthyR 0.2.0

#### New Features
- Fix #139 - Add functions `color_blind()`, `hr_scale_fill_colorblind()` and `hr_scale_color_colorblind()`
- Fix #138 - Add parameter to `gartner_magic_chart_plt()` function in order to size points according to a column in a data.frame/tibble

---

### healthyR 0.1.9

#### Minor Fixes and Improvements
- Fix #129 - Default to `ggplot2::theme_minimal()` for all plots

---

### healthyR 0.1.8

#### Minor Fixes and Improvements
- Fix #116 - Drop need for `cli`, `crayon`, and `rstudioapi`
- Fix #105 - Correct code and make minor updates to `ts_census_los_daily_tbl()`

---

### healthyR 0.1.7

#### New Features
- Fix #107 - Add `service_line_vec()`
- Fix #108 - Add `service_line_augment()`

**Impact:** Major enhancement for service line classification workflows. These functions enable automated classification of patients into clinical service lines based on ICD-10 codes and DRGs.

---

### healthyR 0.1.6

#### New Features
- Fix #92 - Add `diverging_lollipop_plt()`
- Fix #93 - Add `diverging_bar_plt()`

#### Minor Fixes and Improvements
- Fix #90 - Add startup message at library attachment

---

### healthyR 0.1.5

**Major Update:** Time series signature functionality

#### Breaking Changes
- Fix #70 - `ts_ymwdh_tbl()` renamed to `ts_signature_tbl()`

#### New Features
- Fix #70 - Add `timetk::tk_augment_time_series_signature()` to tibble that gets returned
- Fix #59 - Add boolean to `ts_signature_tbl()` for `timetk::pad_by_time()` functionality
- Fix #55 - Add `uwot::umap()` functionality with `umap_list()`
- Fix #56 - Add `umap_plt()` functionality to plot out the UMAP Projection with K-Means cluster assignments

#### Minor Fixes and Improvements
- Fix #76 - Require user to select a column to be aggregated and normalized in the `kmeans_user_item_tbl()` function
- Fix #62 - Add `lifecycle` to project

---

### healthyR 0.1.4

#### New Features

**K-Means Analysis Suite:**
- `kmeans_tidy_tbl()` - Fix #40: A broom style function to get `tidy`, `augment` and `glance` of the `kmeans_obj()` output
- `kmeans_scree_data_tbl()` - Fix #41: Creates a tibble with the data that underlies the `kmeans_scree_plot()`
- `kmeans_scree_plt()` - Fix #42: A `ggplot2` plot of the Scree data to help find the optimal elbow (elbow plot)
- `kmeans_mapped_tbl()` - Fix #43: Generates the data that will help create the elbow plot data. Maps the `kmeans_obj()` function across the stated amount of centers (default: 15)

#### Minor Fixes and Improvements
- Fix #45 - Drop the `.row_col` parameter from the `kmeans_tidy_tbl()` function as it is not needed

---

### healthyR 0.1.3

#### New Features
- `kmeans_user_item_tbl()` - Fix #37: Creates a user item tibble from preprocessed data
- `kmeans_obj()` - Fix #37: Creates a stats kmeans object, a simple wrapper

#### Minor Fixes and Improvements
- `ts_census_los_daily_tbl()` - Fix #37: Correctly arranges by date and gets correct counts by date column

---

### healthyR 0.1.2

#### New Features
- `ts_census_los_daily_tbl()` - Can get the census or length of stay given just two columns of data: admit/arrival date and discharge/departure date

---

### healthyR 0.1.1

#### New Features
- `category_counts_tbl()` - Get counts of a specified column with a user specified grouping if provided
- `named_item_list()` - Good for saving a tibble with many groups to different sheets of an excel file
- `top_n_tbl()` - Good for getting a tibble of n records by some column selection
- `ts_census_los_daily_tbl()` - Can compute the ALOS and census for data given simply the Admit Date and Discharge Date (discharge date can be null). Sometimes you want to see the average length of stay for those patients that are currently admitted

---

### healthyR 0.1.0

**First CRAN Release - December 3, 2020**

Initial public release of healthyR with core functionality for hospital data analysis.

#### Core Features
- Time series analysis functions
- Basic plotting capabilities
- Data transformation utilities
- SQL-style string functions

---

### healthyR 0.0.0.9006 (Development)

#### New Features
- Add data files `dx_cc_mapping` and `px_cc_mapping`
- Added functions:
  1. `save_to_excel()`
  2. `los_ra_index_summary_tbl()`
  3. `los_ra_index_plt()`

---

### healthyR 0.0.0.9005 (Development)

#### New Features
- Add parameters to gartner magic chart for interior labels

---

### healthyR 0.0.0.9004 (Development)

#### New Features
- Update sql substring-like functions

---

### healthyR 0.0.0.9003 (Development)

#### New Features
- Added `plt_gartner_magic_chart()`

---

### healthyR 0.0.0.9002 (Development)

#### New Features
- Added the function `opt_bin()`

---

### healthyR 0.0.0.9001 (Development)

#### New Features
- Added the function `ts_ymwdh_tbl()`

---

### healthyR 0.0.0.9000 (Development)

**Initial Development Release**

- Project initialization
- Basic structure established
- Added a NEWS.md file to track changes to the package

---

## Upgrade Notes

### Upgrading to 0.2.x from 0.1.x

**Minor breaking changes:**
- K-means and UMAP functions moved to separate `healthyR.ai` package
- If you use these functions, install `healthyR.ai`:
```r
install.packages("healthyR.ai")
```

**New features to explore:**
- Color-blind friendly palettes
- Enhanced Gartner magic chart

### Upgrading to 0.1.5+ from 0.1.4 or earlier

**Breaking change:**
- `ts_ymwdh_tbl()` renamed to `ts_signature_tbl()`

**Migration:**
```r
# Old code
result <- ts_ymwdh_tbl(.data = data, .date_col = date)

# New code
result <- ts_signature_tbl(.data = data, .date_col = date)
```

---

## Related Packages

healthyR is part of the healthy-verse of packages:

- **[healthyR](https://github.com/spsanderson/healthyR)** - Hospital data analysis workflows (this package)
- **[healthyR.data](https://github.com/spsanderson/healthyR.data)** - Sample healthcare datasets
- **[healthyR.ai](https://github.com/spsanderson/healthyR.ai)** - Machine learning for healthcare data
- **[healthyR.ts](https://github.com/spsanderson/healthyR.ts)** - Time series forecasting for healthcare

---

## Contributing to Changelog

When contributing to healthyR, please update NEWS.md:

```markdown
# healthyR (development version)

## New Features
- Description of new feature (#issue_number)

## Bug Fixes  
- Description of bug fix (#issue_number)

## Breaking Changes
- Description of breaking change (#issue_number)
```

See [Contributing Guidelines](Contributing) for more details.

---

## Version Numbering

healthyR follows [Semantic Versioning](https://semver.org/):

- **Major version (X.0.0)**: Breaking changes
- **Minor version (0.X.0)**: New features, backwards compatible
- **Patch version (0.0.X)**: Bug fixes, backwards compatible

Development versions use `.9000` suffix (e.g., 0.2.2.9000)

---

## Release Schedule

- **Patch releases**: As needed for critical bug fixes
- **Minor releases**: Quarterly or when significant features are ready
- **Major releases**: When breaking changes are necessary

---

## Historical Notes

### Project Milestones

- **Dec 3, 2020** - First CRAN release (v0.1.0)
- **2021** - Added service line classification capabilities
- **2022** - Enhanced visualization features
- **2023** - Added accessibility features (color-blind palettes)
- **2024** - Continued refinement and community contributions

### Popular Features by Version

- **v0.1.0**: Core time series functions
- **v0.1.1**: Excel export utilities
- **v0.1.5**: Enhanced time series signatures
- **v0.1.7**: Service line classification
- **v0.2.0**: Accessibility features

---

## Links

- **Latest Release Notes:** [NEWS.md](https://github.com/spsanderson/healthyR/blob/master/NEWS.md)
- **All Releases:** [GitHub Releases](https://github.com/spsanderson/healthyR/releases)
- **CRAN:** [healthyR on CRAN](https://cran.r-project.org/package=healthyR)
- **Development:** [GitHub Repository](https://github.com/spsanderson/healthyR)

---

## Navigation

- [← Troubleshooting](Troubleshooting)
- [Home](Home)
- [Contributing →](Contributing)

---

*For the most current changelog, always check [NEWS.md](https://github.com/spsanderson/healthyR/blob/master/NEWS.md) in the repository.*
