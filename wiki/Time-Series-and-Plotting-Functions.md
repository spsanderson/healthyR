# Time Series and Plotting Functions

Comprehensive reference for all time series analysis and plotting functions in healthyR.

## Table of Contents

- [ts_signature_tbl()](#ts_signature_tbl)
- [ts_alos_plt()](#ts_alos_plt)
- [ts_readmit_rate_plt()](#ts_readmit_rate_plt)
- [ts_median_excess_plt()](#ts_median_excess_plt)
- [ts_census_los_daily_tbl()](#ts_census_los_daily_tbl)
- [ts_plt()](#ts_plt)

---

## ts_signature_tbl()

Add time-based signature features to your data for advanced temporal analysis.

### Description

Augments a data frame with time series signature features from the timetk package. This adds year, month, week, day, and other temporal components based on a date column.

### Usage

```r
ts_signature_tbl(
  .data,
  .date_col,
  .pad_time = TRUE,
  ...
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data to augment with time features |
| `.date_col` | column name (unquoted) | The date column to use for feature extraction |
| `.pad_time` | logical | If TRUE, fills in missing time periods using `timetk::pad_by_time()`. Default: TRUE |
| `...` | column names | Optional grouping variables for padding |

### Returns

A tibble with the original data plus added time signature columns.

### Added Columns

The function adds these temporal features:

- **index.num** - Numeric representation of the date
- **diff** - Difference from previous timestamp
- **year** - Year (2024)
- **year.iso** - ISO 8601 year
- **half** - Half of year (1 or 2)
- **quarter** - Quarter (1-4)
- **month** - Month number (1-12)
- **month.xts** - Month for xts compatibility (0-11)
- **month.lbl** - Month label (Jan, Feb, etc.)
- **day** - Day component
- **hour**, **minute**, **second** - Time components
- **hour12**, **am.pm** - 12-hour format
- **wday** - Day of week (1-7, Sunday = 1)
- **wday.xts** - Weekday for xts (0-6)
- **wday.lbl** - Weekday label (Sun, Mon, etc.)
- **mday** - Day of month
- **qday** - Day of quarter
- **yday** - Day of year
- **mweek** - Week of month
- **week** - Week of year (1-53)
- **week.iso** - ISO 8601 week
- **week2**, **week3**, **week4** - Bi-weekly, tri-weekly, quad-weekly
- **mday7** - Alignment marker

### Examples

#### Basic Usage

```r
library(healthyR)
library(timetk)
library(dplyr)

# Simple time signature
result <- ts_signature_tbl(
  .data = m4_daily,
  .date_col = date,
  .pad_time = FALSE
)

head(result)
```

#### With Time Padding

```r
# Fill in missing dates by group
complete_data <- hospital_data %>%
  ts_signature_tbl(
    .date_col = admission_date,
    .pad_time = TRUE,
    patient_id  # Group by patient
  )
```

#### In a Pipeline

```r
# Use in a typical workflow
analysis <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(year >= 2023) %>%
  group_by(year, month.lbl, service_line) %>%
  summarise(
    n_encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    .groups = "drop"
  )
```

### Use Cases

- Monthly/quarterly trend analysis
- Seasonal pattern detection
- Day-of-week analysis
- Year-over-year comparisons
- Time-based feature engineering for modeling

### Notes

- The function uses `timetk::tk_augment_timeseries_signature()` internally
- Padding fills gaps in the time series with NA values
- When padding with groups, each group is padded independently
- All added columns are available for subsequent analysis

### See Also

- [ts_alos_plt()](#ts_alos_plt) - Plot average length of stay
- [ts_census_los_daily_tbl()](#ts_census_los_daily_tbl) - Daily census calculations
- [Core Concepts: Time Series Analysis](Core-Concepts#time-series-analysis)

---

## ts_alos_plt()

Create time series plots of Average Length of Stay (ALOS).

### Description

Generates a time series visualization of average length of stay, aggregated by the specified time period (day, week, month, quarter, or year).

### Usage

```r
ts_alos_plt(
  .data,
  .date_col,
  .value_col,
  .by_grouping = "month",
  .interactive = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data containing dates and LOS values |
| `.date_col` | column name (unquoted) | Date column for the x-axis |
| `.value_col` | column name (unquoted) | Numeric column containing length of stay values |
| `.by_grouping` | character | Time aggregation: "day", "week", "month", "quarter", or "year" |
| `.interactive` | logical | If TRUE, returns an interactive plotly chart. Default: FALSE |

### Returns

- A `ggplot2` object (if `.interactive = FALSE`)
- A `plotly` object (if `.interactive = TRUE`)

### Examples

#### Static Plot - Monthly ALOS

```r
library(healthyR)

ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = FALSE
)
```

#### Interactive Plot - Weekly ALOS

```r
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "week",
  .interactive = TRUE
)
```

#### By Service Line

```r
# First classify service lines
data_with_sl <- service_line_augment(
  .data = hospital_data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg
)

# Plot ALOS by service line
data_with_sl %>%
  filter(service_line == "cardiac") %>%
  ts_alos_plt(
    .date_col = discharge_date,
    .value_col = los,
    .by_grouping = "month"
  )
```

#### Customize the Plot

```r
library(ggplot2)

# Create base plot and customize
base_plot <- ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = FALSE
)

# Add customization
base_plot +
  labs(
    title = "Hospital Average Length of Stay Trend",
    subtitle = "2023-2024",
    y = "Days"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

### Use Cases

- Monitor LOS trends over time
- Identify seasonal patterns
- Compare pre/post intervention periods
- Track performance against targets
- Create executive dashboards

### Notes

- The function automatically calculates the mean LOS for each time period
- Outliers can significantly affect ALOS; consider using median or filtering extreme values
- Interactive plots are useful for presentations and dashboards
- Missing values are automatically excluded

### Tips

- Use "day" for short-term detailed analysis
- Use "month" or "quarter" for long-term trends
- Use "week" to balance detail and overview
- Consider filtering data before plotting (e.g., by service line, payer, etc.)

### See Also

- [ts_readmit_rate_plt()](#ts_readmit_rate_plt) - Plot readmission rates
- [ts_plt()](#ts_plt) - General time series plotting
- [Tutorial: ALOS Analysis](Tutorial-ALOS-Analysis)

---

## ts_readmit_rate_plt()

Create time series plots of readmission rates.

### Description

Generates a time series visualization of readmission rates, aggregated by the specified time period.

### Usage

```r
ts_readmit_rate_plt(
  .data,
  .date_col,
  .value_col,
  .by_grouping = "month",
  .interactive = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data containing dates and readmission indicators |
| `.date_col` | column name (unquoted) | Date column for the x-axis |
| `.value_col` | column name (unquoted) | Binary/numeric column indicating readmission (1 = yes, 0 = no) |
| `.by_grouping` | character | Time aggregation: "day", "week", "month", "quarter", or "year" |
| `.interactive` | logical | If TRUE, returns an interactive plotly chart. Default: FALSE |

### Returns

- A `ggplot2` object (if `.interactive = FALSE`)
- A `plotly` object (if `.interactive = TRUE`)

### Examples

#### Basic Readmission Rate Plot

```r
library(healthyR)

# Assume readmit_flag is 1 for readmissions, 0 otherwise
ts_readmit_rate_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = readmit_flag,
  .by_grouping = "month",
  .interactive = FALSE
)
```

#### 30-Day Readmission Analysis

```r
# Calculate 30-day readmissions
library(dplyr)
library(lubridate)

readmit_data <- hospital_data %>%
  arrange(patient_id, discharge_date) %>%
  group_by(patient_id) %>%
  mutate(
    next_admit = lead(admission_date),
    days_to_readmit = as.numeric(next_admit - discharge_date),
    readmit_30d = ifelse(!is.na(days_to_readmit) & days_to_readmit <= 30, 1, 0)
  ) %>%
  ungroup()

# Plot 30-day readmission rate
ts_readmit_rate_plt(
  .data = readmit_data,
  .date_col = discharge_date,
  .value_col = readmit_30d,
  .by_grouping = "month"
)
```

#### Compare Service Lines

```r
# Plot readmission rates by service line
library(purrr)

service_lines <- c("cardiac", "orthopedic", "surgical")

plots <- map(service_lines, ~{
  data_filtered <- readmit_data %>% filter(service_line == .x)
  
  ts_readmit_rate_plt(
    .data = data_filtered,
    .date_col = discharge_date,
    .value_col = readmit_30d,
    .by_grouping = "month"
  ) +
    ggtitle(paste("Readmission Rate -", .x))
})

# Combine plots
library(cowplot)
plot_grid(plotlist = plots, ncol = 2)
```

### Use Cases

- Monitor readmission trends
- Track quality improvement initiatives
- Compare to national benchmarks
- Identify seasonal patterns in readmissions
- Analyze by service line or payer

### Notes

- Readmission data should be binary (0/1) or the function will calculate the mean
- The function calculates the rate as: (sum of readmits) / (total encounters)
- Rates are displayed as percentages (0-100%)
- Consider the time window for readmission (30-day, 60-day, etc.)

### Common Patterns

#### All-Cause Readmission

```r
# Any readmission within 30 days
readmit_data %>%
  mutate(any_readmit_30d = ifelse(days_to_readmit <= 30, 1, 0)) %>%
  ts_readmit_rate_plt(
    .date_col = discharge_date,
    .value_col = any_readmit_30d,
    .by_grouping = "month"
  )
```

#### Unplanned Readmission

```r
# Exclude planned readmissions
readmit_data %>%
  mutate(unplanned_readmit = ifelse(
    readmit_30d == 1 & planned_readmit_flag == 0, 1, 0
  )) %>%
  ts_readmit_rate_plt(
    .date_col = discharge_date,
    .value_col = unplanned_readmit,
    .by_grouping = "month"
  )
```

### See Also

- [ts_alos_plt()](#ts_alos_plt) - Plot average length of stay
- [los_ra_index_summary_tbl()](#los_ra_index_summary_tbl) - Combine LOS and readmission
- [Tutorial: Readmission Analysis](Tutorial-Readmission-Analysis)

---

## ts_median_excess_plt()

Visualize excess utilization patterns using median-based analysis.

### Description

Creates a plot showing deviations from the median across different time periods, useful for identifying patterns of excess or below-average utilization.

### Usage

```r
ts_median_excess_plt(
  .data,
  .date_col,
  .value_col,
  .x_axis,
  .ggplot_group_var,
  .years_back = 5
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data with time signature features |
| `.date_col` | column name (unquoted) | Date column |
| `.value_col` | column name (unquoted) | Numeric value to analyze |
| `.x_axis` | column name (unquoted) | Time component for x-axis (e.g., week, month) |
| `.ggplot_group_var` | column name (unquoted) | Grouping variable (typically year) |
| `.years_back` | numeric | Number of years to include in analysis. Default: 5 |

### Returns

A `ggplot2` object showing median and deviations.

### Examples

#### Weekly Pattern Analysis

```r
library(healthyR)
library(timetk)
library(dplyr)

# First add time signature
data_with_time <- ts_signature_tbl(
  .data = m4_daily,
  .date_col = date,
  .pad_time = TRUE,
  id
)

# Plot median excess by week
ts_median_excess_plt(
  .data = data_with_time,
  .date_col = date,
  .value_col = value,
  .x_axis = week,
  .ggplot_group_var = year,
  .years_back = 5
)
```

#### Hospital Census Patterns

```r
# Analyze census patterns
census_data <- hospital_data %>%
  ts_signature_tbl(.date_col = date) %>%
  group_by(date, year, week) %>%
  summarise(daily_census = n(), .groups = "drop")

ts_median_excess_plt(
  .data = census_data,
  .date_col = date,
  .value_col = daily_census,
  .x_axis = week,
  .ggplot_group_var = year,
  .years_back = 3
)
```

### Use Cases

- Identify seasonal patterns
- Detect unusual spikes or drops
- Compare current year to historical patterns
- Capacity planning
- Staffing optimization

### Notes

- Requires data with time signature features (use `ts_signature_tbl()` first)
- The median is calculated across all periods for comparison
- Values above the median indicate higher-than-typical utilization
- Years back parameter controls how much historical data to include

### See Also

- [ts_signature_tbl()](#ts_signature_tbl) - Add time features
- [ts_census_los_daily_tbl()](#ts_census_los_daily_tbl) - Calculate daily metrics

---

## ts_census_los_daily_tbl()

Calculate daily census and length of stay metrics.

### Description

Computes daily hospital census and average length of stay from admission and discharge dates.

### Usage

```r
ts_census_los_daily_tbl(
  .data,
  .date_col,
  .admit_col,
  .los_col = NULL
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Patient encounter data |
| `.date_col` | column name (unquoted) | Date column (typically discharge date) |
| `.admit_col` | column name (unquoted) | Admission date column |
| `.los_col` | column name (unquoted) | Optional: Pre-calculated LOS column. If NULL, calculated from dates |

### Returns

A tibble with daily census and LOS metrics.

### Examples

#### Basic Census Calculation

```r
library(healthyR)

census_data <- ts_census_los_daily_tbl(
  .data = hospital_data,
  .date_col = discharge_date,
  .admit_col = admission_date,
  .los_col = NULL  # Will calculate LOS
)

head(census_data)
```

#### With Pre-calculated LOS

```r
census_data <- ts_census_los_daily_tbl(
  .data = hospital_data,
  .date_col = discharge_date,
  .admit_col = admission_date,
  .los_col = length_of_stay  # Use existing column
)
```

#### Visualize Census Trends

```r
library(ggplot2)

census_data %>%
  ggplot(aes(x = date_col, y = census)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Daily Hospital Census", x = "Date", y = "Census")
```

### Use Cases

- Daily census monitoring
- Capacity planning
- Staffing level optimization
- Bed utilization analysis
- Seasonal demand forecasting

### See Also

- [ts_alos_plt()](#ts_alos_plt) - Plot ALOS trends
- [ts_signature_tbl()](#ts_signature_tbl) - Add time features

---

## ts_plt()

General-purpose time series plotting function.

### Description

A flexible time series plotting function for creating customized temporal visualizations.

### Usage

```r
ts_plt(
  .data,
  .date_col,
  .value_col,
  .by_grouping = "month",
  .interactive = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data to plot |
| `.date_col` | column name (unquoted) | Date column |
| `.value_col` | column name (unquoted) | Value to plot |
| `.by_grouping` | character | Time aggregation level |
| `.interactive` | logical | Interactive plotly chart if TRUE |

### Returns

A `ggplot2` or `plotly` object.

### Examples

#### Basic Time Series Plot

```r
library(healthyR)

ts_plt(
  .data = hospital_data,
  .date_col = encounter_date,
  .value_col = charges,
  .by_grouping = "month"
)
```

### See Also

- [ts_alos_plt()](#ts_alos_plt) - Specialized ALOS plotting
- [ts_readmit_rate_plt()](#ts_readmit_rate_plt) - Specialized readmission plotting

---

## Navigation

- [← Back to Home](Home)
- [Performance Metrics Functions →](Performance-Metrics-Functions)
- [View All Tutorials](Tutorial-ALOS-Analysis)

---

*Need help with time series analysis? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
