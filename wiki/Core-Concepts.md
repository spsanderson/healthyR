# Core Concepts

Understanding the fundamental concepts behind healthyR will help you use the package more effectively and make better decisions when analyzing hospital data.

## Table of Contents

- [Design Philosophy](#design-philosophy)
- [Tidy Data Principles](#tidy-data-principles)
- [Time Series Analysis](#time-series-analysis)
- [Service Line Classification](#service-line-classification)
- [Performance Metrics](#performance-metrics)
- [The Pipe Workflow](#the-pipe-workflow)

## Design Philosophy

healthyR is built on several core principles:

### 1. Consistency

All functions follow consistent naming and parameter conventions:

- **Prefix naming**: Functions are grouped by prefix
  - `ts_*` - Time series functions
  - `*_plt` - Plotting functions
  - `*_tbl` - Functions that return tibbles
  - `*_vec` - Functions that return vectors
  - `*_augment` - Functions that add columns to data

- **Parameter naming**: Common parameter patterns
  - `.data` - The input data frame or tibble
  - `.date_col` - Date column for time-based operations
  - `.value_col` - Numeric value column to analyze
  - `.dx_col` - Diagnosis code column
  - `.px_col` - Procedure code column
  - `.drg_col` - DRG code column

### 2. Simplicity

healthyR takes the guesswork out of common hospital analytics tasks. Instead of writing complex code for routine analyses, use simple, intuitive function calls.

**Before healthyR:**
```r
# Complex manual calculation
data %>%
  mutate(date = as.Date(date)) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(avg_los = mean(los, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_los)) +
  geom_line() +
  facet_wrap(~year) +
  theme_minimal() +
  labs(title = "Average Length of Stay", x = "Month", y = "Days")
```

**With healthyR:**
```r
# Simple function call
ts_alos_plt(.data = data, .date_col = date, .value_col = los, .by_grouping = "month")
```

### 3. Interoperability

healthyR works seamlessly with the tidyverse ecosystem:

- **dplyr** - For data manipulation
- **ggplot2** - For customizing plots
- **lubridate** - For date operations
- **tidyr** - For data reshaping
- **purrr** - For functional programming

### 4. Healthcare Focus

Functions are designed specifically for hospital and healthcare analytics use cases, with terminology and workflows familiar to healthcare analysts.

## Tidy Data Principles

healthyR expects data in "tidy" format, following these principles:

1. **Each variable is a column**
2. **Each observation is a row**
3. **Each type of observational unit is a table**

### Example of Tidy Hospital Data

```r
# GOOD - Tidy format
patient_encounters <- tibble(
  encounter_id = 1:5,
  patient_id = c(101, 102, 103, 104, 105),
  admission_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", 
                              "2024-01-04", "2024-01-05")),
  discharge_date = as.Date(c("2024-01-05", "2024-01-08", "2024-01-10",
                              "2024-01-07", "2024-01-09")),
  principal_dx = c("I50.9", "J96.00", "F10.10", "N39.0", "I21.09"),
  los = c(4, 6, 7, 3, 4)
)
```

### Why Tidy Data?

- **Easier manipulation**: Works naturally with dplyr verbs
- **Consistent structure**: Same operations work across datasets
- **Better for modeling**: Most R modeling functions expect tidy data
- **Facilitates visualization**: ggplot2 works best with tidy data

## Time Series Analysis

Time series analysis is central to hospital analytics. healthyR provides several approaches:

### Time Signatures

The `ts_signature_tbl()` function adds temporal features to your data:

```r
data_with_time <- ts_signature_tbl(
  .data = hospital_data,
  .date_col = admission_date
)
```

**Added features include:**
- `year`, `year.iso` - Calendar and ISO years
- `half` - Half of year (1 or 2)
- `quarter` - Quarter (1-4)
- `month`, `month.lbl` - Month number and label
- `week`, `week.iso` - Week numbers
- `wday`, `wday.lbl` - Weekday number and label
- `mday` - Day of month
- `yday` - Day of year
- `hour`, `minute`, `second` - Time components

### Time Aggregation

Functions like `ts_alos_plt()` support various aggregation levels:

- `"day"` - Daily aggregation
- `"week"` - Weekly aggregation
- `"month"` - Monthly aggregation
- `"quarter"` - Quarterly aggregation
- `"year"` - Yearly aggregation

### Padding Time Series

Use `.pad_time = TRUE` to fill in missing time periods:

```r
# Fill gaps in time series
complete_series <- ts_signature_tbl(
  .data = hospital_data,
  .date_col = admission_date,
  .pad_time = TRUE,
  patient_id  # Group by patient when padding
)
```

### Census and Length of Stay

Calculate daily census and LOS metrics:

```r
census_data <- ts_census_los_daily_tbl(
  .data = hospital_data,
  .date_col = discharge_date,
  .admit_col = admission_date,
  .los_col = los
)
```

## Service Line Classification

Service line classification groups patients into clinical categories based on their diagnoses, procedures, and DRGs.

### Why Service Lines Matter

- **Resource Planning**: Understand demand by clinical specialty
- **Financial Analysis**: Different service lines have different payment models
- **Quality Metrics**: Service line-specific benchmarking
- **Strategic Planning**: Identify growth opportunities

### How Classification Works

healthyR uses a hierarchical approach:

1. **Check DRG**: If DRG maps to a specific service line, use it
2. **Check Procedure**: If principal procedure maps to a service line, use it
3. **Check Diagnosis**: If principal diagnosis maps to a service line, use it
4. **Default**: If no match, classify as "Other"

### Service Line Categories

healthyR includes predefined mappings for common service lines:

- Alcohol Abuse
- Bariatric Surgery
- Cardiac
- Neurology
- Obstetrics
- Orthopedics
- Pulmonary
- Renal
- Psychiatric
- And more...

### Usage

```r
# As a vector (for mutate)
data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    )
  )

# As an augment function
data_augmented <- service_line_augment(
  .data = data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg,
  .drg_type = "ms"  # Medicare Severity DRG
)
```

### Reference Data

Service line mappings are based on:

- `dx_cc_mapping` - ICD-10 diagnosis to condition category mappings
- `px_cc_mapping` - ICD-10 procedure to procedure category mappings

These datasets are included with healthyR and based on AHRQ Clinical Classifications Software (CCS).

## Performance Metrics

healthyR provides tools for calculating and visualizing key performance indicators.

### Length of Stay (LOS)

**Average Length of Stay (ALOS)** is a key efficiency metric:

```r
# Plot ALOS trends
ts_alos_plt(
  .data = data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month"
)
```

**Considerations:**
- Outliers can skew ALOS (consider median)
- Different service lines have different expected LOS
- Seasonal variations are common

### Readmission Rates

Track patients returning within a specified timeframe:

```r
# Plot readmission trends
ts_readmit_rate_plt(
  .data = readmit_data,
  .date_col = discharge_date,
  .value_col = readmit_flag,
  .by_grouping = "month"
)
```

### LOS and Readmission Index

Combine LOS and readmission data into a single performance index:

```r
# Calculate index
index_data <- los_ra_index_summary_tbl(
  .data = data,
  .max_los = 15,
  .alos = avg_los,
  .readmit_rate = readmit_rate
)

# Visualize index
los_ra_index_plt(.data = index_data)
```

### Gartner Magic Charts

Quadrant analysis for comparing two metrics:

```r
gartner_magic_chart_plt(
  .data = data,
  .x_col = metric1,
  .y_col = metric2,
  .top_left_label = "High Y, Low X",
  .top_right_label = "High Y, High X",
  .bottom_left_label = "Low Y, Low X",
  .bottom_right_label = "Low Y, High X"
)
```

**Use cases:**
- LOS vs Readmission
- Cost vs Quality
- Volume vs Margin
- Efficiency vs Effectiveness

## The Pipe Workflow

healthyR is designed to work seamlessly with the pipe operator (`%>%` or `|>`):

### Basic Pipe Pattern

```r
library(dplyr)
library(healthyR)

# Typical healthyR workflow
hospital_data %>%
  # 1. Add time features
  ts_signature_tbl(.date_col = discharge_date) %>%
  # 2. Add service line
  service_line_augment(
    .dx_col = principal_dx,
    .px_col = principal_px,
    .drg_col = drg
  ) %>%
  # 3. Filter and transform
  filter(!is.na(service_line)) %>%
  mutate(los_category = case_when(
    los <= 3 ~ "Short",
    los <= 7 ~ "Medium",
    TRUE ~ "Long"
  )) %>%
  # 4. Summarize
  group_by(year, month.lbl, service_line) %>%
  summarise(
    n = n(),
    avg_los = mean(los),
    .groups = "drop"
  )
```

### Function Composition

Many healthyR functions can be chained:

```r
data %>%
  ts_signature_tbl(.date_col = date) %>%  # Add time features
  category_counts_tbl(                      # Count by category
    .count_col = service_line,
    .grouping_col = c(year, month.lbl)
  ) %>%
  top_n_tbl(.count_col = N, .n = 10)      # Get top 10
```

## Data Types and Coercion

### Date Handling

healthyR automatically handles various date formats:

```r
# These all work
ts_signature_tbl(.data = data, .date_col = date_as_date)
ts_signature_tbl(.data = data, .date_col = date_as_character)
ts_signature_tbl(.data = data, .date_col = date_as_posix)
```

### Missing Values

Most healthyR functions handle missing values gracefully:

- Plotting functions exclude NAs by default
- Summary functions use `na.rm = TRUE` internally
- Service line functions treat NA as "Other" or "unclassified"

### Factor vs Character

healthyR generally returns character vectors for categorical data, giving you flexibility to:

- Convert to factors when needed for ordered categories
- Keep as character for easier filtering and manipulation

```r
# Convert to factor if needed
data %>%
  mutate(service_line = factor(service_line, 
                                levels = c("cardiac", "orthopedic", "other")))
```

## Best Practices

1. **Start with tidy data** - Ensure your data is in tidy format
2. **Use the pipe** - Chain operations for readable code
3. **Add features early** - Use `ts_signature_tbl()` and `service_line_augment()` early in your pipeline
4. **Handle outliers** - Be aware of extreme values in metrics like LOS
5. **Group appropriately** - Consider seasonal patterns and clinical categories
6. **Validate results** - Spot-check calculated metrics against known values
7. **Document your work** - Use comments to explain business logic

## Next Steps

Now that you understand the core concepts:

1. **Explore Functions**: Review detailed documentation for:
   - [Time Series & Plotting Functions](Time-Series-and-Plotting-Functions)
   - [Performance Metrics Functions](Performance-Metrics-Functions)
   - [Data Transformation Functions](Data-Transformation-Functions)

2. **Try Tutorials**: Follow step-by-step guides:
   - [Average Length of Stay Analysis](Tutorial-ALOS-Analysis)
   - [Service Line Classification](Tutorial-Service-Line-Classification)

3. **Reference Materials**: Check out:
   - [Data Files Reference](Data-Files-Reference)
   - [API Reference](API-Reference)

---

*Questions about core concepts? Check the [FAQ](FAQ) or [ask a question](https://github.com/spsanderson/healthyR/issues).*
