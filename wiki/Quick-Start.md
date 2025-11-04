# Quick Start Guide

Get up and running with healthyR in 5 minutes! This guide covers the essentials to help you start analyzing hospital data right away.

## Prerequisites

Make sure you have healthyR installed. If not, see the [Installation Guide](Installation-Guide.md).

```r
install.packages("healthyR")
```

## Load Required Packages

```r
library(healthyR)
library(dplyr)
library(timetk)
library(ggplot2)
```

## Basic Workflow

### 1. Prepare Your Data

healthyR works best with tidy data where each row is an observation (e.g., a patient encounter) and columns represent variables.

#### Example Hospital Data Structure

Your data should typically include columns like:

- **Date columns**: Admission date, discharge date, visit date
- **Diagnosis codes**: ICD-10 principal and secondary diagnoses
- **Procedure codes**: ICD-10 procedure codes
- **DRG codes**: Diagnosis Related Group numbers
- **Metrics**: Length of stay, charges, payments, etc.
- **Demographics**: Age, gender, service area, etc.

```r
# Example data structure
library(tibble)

# Simulated hospital data
hospital_data <- tibble(
  patient_id = 1:100,
  admission_date = seq.Date(from = as.Date("2024-01-01"), 
                             by = "day", 
                             length.out = 100),
  discharge_date = admission_date + sample(1:15, 100, replace = TRUE),
  principal_dx = sample(c("I50.9", "J96.00", "F10.10", "N39.0"), 100, replace = TRUE),
  principal_px = sample(c("0BH17EZ", NA), 100, replace = TRUE),
  drg = sample(c("291", "292", "896", "689"), 100, replace = TRUE),
  age = sample(18:95, 100, replace = TRUE),
  los = as.numeric(discharge_date - admission_date)
)

head(hospital_data)
```

### 2. Time Series Analysis

Add time-based features to your data for temporal analysis:

```r
# Add time series signature features
data_with_time <- ts_signature_tbl(
  .data = hospital_data,
  .date_col = discharge_date,
  .pad_time = FALSE
)

# View the added time features
head(data_with_time)
```

This adds features like: year, month, week, day of week, quarter, etc.

### 3. Visualize Length of Stay Trends

Create a time series plot of average length of stay:

```r
# Plot ALOS by month
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = FALSE
)
```

For an interactive plot:

```r
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = TRUE
)
```

### 4. Service Line Classification

Classify patients into service lines based on clinical codes:

```r
# Add service line classification
data_with_service_line <- hospital_data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    )
  )

# View results
table(data_with_service_line$service_line)
```

Or use the augment function:

```r
data_with_service_line <- service_line_augment(
  .data = hospital_data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg,
  .drg_type = "ms"
)
```

### 5. Performance Analysis

Create a Gartner Magic Chart to analyze performance across two dimensions:

```r
# Example: Analyze LOS vs Readmission by service line
# First, calculate metrics by service line
performance_data <- data_with_service_line %>%
  group_by(service_line) %>%
  summarise(
    avg_los = mean(los, na.rm = TRUE),
    readmit_rate = mean(sample(c(0, 1), n(), replace = TRUE))  # Simulated
  ) %>%
  filter(!is.na(service_line))

# Create Gartner chart
gartner_magic_chart_plt(
  .data = performance_data,
  .x_col = avg_los,
  .y_col = readmit_rate,
  .point_size_col = NULL,
  .x_lab = "Average Length of Stay",
  .y_lab = "Readmission Rate",
  .plot_title = "Service Line Performance",
  .top_right_label = "High LOS & Readmits",
  .top_left_label = "High Readmits Only",
  .bottom_left_label = "Best Performers",
  .bottom_right_label = "High LOS Only"
)
```

### 6. Category Counts

Get frequency counts of any categorical variable:

```r
# Count by service line
category_counts_tbl(
  .data = data_with_service_line,
  .count_col = service_line,
  .arrange_value = TRUE
)

# Count by service line and month
data_with_time_and_sl <- data_with_service_line %>%
  ts_signature_tbl(.date_col = discharge_date, .pad_time = FALSE)

category_counts_tbl(
  .data = data_with_time_and_sl,
  .count_col = service_line,
  .grouping_col = c(year, month.lbl),
  .arrange_value = TRUE
)
```

### 7. Export Results to Excel

Save your analysis results to Excel with a timestamp:

```r
# Save a single table
save_to_excel(
  .data = data_with_service_line,
  .file_name = "hospital_analysis"
)

# Save multiple tables to different sheets
results_list <- list(
  summary = performance_data,
  details = data_with_service_line,
  counts = category_counts_tbl(.data = data_with_service_line, 
                                .count_col = service_line)
)

save_to_excel(
  .data = results_list,
  .file_name = "comprehensive_analysis"
)
```

## Working with Real Data

### Using healthyR.data Package

For practice and examples, use the companion `healthyR.data` package:

```r
# Install if needed
install.packages("healthyR.data")

library(healthyR.data)

# Load sample data
data(healthyR_data)
```

### Loading Your Own Data

```r
# From CSV
my_data <- readr::read_csv("path/to/your/data.csv")

# From Excel
my_data <- readxl::read_excel("path/to/your/data.xlsx")

# From database (example with DBI)
library(DBI)
con <- dbConnect(odbc::odbc(), 
                 driver = "SQL Server",
                 server = "your-server",
                 database = "your-db")
my_data <- dbGetQuery(con, "SELECT * FROM encounters")
dbDisconnect(con)
```

## Common Patterns

### Pattern 1: Monthly Trend Analysis

```r
hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date, .pad_time = FALSE) %>%
  group_by(year, month.lbl) %>%
  summarise(
    n_encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month.lbl, y = avg_los, group = year, color = factor(year))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Length of Stay by Month",
       x = "Month", y = "Average LOS", color = "Year")
```

### Pattern 2: Service Line Dashboard

```r
# Create comprehensive service line summary
sl_summary <- data_with_service_line %>%
  group_by(service_line) %>%
  summarise(
    n_encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(service_line)) %>%
  arrange(desc(n_encounters))

print(sl_summary)
```

### Pattern 3: Top N Analysis

```r
# Get top 10 most common diagnoses
top_dx <- top_n_tbl(
  .data = hospital_data,
  .count_col = principal_dx,
  .n = 10
)

print(top_dx)
```

## Accessibility Features

healthyR includes color-blind friendly palettes:

```r
# Use color-blind friendly colors
library(ggplot2)

ggplot(performance_data, aes(x = service_line, y = avg_los, fill = service_line)) +
  geom_col() +
  hr_scale_fill_colorblind() +  # Apply color-blind friendly palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Next Steps

Now that you've completed the quick start:

1. **Dive Deeper**: Explore [Core Concepts](Core-Concepts.md) to understand healthyR's philosophy
2. **Function Reference**: Browse all available functions:
   - [Time Series & Plotting Functions](Time-Series-and-Plotting-Functions.md)
   - [Performance Metrics Functions](Performance-Metrics-Functions.md)
   - [Data Transformation Functions](Data-Transformation-Functions.md)
3. **Tutorials**: Follow detailed tutorials:
   - [Average Length of Stay Analysis](Tutorial-ALOS-Analysis.md)
   - [Readmission Rate Analysis](Tutorial-Readmission-Analysis.md)
   - [Service Line Classification](Tutorial-Service-Line-Classification.md)
4. **Get Help**: Check the [FAQ](FAQ.md) and [Troubleshooting](Troubleshooting.md) pages

## Quick Reference Card

```r
# Installation
install.packages("healthyR")

# Load
library(healthyR)

# Time features
ts_signature_tbl(.data, .date_col)

# Visualizations
ts_alos_plt(.data, .date_col, .value_col, .by_grouping)
ts_readmit_rate_plt(.data, .date_col, .value_col, .by_grouping)
gartner_magic_chart_plt(.data, .x_col, .y_col)

# Service lines
service_line_vec(.data, .dx_col, .px_col, .drg_col)
service_line_augment(.data, .dx_col, .px_col, .drg_col)

# Data operations
category_counts_tbl(.data, .count_col)
top_n_tbl(.data, .count_col, .n)

# Export
save_to_excel(.data, .file_name)

# Accessibility
hr_scale_fill_colorblind()
hr_scale_color_colorblind()
```

---

*Ready for more? Check out the [detailed tutorials](Tutorial-ALOS-Analysis.md) or explore the [complete function reference](Time-Series-and-Plotting-Functions.md).*
