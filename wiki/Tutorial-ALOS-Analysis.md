# Tutorial: Average Length of Stay (ALOS) Analysis

A comprehensive guide to analyzing and visualizing average length of stay using healthyR.

## Table of Contents

- [Introduction](#introduction)
- [Prerequisites](#prerequisites)
- [Understanding ALOS](#understanding-alos)
- [Basic ALOS Analysis](#basic-alos-analysis)
- [Time Series ALOS](#time-series-alos)
- [ALOS by Service Line](#alos-by-service-line)
- [Advanced Analysis](#advanced-analysis)
- [Best Practices](#best-practices)
- [Complete Example](#complete-example)

---

## Introduction

Average Length of Stay (ALOS) is one of the most important efficiency metrics in hospital operations. This tutorial demonstrates how to calculate, visualize, and analyze ALOS using healthyR.

### What You'll Learn

- Calculate ALOS for different time periods
- Create time series visualizations
- Analyze ALOS by service line and other dimensions
- Identify trends and outliers
- Compare to benchmarks

### Time Required

30-45 minutes

---

## Prerequisites

### Install and Load Packages

```r
# Install if needed
install.packages("healthyR")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")

# Load packages
library(healthyR)
library(dplyr)
library(ggplot2)
library(lubridate)
```

### Sample Data

For this tutorial, we'll use simulated hospital data:

```r
# Create sample data
set.seed(123)
hospital_data <- tibble(
  encounter_id = 1:1000,
  patient_id = sample(1:500, 1000, replace = TRUE),
  admission_date = seq.Date(
    from = as.Date("2023-01-01"), 
    to = as.Date("2024-10-31"), 
    length.out = 1000
  ),
  discharge_date = admission_date + sample(1:20, 1000, replace = TRUE, 
                                            prob = c(rep(0.15, 5), rep(0.05, 10), rep(0.01, 5))),
  principal_dx = sample(c("I50.9", "J96.00", "F10.10", "N39.0", "M17.11"), 
                        1000, replace = TRUE),
  principal_px = sample(c(NA, "0SR90J9", "0BH17EZ"), 1000, replace = TRUE, 
                        prob = c(0.6, 0.2, 0.2)),
  drg = sample(c("291", "292", "470", "689", "896"), 1000, replace = TRUE),
  age = sample(18:95, 1000, replace = TRUE),
  readmit_flag = sample(0:1, 1000, replace = TRUE, prob = c(0.85, 0.15))
)

# Calculate length of stay
hospital_data <- hospital_data %>%
  mutate(los = as.numeric(discharge_date - admission_date))

head(hospital_data)
```

---

## Understanding ALOS

### Definition

**Average Length of Stay (ALOS)** is the average number of days patients remain in the hospital, calculated as:

```
ALOS = Total Patient Days / Number of Discharges
```

Or simply:
```
ALOS = mean(length_of_stay)
```

### Why ALOS Matters

1. **Efficiency Indicator**: Lower ALOS often indicates efficient care delivery
2. **Cost Management**: Each additional day increases costs
3. **Capacity Planning**: Affects bed availability
4. **Quality Metric**: Can indicate complications or delays
5. **Benchmarking**: Compare to national or peer standards

### Important Considerations

- **Case Mix**: Different conditions have different expected LOS
- **Severity**: Sicker patients typically stay longer
- **Service Lines**: Cardiac, surgical, and medical patients have different patterns
- **Outliers**: Very long stays can skew the average
- **Timing**: Use discharge date for accurate trending

---

## Basic ALOS Analysis

### Overall ALOS

```r
# Calculate overall ALOS
overall_alos <- hospital_data %>%
  summarise(
    total_encounters = n(),
    total_patient_days = sum(los),
    alos = mean(los),
    median_los = median(los),
    sd_los = sd(los),
    min_los = min(los),
    max_los = max(los)
  )

print(overall_alos)
```

### ALOS by Category

```r
# ALOS by DRG
alos_by_drg <- hospital_data %>%
  group_by(drg) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    .groups = "drop"
  ) %>%
  arrange(desc(alos))

print(alos_by_drg)
```

### Visualize Distribution

```r
# Histogram of length of stay
ggplot(hospital_data, aes(x = los)) +
  geom_histogram(
    binwidth = opt_bin(hospital_data$los),
    fill = color_blind()[2],
    color = "white"
  ) +
  geom_vline(
    xintercept = mean(hospital_data$los),
    color = color_blind()[7],
    linetype = "dashed",
    size = 1
  ) +
  annotate(
    "text",
    x = mean(hospital_data$los) + 1,
    y = Inf,
    label = paste("Mean:", round(mean(hospital_data$los), 1)),
    vjust = 1.5,
    hjust = 0
  ) +
  theme_minimal() +
  labs(
    title = "Length of Stay Distribution",
    x = "Length of Stay (Days)",
    y = "Number of Patients"
  )
```

---

## Time Series ALOS

### Monthly ALOS Trends

```r
# Plot monthly ALOS using healthyR
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = FALSE
)
```

### Customize the Plot

```r
# Create and customize
p <- ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month"
)

p + 
  labs(
    title = "Monthly Average Length of Stay Trend",
    subtitle = "January 2023 - October 2024",
    y = "Average LOS (Days)",
    x = "Month"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )
```

### Weekly ALOS for Recent Period

```r
# Focus on recent data
recent_data <- hospital_data %>%
  filter(discharge_date >= as.Date("2024-07-01"))

ts_alos_plt(
  .data = recent_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "week"
) +
  labs(title = "Weekly ALOS - Last 4 Months")
```

### Interactive Plot

```r
# Create interactive plot for exploration
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = TRUE
)
```

---

## ALOS by Service Line

### Add Service Line Classification

```r
# Classify service lines
data_with_sl <- service_line_augment(
  .data = hospital_data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg,
  .drg_type = "ms"
)

head(data_with_sl)
```

### Service Line Summary

```r
# Calculate ALOS by service line
sl_alos <- data_with_sl %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    q25 = quantile(los, 0.25),
    q75 = quantile(los, 0.75),
    .groups = "drop"
  ) %>%
  filter(!is.na(service_line), service_line != "other") %>%
  arrange(desc(alos))

print(sl_alos)
```

### Visualize Service Line ALOS

```r
# Bar chart of ALOS by service line
ggplot(sl_alos, aes(x = reorder(service_line, alos), y = alos, fill = service_line)) +
  geom_col() +
  geom_text(aes(label = round(alos, 1)), hjust = -0.2) +
  hr_scale_fill_colorblind() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Average Length of Stay by Service Line",
    x = "Service Line",
    y = "Average LOS (Days)"
  ) +
  theme(legend.position = "none") +
  expand_limits(y = max(sl_alos$alos) * 1.1)
```

### Box Plot by Service Line

```r
# Box plot shows distribution
data_with_sl %>%
  filter(!is.na(service_line), service_line != "other") %>%
  ggplot(aes(x = reorder(service_line, los, FUN = median), y = los, fill = service_line)) +
  geom_boxplot() +
  hr_scale_fill_colorblind() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Length of Stay Distribution by Service Line",
    x = "Service Line",
    y = "Length of Stay (Days)"
  ) +
  theme(legend.position = "none")
```

### Service Line Trends Over Time

```r
# ALOS trends by service line
data_with_sl %>%
  filter(!is.na(service_line), service_line != "other") %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, month, service_line) %>%
  summarise(alos = mean(los), .groups = "drop") %>%
  ggplot(aes(x = month, y = alos, color = service_line, group = service_line)) +
  geom_line(size = 1) +
  geom_point() +
  hr_scale_color_colorblind() +
  facet_wrap(~year) +
  theme_minimal() +
  labs(
    title = "Monthly ALOS Trends by Service Line",
    x = "Month",
    y = "Average LOS (Days)",
    color = "Service Line"
  )
```

---

## Advanced Analysis

### Identifying Outliers

```r
# Flag outliers (> 95th percentile or < 5th percentile)
outlier_threshold <- quantile(hospital_data$los, c(0.05, 0.95))

data_with_outliers <- hospital_data %>%
  mutate(
    outlier_flag = case_when(
      los < outlier_threshold[1] ~ "Low Outlier",
      los > outlier_threshold[2] ~ "High Outlier",
      TRUE ~ "Normal"
    )
  )

# Summary by outlier status
data_with_outliers %>%
  count(outlier_flag) %>%
  mutate(pct = n / sum(n) * 100)
```

### ALOS Excluding Outliers

```r
# Compare ALOS with and without outliers
comparison <- tibble(
  metric = c("All Patients", "Excluding Outliers"),
  alos = c(
    mean(hospital_data$los),
    mean(data_with_outliers$los[data_with_outliers$outlier_flag == "Normal"])
  ),
  median_los = c(
    median(hospital_data$los),
    median(data_with_outliers$los[data_with_outliers$outlier_flag == "Normal"])
  )
)

print(comparison)
```

### Age-Stratified Analysis

```r
# ALOS by age group
age_analysis <- hospital_data %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "Pediatric (< 18)",
      age < 65 ~ "Adult (18-64)",
      TRUE ~ "Senior (65+)"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    .groups = "drop"
  )

print(age_analysis)

# Visualize
ggplot(age_analysis, aes(x = age_group, y = alos, fill = age_group)) +
  geom_col() +
  geom_text(aes(label = round(alos, 1)), vjust = -0.5) +
  hr_scale_fill_colorblind() +
  theme_minimal() +
  labs(
    title = "Average Length of Stay by Age Group",
    x = "Age Group",
    y = "Average LOS (Days)"
  ) +
  theme(legend.position = "none")
```

### Readmission Impact on LOS

```r
# Does readmission status affect LOS?
readmit_analysis <- hospital_data %>%
  mutate(readmit_status = ifelse(readmit_flag == 1, "Readmitted", "Not Readmitted")) %>%
  group_by(readmit_status) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    .groups = "drop"
  )

print(readmit_analysis)

# Statistical test
t.test(los ~ readmit_flag, data = hospital_data)
```

### Seasonal Patterns

```r
# Analyze seasonal patterns
seasonal_alos <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(quarter) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    .groups = "drop"
  )

ggplot(seasonal_alos, aes(x = factor(quarter), y = alos, fill = factor(quarter))) +
  geom_col() +
  geom_text(aes(label = round(alos, 2)), vjust = -0.5) +
  hr_scale_fill_colorblind() +
  theme_minimal() +
  labs(
    title = "Average Length of Stay by Quarter",
    x = "Quarter",
    y = "Average LOS (Days)",
    fill = "Quarter"
  )
```

---

## Best Practices

### 1. Handle Missing Data

```r
# Check for missing or invalid LOS
hospital_data %>%
  summarise(
    total_records = n(),
    missing_admit = sum(is.na(admission_date)),
    missing_discharge = sum(is.na(discharge_date)),
    missing_los = sum(is.na(los)),
    negative_los = sum(los < 0, na.rm = TRUE),
    zero_los = sum(los == 0, na.rm = TRUE)
  )
```

### 2. Set Reasonable Limits

```r
# Filter unrealistic values
clean_data <- hospital_data %>%
  filter(
    los > 0,           # Must be positive
    los <= 365,        # Cap at 1 year
    !is.na(los)        # No missing
  )
```

### 3. Use Median for Skewed Data

```r
# ALOS is often right-skewed
# Consider reporting both mean and median
summary_stats <- hospital_data %>%
  summarise(
    mean_los = mean(los),
    median_los = median(los),
    mean_vs_median_diff = mean_los - median_los
  )

# If mean >> median, data is right-skewed
```

### 4. Account for Case Mix

```r
# Adjust for service line when comparing
# Don't compare cardiac ALOS to OB ALOS directly
```

### 5. Consider Geometric Mean for Outliers

```r
# Geometric mean is less sensitive to outliers
geometric_mean_los <- exp(mean(log(hospital_data$los)))
print(geometric_mean_los)
```

---

## Complete Example

Here's a complete analysis workflow:

```r
library(healthyR)
library(dplyr)
library(ggplot2)

# 1. Load and prepare data
hospital_data <- hospital_data %>%
  filter(los > 0, los <= 365) %>%  # Clean data
  service_line_augment(                # Add service line
    .dx_col = principal_dx,
    .px_col = principal_px,
    .drg_col = drg
  ) %>%
  ts_signature_tbl(.date_col = discharge_date)  # Add time features

# 2. Overall summary
overall <- hospital_data %>%
  summarise(
    encounters = n(),
    alos = round(mean(los), 1),
    median_los = round(median(los), 1)
  )

# 3. Service line comparison
by_service_line <- hospital_data %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    alos = round(mean(los), 1),
    median_los = round(median(los), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(alos))

# 4. Trend analysis
monthly_trend <- hospital_data %>%
  group_by(year, month, month.lbl) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    .groups = "drop"
  )

# 5. Visualizations
# Overall trend
p1 <- ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month"
) +
  labs(title = "Monthly ALOS Trend")

# By service line
p2 <- ggplot(by_service_line, aes(x = reorder(service_line, alos), y = alos)) +
  geom_col(fill = color_blind()[2]) +
  geom_text(aes(label = alos), hjust = -0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "ALOS by Service Line", x = "", y = "Days")

# 6. Export results
report_data <- list(
  summary = overall,
  by_service_line = by_service_line,
  monthly_trend = monthly_trend
)

save_to_excel(.data = report_data, .file_name = "alos_analysis")

# Display
print(p1)
print(p2)
```

---

## Navigation

- [← Data Files Reference](Data-Files-Reference)
- [Readmission Analysis Tutorial →](Tutorial-Readmission-Analysis)
- [View All Tutorials](Home#tutorials--examples)

---

*Questions? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
