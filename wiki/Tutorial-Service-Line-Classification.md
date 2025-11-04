# Tutorial: Service Line Classification

Learn how to classify patients into service lines using healthyR.

## Table of Contents

- [Introduction](#introduction)
- [Prerequisites](#prerequisites)
- [Understanding Service Lines](#understanding-service-lines)
- [Basic Classification](#basic-classification)
- [Analyzing Service Line Data](#analyzing-service-line-data)
- [Advanced Techniques](#advanced-techniques)
- [Complete Workflow](#complete-workflow)

---

## Introduction

Service line classification groups patients by clinical specialty (e.g., Cardiac, Orthopedics, Obstetrics). This is essential for:

- Financial analysis and planning
- Resource allocation
- Quality benchmarking
- Strategic decision-making

### What You'll Learn

- How service line classification works
- Using `service_line_vec()` and `service_line_augment()`
- Analyzing data by service line
- Creating service line reports
- Handling edge cases and unmapped codes

### Time Required

20-30 minutes

---

## Prerequisites

```r
# Load required packages
library(healthyR)
library(dplyr)
library(ggplot2)
```

### Sample Data

```r
# Create sample hospital data
set.seed(123)
hospital_data <- tibble(
  encounter_id = 1:500,
  admission_date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), length.out = 500),
  discharge_date = admission_date + sample(1:15, 500, replace = TRUE),
  principal_dx = sample(
    c("I50.9", "I21.09", "J96.00", "F10.10", "N39.0", 
      "M17.11", "O80", "G45.9", "E11.9", "K80.20"),
    500, replace = TRUE
  ),
  principal_px = sample(
    c(NA, "0SR90J9", "0BH17EZ", "021209W", "10D00Z0"),
    500, replace = TRUE, prob = c(0.5, 0.125, 0.125, 0.125, 0.125)
  ),
  drg = sample(
    c("291", "292", "470", "689", "896", "190", "767", "945", "640", "872"),
    500, replace = TRUE
  ),
  age = sample(18:95, 500, replace = TRUE),
  total_charges = runif(500, 5000, 50000)
)

hospital_data <- hospital_data %>%
  mutate(los = as.numeric(discharge_date - admission_date))

head(hospital_data)
```

---

## Understanding Service Lines

### What Are Service Lines?

Service lines group related clinical services:

- **Cardiac** - Heart conditions and procedures
- **Orthopedics** - Bone and joint conditions
- **Neurology** - Brain and nervous system
- **Obstetrics** - Childbirth and pregnancy
- **Pulmonary** - Lung and respiratory
- **Renal** - Kidney conditions
- **And more...**

### How Classification Works

healthyR uses a hierarchical approach:

1. **Check DRG** - Some DRGs map directly to service lines
2. **Check Procedure** - Principal procedure can determine service line
3. **Check Diagnosis** - Principal diagnosis as fallback
4. **Default to "other"** - If no match found

### Data Requirements

You need three code columns:
- **Principal Diagnosis** (ICD-10-CM format: "I50.9")
- **Principal Procedure** (ICD-10-PCS format: "0SR90J9")
- **DRG Number** (as character: "291")

---

## Basic Classification

### Method 1: Using service_line_vec()

Use within `mutate()` for flexibility:

```r
# Add service line column
data_with_sl <- hospital_data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    )
  )

# View results
head(data_with_sl %>% select(encounter_id, principal_dx, principal_px, drg, service_line))
```

### Method 2: Using service_line_augment()

One-step augmentation:

```r
# Augment data with service line
data_with_sl <- service_line_augment(
  .data = hospital_data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg,
  .drg_type = "ms"  # Medicare Severity DRG
)

head(data_with_sl)
```

### Check Classification Results

```r
# Count by service line
service_line_counts <- data_with_sl %>%
  count(service_line, sort = TRUE)

print(service_line_counts)

# Calculate percentage
service_line_counts %>%
  mutate(percentage = n / sum(n) * 100)
```

### Visualize Distribution

```r
ggplot(service_line_counts, aes(x = reorder(service_line, n), y = n, fill = service_line)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2) +
  hr_scale_fill_colorblind() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Encounters by Service Line",
    x = "Service Line",
    y = "Number of Encounters"
  ) +
  theme(legend.position = "none")
```

---

## Analyzing Service Line Data

### Volume Analysis

```r
# Monthly volume by service line
monthly_volume <- data_with_sl %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(year, month, month.lbl, service_line) %>%
  summarise(encounters = n(), .groups = "drop")

# Visualize
ggplot(monthly_volume, aes(x = month, y = encounters, color = service_line, group = service_line)) +
  geom_line(size = 1) +
  geom_point() +
  hr_scale_color_colorblind() +
  theme_minimal() +
  labs(
    title = "Monthly Encounter Volume by Service Line",
    x = "Month",
    y = "Encounters",
    color = "Service Line"
  )
```

### Financial Analysis

```r
# Revenue and charges by service line
financial_summary <- data_with_sl %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    total_charges = sum(total_charges),
    avg_charges = mean(total_charges),
    median_charges = median(total_charges),
    .groups = "drop"
  ) %>%
  arrange(desc(total_charges))

print(financial_summary)

# Visualize
ggplot(financial_summary, aes(x = reorder(service_line, total_charges), y = total_charges)) +
  geom_col(fill = color_blind()[2]) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Charges by Service Line",
    x = "Service Line",
    y = "Total Charges"
  )
```

### Length of Stay by Service Line

```r
# ALOS by service line
los_by_sl <- data_with_sl %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    .groups = "drop"
  ) %>%
  arrange(desc(alos))

print(los_by_sl)

# Box plot
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

### Age Demographics

```r
# Age analysis by service line
age_by_sl <- data_with_sl %>%
  filter(!is.na(service_line), service_line != "other") %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "<18",
      age < 40 ~ "18-39",
      age < 65 ~ "40-64",
      TRUE ~ "65+"
    )
  ) %>%
  group_by(service_line, age_group) %>%
  summarise(encounters = n(), .groups = "drop")

# Stacked bar chart
ggplot(age_by_sl, aes(x = service_line, y = encounters, fill = age_group)) +
  geom_col(position = "fill") +
  hr_scale_fill_colorblind() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Age Distribution by Service Line",
    x = "Service Line",
    y = "Percentage",
    fill = "Age Group"
  )
```

---

## Advanced Techniques

### Verify Classifications

```r
# Check what codes mapped to each service line
code_mapping <- data_with_sl %>%
  group_by(service_line, principal_dx) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(service_line, desc(count))

# View top codes for each service line
code_mapping %>%
  group_by(service_line) %>%
  slice_head(n = 5)
```

### Handle Unclassified Cases

```r
# Analyze "other" category
other_cases <- data_with_sl %>%
  filter(service_line == "other") %>%
  count(principal_dx, principal_px, drg, sort = TRUE)

print(other_cases)

# What percentage is "other"?
data_with_sl %>%
  mutate(is_other = service_line == "other") %>%
  summarise(
    total = n(),
    other_count = sum(is_other),
    other_pct = mean(is_other) * 100
  )
```

### Custom Grouping

```r
# Create custom service line groups
data_custom <- data_with_sl %>%
  mutate(
    service_line_grouped = case_when(
      service_line %in% c("cardiac", "pulmonary") ~ "Cardiopulmonary",
      service_line %in% c("orthopedics", "neurology") ~ "Surgical Specialties",
      service_line == "obstetrics" ~ "Women's Health",
      service_line == "other" ~ "Other",
      TRUE ~ "Medical"
    )
  )

# Analyze grouped service lines
data_custom %>%
  count(service_line_grouped, sort = TRUE)
```

### Time-Based Changes

```r
# Has service line mix changed over time?
mix_over_time <- data_with_sl %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(year, quarter, service_line) %>%
  summarise(encounters = n(), .groups = "drop") %>%
  group_by(year, quarter) %>%
  mutate(percentage = encounters / sum(encounters) * 100)

# Visualize
ggplot(mix_over_time, aes(x = quarter, y = percentage, fill = service_line)) +
  geom_area() +
  hr_scale_fill_colorblind() +
  facet_wrap(~year) +
  theme_minimal() +
  labs(
    title = "Service Line Mix Over Time",
    x = "Quarter",
    y = "Percentage of Total Encounters",
    fill = "Service Line"
  )
```

### Compare to Benchmarks

```r
# Compare your mix to a benchmark
benchmark <- tibble(
  service_line = c("cardiac", "orthopedics", "pulmonary", "other"),
  benchmark_pct = c(25, 20, 15, 40)
)

actual_mix <- data_with_sl %>%
  count(service_line) %>%
  mutate(actual_pct = n / sum(n) * 100) %>%
  select(service_line, actual_pct)

comparison <- actual_mix %>%
  left_join(benchmark, by = "service_line") %>%
  mutate(
    difference = actual_pct - benchmark_pct,
    status = case_when(
      difference > 5 ~ "Above Benchmark",
      difference < -5 ~ "Below Benchmark",
      TRUE ~ "At Benchmark"
    )
  )

print(comparison)
```

---

## Complete Workflow

Here's a complete service line analysis workflow:

```r
library(healthyR)
library(dplyr)
library(ggplot2)

# 1. Classify service lines
analysis_data <- hospital_data %>%
  service_line_augment(
    .dx_col = principal_dx,
    .px_col = principal_px,
    .drg_col = drg
  ) %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(!is.na(service_line))

# 2. Volume Summary
volume_summary <- analysis_data %>%
  count(service_line, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

# 3. Financial Summary
financial_summary <- analysis_data %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    total_charges = sum(total_charges),
    avg_charges = mean(total_charges),
    .groups = "drop"
  ) %>%
  arrange(desc(total_charges))

# 4. Clinical Summary
clinical_summary <- analysis_data %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    alos = mean(los),
    median_los = median(los),
    avg_age = mean(age),
    .groups = "drop"
  )

# 5. Trend Analysis
monthly_trends <- analysis_data %>%
  filter(service_line != "other") %>%
  group_by(year, month, month.lbl, service_line) %>%
  summarise(encounters = n(), .groups = "drop")

# 6. Create Visualizations
p1 <- ggplot(volume_summary, aes(x = reorder(service_line, n), y = n)) +
  geom_col(fill = color_blind()[2]) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Volume by Service Line", x = "", y = "Encounters")

p2 <- ggplot(monthly_trends, aes(x = month, y = encounters, 
                                   color = service_line, group = service_line)) +
  geom_line(size = 1) +
  hr_scale_color_colorblind() +
  theme_minimal() +
  labs(title = "Monthly Trends", color = "Service Line")

# 7. Export Results
report_data <- list(
  volume = volume_summary,
  financial = financial_summary,
  clinical = clinical_summary,
  monthly_trends = monthly_trends
)

save_to_excel(.data = report_data, .file_name = "service_line_analysis")

# Display
print(p1)
print(p2)
```

---

## Troubleshooting

### Issue: All patients classified as "other"

**Check code format:**
```r
# Diagnosis codes should have periods
unique(hospital_data$principal_dx) %>% head()
# Should see: "I50.9", "J96.00", etc.

# Procedure codes NO periods
unique(hospital_data$principal_px) %>% head()
# Should see: "0SR90J9", "0BH17EZ", etc.

# DRG as character
class(hospital_data$drg)  # Should be "character"
```

### Issue: Want to see mapping logic

**Examine the reference tables:**
```r
# Check diagnosis mapping
dx_cc_mapping %>%
  filter(ICD_Code == "I50.9", ICD_Ver_Flag == "10")

# Check procedure mapping
px_cc_mapping %>%
  filter(ICD_Code == "0SR90J9", ICD_Ver_Flag == "10")
```

### Issue: Service line doesn't match expectation

**Verify the hierarchy:**
```r
# Service line assignment uses this order:
# 1. DRG
# 2. Procedure
# 3. Diagnosis

# Check each individually
hospital_data %>%
  slice(1) %>%
  select(principal_dx, principal_px, drg)
```

---

## Best Practices

1. **Clean Your Codes** - Ensure proper formatting before classification
2. **Validate Results** - Spot-check classifications against clinical knowledge
3. **Document Unmapped Codes** - Track "other" category for follow-up
4. **Consider Case Mix** - Different facilities have different service line mixes
5. **Update Regularly** - Periodically review and update classifications
6. **Use Consistently** - Apply same logic across analyses for comparability

---

## Navigation

- [← Tutorial: ALOS Analysis](Tutorial-ALOS-Analysis.md)
- [FAQ →](FAQ.md)
- [View All Tutorials](Home.md#tutorials--examples)

---

*Questions? Check the [FAQ](FAQ.md) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
