# Utility Functions

Reference for utility and helper functions in healthyR.

## Table of Contents

- [save_to_excel()](#save_to_excel)
- [opt_bin()](#opt_bin)
- [SQL String Functions](#sql-string-functions)
  - [sql_left()](#sql_left)
  - [sql_right()](#sql_right)
  - [sql_mid()](#sql_mid)

---

## save_to_excel()

Export data frames or lists of data frames to Excel with timestamp.

### Description

A convenient wrapper for exporting R data to Excel files. Automatically adds a timestamp to the filename and handles both single data frames and lists of data frames (creating multiple sheets).

### Usage

```r
save_to_excel(
  .data,
  .file_name
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame, tibble, or list | Data to export. Can be a single data frame or a named list of data frames |
| `.file_name` | character | Base file name (without .xlsx extension) |

### Returns

Invisibly returns the file path. Writes an Excel file to the current working directory.

### Filename Convention

The function automatically adds a timestamp to your filename:
- Input: `"my_report"`
- Output: `"my_report_2024_11_04_103045.xlsx"`
- Format: `filename_YYYY_MM_DD_HHMMSS.xlsx`

### Examples

#### Export Single Data Frame

```r
library(healthyR)
library(dplyr)

# Create summary data
summary_data <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    total_charges = sum(charges, na.rm = TRUE)
  )

# Export to Excel
save_to_excel(
  .data = summary_data,
  .file_name = "service_line_summary"
)

# Creates: service_line_summary_2024_11_04_103045.xlsx
```

#### Export Multiple Sheets

```r
library(healthyR)

# Create multiple analyses
analyses <- list(
  summary = summary_by_service_line,
  top_diagnoses = top_dx_data,
  monthly_trends = monthly_data,
  provider_metrics = provider_summary
)

# Export all to one Excel file with multiple sheets
save_to_excel(
  .data = analyses,
  .file_name = "comprehensive_report"
)

# Creates file with 4 sheets: summary, top_diagnoses, monthly_trends, provider_metrics
```

#### Complete Workflow Example

```r
library(healthyR)
library(dplyr)

# Prepare multiple related analyses
report_data <- list(
  
  # Executive Summary
  executive_summary = hospital_data %>%
    summarise(
      total_encounters = n(),
      avg_los = mean(los, na.rm = TRUE),
      total_charges = sum(charges, na.rm = TRUE),
      readmit_rate = mean(readmit_flag, na.rm = TRUE)
    ),
  
  # Service Line Detail
  service_line = hospital_data %>%
    group_by(service_line) %>%
    summarise(
      encounters = n(),
      avg_los = mean(los, na.rm = TRUE),
      readmit_rate = mean(readmit_flag, na.rm = TRUE)
    ),
  
  # Top Diagnoses
  top_diagnoses = top_n_tbl(
    .data = hospital_data,
    .count_col = principal_dx,
    .n = 20
  ),
  
  # Monthly Volume
  monthly_volume = hospital_data %>%
    ts_signature_tbl(.date_col = discharge_date) %>%
    group_by(year, month.lbl) %>%
    summarise(encounters = n(), .groups = "drop")
)

# Export everything
save_to_excel(
  .data = report_data,
  .file_name = "monthly_hospital_report"
)
```

#### Using with named_item_list()

```r
# Split data by category and export
service_line_reports <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    top_dx = first(principal_dx)
  ) %>%
  named_item_list(.group_col = service_line)

# Export each service line to its own sheet
save_to_excel(
  .data = service_line_reports,
  .file_name = "service_line_detail"
)
```

#### Set Working Directory First

```r
# Control where files are saved
setwd("C:/Reports/2024")

save_to_excel(
  .data = my_data,
  .file_name = "report"
)

# Or use full paths (though function adds timestamp to basename only)
```

### Use Cases

- Automated report generation
- Data export for stakeholders
- Archiving analysis results
- Sharing data with non-R users
- Creating timestamped backups

### Features

1. **Automatic Timestamp**: Never overwrite previous exports
2. **Multiple Sheets**: Export related data together
3. **Named Sheets**: List names become sheet names
4. **No Dependencies**: Uses `writexl` (no Excel installation needed)
5. **Cross-Platform**: Works on Windows, Mac, and Linux

### Tips and Best Practices

#### Meaningful Sheet Names

```r
# Good: Descriptive names
list(
  service_line_summary = sl_data,
  top_20_diagnoses = dx_data,
  monthly_trends_2024 = trend_data
)

# Avoid: Generic names
list(
  data1 = sl_data,
  data2 = dx_data,
  data3 = trend_data
)
```

#### Check File Path

```r
# Capture and display file path
file_path <- save_to_excel(
  .data = my_data,
  .file_name = "report"
)

message("File saved to: ", file_path)
```

#### Format Data Before Export

```r
# Clean up data for Excel
export_data <- summary_data %>%
  mutate(
    # Format percentages
    readmit_rate = scales::percent(readmit_rate, accuracy = 0.1),
    # Format currency
    avg_charges = scales::dollar(avg_charges),
    # Round numbers
    avg_los = round(avg_los, 1)
  ) %>%
  # Rename columns for readability
  rename(
    `Service Line` = service_line,
    `Encounters` = encounters,
    `Avg LOS` = avg_los
  )

save_to_excel(.data = export_data, .file_name = "formatted_report")
```

### Common Issues

#### Issue: File Won't Open in Excel

**Solution**: Ensure data doesn't contain problematic types:
```r
# Check data types
str(my_data)

# Convert problematic columns
my_data_clean <- my_data %>%
  mutate(
    # Convert dates
    date_col = as.character(date_col),
    # Convert lists to strings
    list_col = sapply(list_col, paste, collapse = ", ")
  )
```

#### Issue: Sheet Names Too Long

**Solution**: Excel has a 31-character limit for sheet names:
```r
# Truncate long names
report_data <- list(
  exec_summary = data1,  # Not "executive_summary_for_board_meeting"
  svc_line = data2        # Not "service_line_detailed_analysis"
)
```

#### Issue: File Permission Error

**Solution**: Close the Excel file if it's open, or change the filename

### Alternatives for Advanced Excel Formatting

If you need advanced Excel formatting (colors, formulas, charts), consider:

```r
# Option 1: openxlsx package
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", my_data)
saveWorkbook(wb, "formatted_report.xlsx")

# Option 2: Export as CSV
readr::write_csv(my_data, "report.csv")
```

### See Also

- [named_item_list()](Data-Transformation-Functions#named_item_list) - Create named lists
- [category_counts_tbl()](Data-Transformation-Functions#category_counts_tbl) - Summary tables

---

## opt_bin()

Calculate optimal bin size for histograms using Freedman-Diaconis rule.

### Description

Determines the optimal number of bins for a histogram based on the Freedman-Diaconis rule, which considers the interquartile range and sample size.

### Usage

```r
opt_bin(x)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `x` | numeric vector | The data for which to calculate optimal bin size |

### Returns

A single numeric value representing the optimal bin width.

### Formula

The Freedman-Diaconis rule calculates bin width as:

```
bin_width = 2 * IQR(x) / n^(1/3)
```

Where:
- `IQR(x)` = Interquartile range (Q3 - Q1)
- `n` = Number of observations
- Result gives the width, not the number of bins

### Examples

#### Basic Usage

```r
library(healthyR)

# Calculate optimal bin width for length of stay data
los_data <- hospital_data$los

optimal_width <- opt_bin(los_data)
print(optimal_width)
```

#### Create Histogram with Optimal Bins

```r
library(ggplot2)
library(healthyR)

# Get optimal bin width
bin_width <- opt_bin(hospital_data$los)

# Create histogram
ggplot(hospital_data, aes(x = los)) +
  geom_histogram(binwidth = bin_width, fill = color_blind()[2], color = "white") +
  theme_minimal() +
  labs(title = "Length of Stay Distribution",
       subtitle = paste("Bin width:", round(bin_width, 2)),
       x = "Length of Stay (Days)",
       y = "Frequency")
```

#### Compare Different Binning Methods

```r
library(ggplot2)
library(patchwork)

# Calculate different bin specifications
fd_width <- opt_bin(hospital_data$los)  # Freedman-Diaconis
sturges_bins <- nclass.Sturges(hospital_data$los)  # Sturges' rule
scott_bins <- nclass.scott(hospital_data$los)  # Scott's rule

# Create comparison plots
p1 <- ggplot(hospital_data, aes(x = los)) +
  geom_histogram(binwidth = fd_width, fill = "steelblue") +
  labs(title = "Freedman-Diaconis", subtitle = paste("Width:", round(fd_width, 2)))

p2 <- ggplot(hospital_data, aes(x = los)) +
  geom_histogram(bins = sturges_bins, fill = "coral") +
  labs(title = "Sturges", subtitle = paste("Bins:", sturges_bins))

p3 <- ggplot(hospital_data, aes(x = los)) +
  geom_histogram(bins = scott_bins, fill = "seagreen") +
  labs(title = "Scott", subtitle = paste("Bins:", scott_bins))

# Combine plots
(p1 | p2 | p3)
```

#### By Service Line

```r
library(dplyr)
library(purrr)
library(ggplot2)

# Calculate optimal bins for each service line
service_line_bins <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    optimal_width = opt_bin(los),
    n = n(),
    .groups = "drop"
  )

print(service_line_bins)

# Create faceted histogram with service-line specific bins
plots <- hospital_data %>%
  split(.$service_line) %>%
  map2(service_line_bins$optimal_width, ~{
    ggplot(.x, aes(x = los)) +
      geom_histogram(binwidth = .y, fill = color_blind()[2]) +
      labs(title = unique(.x$service_line),
           subtitle = paste("Bin width:", round(.y, 2)))
  })
```

### Use Cases

- Creating publication-quality histograms
- Exploratory data analysis
- Distribution analysis
- Outlier detection
- Data quality assessment

### When to Use Freedman-Diaconis

**Advantages:**
- Robust to outliers (uses IQR instead of standard deviation)
- Works well with skewed distributions
- Generally provides better results than Sturges' rule

**Consider Alternatives When:**
- Data is normally distributed (Sturges' or Scott's rule may work)
- You have very few observations (< 30)
- You want to emphasize particular features

### Manual Bin Control

```r
# Sometimes you want to override
# For presentation, you might want round numbers
optimal <- opt_bin(data$value)
rounded_width <- round(optimal)

# Or specific boundaries
ggplot(data, aes(x = value)) +
  geom_histogram(breaks = seq(0, max(data$value), by = 5))
```

### Theory and Background

The Freedman-Diaconis rule is one of several methods for choosing histogram bin width:

| Method | Formula | Best For |
|--------|---------|----------|
| Freedman-Diaconis | `2 * IQR / n^(1/3)` | Skewed data, robust |
| Sturges | `ceiling(log2(n) + 1)` | Normal distribution |
| Scott | `3.5 * sd / n^(1/3)` | Normal distribution |
| Square Root | `sqrt(n)` | Quick approximation |

### See Also

- Base R: `nclass.FD()`, `nclass.Sturges()`, `nclass.scott()`
- ggplot2: `geom_histogram()`, `geom_density()`

---

## SQL String Functions

healthyR provides SQL-style string manipulation functions for users familiar with SQL syntax.

### sql_left()

Extract characters from the left side of a string.

#### Description

Returns a specified number of characters from the beginning (left side) of a string, similar to SQL's `LEFT()` function.

#### Usage

```r
sql_left(x, n)
```

#### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `x` | character vector | The string(s) to extract from |
| `n` | numeric | Number of characters to extract from the left |

#### Returns

A character vector of the same length as `x`.

#### Examples

```r
library(healthyR)

# Extract first 3 characters
sql_left("Hello World", 3)
# [1] "Hel"

# Extract DRG family (first 3 digits)
drg_data <- data.frame(
  drg = c("291", "292", "293", "470", "471")
)

drg_data$drg_family <- sql_left(drg_data$drg, 2)

# Extract ICD-10 category
dx_data <- data.frame(
  dx_code = c("I50.9", "I50.1", "J96.00", "F10.10")
)

dx_data$category <- sql_left(dx_data$dx_code, 3)
```

---

### sql_right()

Extract characters from the right side of a string.

#### Description

Returns a specified number of characters from the end (right side) of a string, similar to SQL's `RIGHT()` function.

#### Usage

```r
sql_right(x, n)
```

#### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `x` | character vector | The string(s) to extract from |
| `n` | numeric | Number of characters to extract from the right |

#### Returns

A character vector of the same length as `x`.

#### Examples

```r
library(healthyR)

# Extract last 3 characters
sql_right("Hello World", 3)
# [1] "rld"

# Extract ICD-10 subcategory
dx_data <- data.frame(
  dx_code = c("I50.9", "I50.1", "J96.00")
)

dx_data$subcategory <- sql_right(dx_data$dx_code, 1)

# Extract year from date string
dates <- c("2024-01-15", "2024-02-20", "2024-03-10")
years <- sql_right(dates, 4)  # Gets "2024" (from right)
# Note: For actual dates, use lubridate::year() instead
```

---

### sql_mid()

Extract substring from the middle of a string.

#### Description

Returns a substring from the middle of a string, starting at a specified position, similar to SQL's `SUBSTRING()` or `MID()` function.

#### Usage

```r
sql_mid(x, start, n)
```

#### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `x` | character vector | The string(s) to extract from |
| `start` | numeric | Starting position (1-indexed) |
| `n` | numeric | Number of characters to extract |

#### Returns

A character vector of the same length as `x`.

#### Examples

```r
library(healthyR)

# Extract middle characters
sql_mid("Hello World", 7, 5)
# [1] "World"

# Extract from ICD-10 code
dx_data <- data.frame(
  dx_code = c("I50.9", "J96.00", "F10.10")
)

# Extract main category (characters 1-3)
dx_data$main_category <- sql_mid(dx_data$dx_code, 1, 3)

# Extract subcategory (after the period)
dx_data$sub <- sql_mid(dx_data$dx_code, 5, 2)

# Parse structured codes
patient_ids <- c("MRN-12345-A", "MRN-67890-B", "MRN-11111-C")

patient_numbers <- sql_mid(patient_ids, 5, 5)  # Gets "12345", "67890", "11111"
```

---

### SQL String Functions: Combined Example

```r
library(healthyR)
library(dplyr)

# Parse and analyze ICD-10 codes
icd10_analysis <- hospital_data %>%
  mutate(
    # Extract category (first 3 characters)
    dx_category = sql_left(principal_dx, 3),
    
    # Extract subcategory (after the period)
    dx_subcategory = sql_right(principal_dx, 2),
    
    # Extract chapter (first character)
    dx_chapter = sql_left(principal_dx, 1),
    
    # Extract middle portion
    dx_middle = sql_mid(principal_dx, 2, 2)
  ) %>%
  # Analyze by category
  group_by(dx_category) %>%
  summarise(
    n_encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_encounters))
```

### When to Use SQL String Functions vs stringr

**Use SQL functions when:**
- You're familiar with SQL syntax
- Simple position-based extraction
- Quick one-off operations

**Use stringr package when:**
- Pattern matching with regex
- More complex string operations
- Part of tidyverse workflow

```r
# SQL style
sql_left(x, 3)

# stringr equivalent
stringr::str_sub(x, 1, 3)

# SQL style
sql_mid(x, 4, 2)

# stringr equivalent
stringr::str_sub(x, 4, 5)
```

### See Also

- `stringr` package for advanced string manipulation
- `base::substr()` for base R substring extraction
- `base::substring()` for alternative base R approach

---

## Navigation

- [← Accessibility Features](Accessibility-Features)
- [Data Files Reference →](Data-Files-Reference)
- [View All Functions](Time-Series-and-Plotting-Functions)

---

*Need help with utilities? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
