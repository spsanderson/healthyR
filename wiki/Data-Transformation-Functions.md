# Data Transformation Functions

Comprehensive reference for data manipulation and transformation functions in healthyR.

## Table of Contents

- [service_line_vec()](#service_line_vec)
- [service_line_augment()](#service_line_augment)
- [category_counts_tbl()](#category_counts_tbl)
- [top_n_tbl()](#top_n_tbl)
- [named_item_list()](#named_item_list)

---

## service_line_vec()

Classify patients into service lines (returns a vector).

### Description

A vectorized function that assigns service line classifications to patients based on their ICD-10 diagnosis codes, procedure codes, and DRG numbers. Returns a character vector that can be used within `mutate()` or other dplyr operations.

### Usage

```r
service_line_vec(
  .data,
  .dx_col,
  .px_col,
  .drg_col
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The patient encounter data |
| `.dx_col` | column name (unquoted) | Principal diagnosis code column (ICD-10) |
| `.px_col` | column name (unquoted) | Principal procedure code column (ICD-10) |
| `.drg_col` | column name (unquoted) | DRG number column |

### Returns

A character vector with service line assignments. Possible values include:
- `"alcohol_abuse"`
- `"bariatric_surgery"`
- `"cardiac"`
- `"neurology"`
- `"obstetrics"`
- `"orthopedics"`
- `"pulmonary"`
- `"renal"`
- `"psychiatric"`
- `"other"` (default when no match found)

### Classification Logic

The function uses a hierarchical approach:

1. **Check DRG**: If DRG maps to a service line, use it
2. **Check Procedure**: If principal procedure maps to a service line, use it  
3. **Check Diagnosis**: If principal diagnosis maps to a service line, use it
4. **Default**: Assign to "other" if no match

### Examples

#### Basic Usage with mutate()

```r
library(healthyR)
library(dplyr)

# Simple example
df <- data.frame(
  encounter_id = 1:3,
  dx_col = c("F10.10", "I50.9", "M17.11"),
  px_col = c(NA, NA, "0SR90J9"),
  drg_col = c("896", "291", "470")
)

result <- df %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = dx_col,
      .px_col = px_col,
      .drg_col = drg_col
    )
  )

print(result)
```

Output:
```
  encounter_id dx_col  px_col drg_col   service_line
1            1 F10.10    <NA>     896 alcohol_abuse
2            2  I50.9    <NA>     291       cardiac
3            3 M17.11 0SR90J9     470   orthopedics
```

#### In a Data Pipeline

```r
# Full workflow
hospital_analysis <- hospital_data %>%
  # Add service line
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    )
  ) %>%
  # Add time features
  ts_signature_tbl(.date_col = discharge_date) %>%
  # Summarize by service line and month
  group_by(year, month.lbl, service_line) %>%
  summarise(
    n_encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    total_charges = sum(charges, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter out "other"
  filter(service_line != "other")
```

#### With Multiple Groups

```r
# Classify and analyze by multiple dimensions
service_line_analysis <- hospital_data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    ),
    age_group = case_when(
      age < 18 ~ "Pediatric",
      age < 65 ~ "Adult",
      TRUE ~ "Senior"
    )
  ) %>%
  group_by(service_line, age_group) %>%
  summarise(
    n = n(),
    avg_los = mean(los),
    .groups = "drop"
  )
```

### Use Cases

- Service line reporting and analytics
- Financial analysis by clinical category
- Resource allocation planning
- Quality metrics by specialty
- Strategic planning
- Clinical pathway development

### Notes

- **ICD-10 Only**: This function works only with ICD-10 codes
- **Missing Values**: NA values in diagnosis, procedure, or DRG columns are handled gracefully
- **Performance**: Vectorized for efficient processing of large datasets
- **Extensibility**: Uses reference tables (`dx_cc_mapping` and `px_cc_mapping`) that can be examined

### Troubleshooting

#### Issue: All patients classified as "other"

**Solution**: Check that your codes are in the correct format:
- ICD-10 diagnosis: "I50.9", "F10.10" (include periods)
- ICD-10 procedure: "0SR90J9" (no periods)
- DRG: "291", "896" (as character)

```r
# Check code format
unique(hospital_data$principal_dx) %>% head()
```

#### Issue: Service line seems incorrect

**Solution**: Verify the code mapping:

```r
library(healthyR)

# Check what a specific diagnosis maps to
dx_cc_mapping %>%
  filter(ICD_Code == "F10.10", ICD_Ver_Flag == "10")

# Check what a specific procedure maps to  
px_cc_mapping %>%
  filter(ICD_Code == "0SR90J9", ICD_Ver_Flag == "10")
```

### Reference Tables

The function uses two internal datasets:

- **dx_cc_mapping**: Maps ICD-10 diagnosis codes to condition categories
- **px_cc_mapping**: Maps ICD-10 procedure codes to procedure categories

To explore these mappings:

```r
# View available diagnosis mappings
data(dx_cc_mapping)
head(dx_cc_mapping)

# View available procedure mappings
data(px_cc_mapping)
head(px_cc_mapping)
```

### See Also

- [service_line_augment()](#service_line_augment) - Alternative approach that returns a tibble
- [Data Files Reference](Data-Files-Reference) - Details on mapping tables
- [Tutorial: Service Line Classification](Tutorial-Service-Line-Classification)

---

## service_line_augment()

Add service line classification column to data (returns tibble).

### Description

An augment-style function that adds a service line classification column to your data frame. This is a convenience wrapper around `service_line_vec()` that returns the full augmented tibble.

### Usage

```r
service_line_augment(
  .data,
  .dx_col,
  .px_col,
  .drg_col,
  .drg_type = "ms"
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The patient encounter data |
| `.dx_col` | column name (unquoted) | Principal diagnosis code column |
| `.px_col` | column name (unquoted) | Principal procedure code column |
| `.drg_col` | column name (unquoted) | DRG number column |
| `.drg_type` | character | DRG type: "ms" (Medicare Severity) or "apr" (All Patient Refined). Default: "ms" |

### Returns

The original tibble with an added `service_line` column.

### Examples

#### Basic Usage

```r
library(healthyR)

# Augment data with service line
augmented_data <- service_line_augment(
  .data = hospital_data,
  .dx_col = principal_dx,
  .px_col = principal_px,
  .drg_col = drg,
  .drg_type = "ms"
)

# New column added
head(augmented_data)
```

#### In a Pipeline

```r
library(dplyr)

# Complete analysis workflow
analysis_results <- hospital_data %>%
  service_line_augment(
    .dx_col = principal_dx,
    .px_col = principal_px,
    .drg_col = drg
  ) %>%
  filter(!is.na(service_line), service_line != "other") %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    total_revenue = sum(net_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))
```

### Use Cases

- One-step service line classification
- Cleaner code compared to mutate() approach
- Batch processing of encounter data
- Report generation

### Comparison: augment() vs vec()

#### Use `service_line_augment()` when:
- You want to add service line as a new column in one step
- You're working with a single dataset
- You prefer a simpler, more explicit approach

#### Use `service_line_vec()` when:
- You need more control over the column name
- You want to use it within complex mutate() operations
- You're creating multiple calculated columns simultaneously

```r
# service_line_augment() approach
data1 <- service_line_augment(.data = data, .dx_col = dx, .px_col = px, .drg_col = drg)

# service_line_vec() approach (equivalent)
data2 <- data %>%
  mutate(service_line = service_line_vec(.data = ., .dx_col = dx, .px_col = px, .drg_col = drg))
```

### See Also

- [service_line_vec()](#service_line_vec) - Vectorized version
- [Tutorial: Service Line Classification](Tutorial-Service-Line-Classification)

---

## category_counts_tbl()

Get frequency counts of categorical variables.

### Description

A convenience function for calculating frequency counts of categorical variables, with optional grouping and sorting.

### Usage

```r
category_counts_tbl(
  .data,
  .count_col,
  .grouping_col = NULL,
  .arrange_value = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data to analyze |
| `.count_col` | column name (unquoted) | The categorical column to count |
| `.grouping_col` | column names (unquoted) | Optional grouping columns. Use `c()` for multiple columns |
| `.arrange_value` | logical | If TRUE, sorts by count (descending). Default: FALSE |

### Returns

A tibble with columns for the category, optional grouping variables, and a count column named `N`.

### Examples

#### Simple Counts

```r
library(healthyR)

# Count by service line
service_line_counts <- category_counts_tbl(
  .data = hospital_data,
  .count_col = service_line,
  .arrange_value = TRUE
)

print(service_line_counts)
```

Output:
```
  service_line      N
  <chr>         <int>
1 cardiac        1250
2 orthopedics    1180
3 pulmonary       950
4 other           820
```

#### With Grouping

```r
# Count by service line and year
yearly_counts <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  category_counts_tbl(
    .count_col = service_line,
    .grouping_col = year,
    .arrange_value = TRUE
  )
```

#### Multiple Grouping Variables

```r
# Count by service line, year, and quarter
quarterly_counts <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  category_counts_tbl(
    .count_col = service_line,
    .grouping_col = c(year, quarter),
    .arrange_value = FALSE
  )
```

#### Create a Summary Table

```r
# Top diagnoses by service line
top_dx_by_sl <- hospital_data %>%
  category_counts_tbl(
    .count_col = principal_dx,
    .grouping_col = service_line,
    .arrange_value = TRUE
  ) %>%
  group_by(service_line) %>%
  slice_head(n = 5)  # Top 5 per service line
```

#### Visualize Counts

```r
library(ggplot2)

counts <- category_counts_tbl(
  .data = hospital_data,
  .count_col = service_line,
  .arrange_value = TRUE
)

ggplot(counts, aes(x = reorder(service_line, N), y = N)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Encounters by Service Line",
       x = "Service Line", y = "Count")
```

### Use Cases

- Quick frequency tables
- Volume analysis by category
- Cross-tabulation with grouping
- Data exploration and validation
- Report generation

### Notes

- Returns a tibble for easy further manipulation
- The count column is always named `N`
- Handles missing values (counts them as a category)
- Works with any categorical or character column

### Tips

```r
# Calculate percentages
counts %>%
  mutate(pct = N / sum(N) * 100)

# Filter rare categories
counts %>%
  filter(N >= 10)  # Minimum 10 occurrences

# Wide format for reporting
counts %>%
  pivot_wider(names_from = year, values_from = N, values_fill = 0)
```

### See Also

- [top_n_tbl()](#top_n_tbl) - Get top N categories
- [Core Concepts: Tidy Data](Core-Concepts#tidy-data-principles)

---

## top_n_tbl()

Extract top N records by specified criteria.

### Description

Returns the top N most frequent values from a categorical column, optionally within groups.

### Usage

```r
top_n_tbl(
  .data,
  .count_col,
  .n = 10,
  .grouping_col = NULL
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data to analyze |
| `.count_col` | column name (unquoted) | The categorical column to count and rank |
| `.n` | numeric | Number of top records to return. Default: 10 |
| `.grouping_col` | column names (unquoted) | Optional grouping columns |

### Returns

A tibble with the top N categories and their counts.

### Examples

#### Top 10 Diagnoses

```r
library(healthyR)

# Most common diagnoses
top_diagnoses <- top_n_tbl(
  .data = hospital_data,
  .count_col = principal_dx,
  .n = 10
)

print(top_diagnoses)
```

#### Top 5 by Service Line

```r
# Top 5 diagnoses within each service line
top_dx_by_sl <- top_n_tbl(
  .data = hospital_data,
  .count_col = principal_dx,
  .n = 5,
  .grouping_col = service_line
)
```

#### Top Procedures

```r
# Most common procedures (excluding NAs)
top_procedures <- hospital_data %>%
  filter(!is.na(principal_px)) %>%
  top_n_tbl(
    .count_col = principal_px,
    .n = 20
  )
```

#### Visualize Top N

```r
library(ggplot2)

top_n_tbl(
  .data = hospital_data,
  .count_col = attending_provider,
  .n = 15
) %>%
  ggplot(aes(x = reorder(attending_provider, N), y = N)) +
  geom_col(fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Providers by Volume",
       x = "", y = "Number of Patients")
```

#### By Time Period

```r
# Top DRGs by quarter
quarterly_top_drgs <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  top_n_tbl(
    .count_col = drg,
    .n = 5,
    .grouping_col = c(year, quarter)
  )
```

### Use Cases

- Identify most common codes (DRG, ICD-10)
- Find high-volume providers or locations
- Focus quality improvement efforts
- Priority setting for documentation improvement
- Marketing and strategic planning

### Notes

- Results are automatically sorted by count (descending)
- Ties at the Nth position may result in more than N rows
- Use with grouping to get top N within categories

### Advanced Example: Top N Comparison

```r
# Compare top diagnoses year-over-year
library(dplyr)
library(tidyr)

top_dx_comparison <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(year %in% c(2023, 2024)) %>%
  top_n_tbl(
    .count_col = principal_dx,
    .n = 10,
    .grouping_col = year
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = N,
    names_prefix = "year_",
    values_fill = 0
  ) %>%
  mutate(
    change = year_2024 - year_2023,
    pct_change = (year_2024 / year_2023 - 1) * 100
  ) %>%
  arrange(desc(year_2024))
```

### See Also

- [category_counts_tbl()](#category_counts_tbl) - Get all counts
- [Core Concepts: Tidy Data](Core-Concepts#tidy-data-principles)

---

## named_item_list()

Create named lists for Excel export with multiple sheets.

### Description

Creates a named list from grouped data, useful for exporting multiple data frames to separate Excel sheets with meaningful names.

### Usage

```r
named_item_list(
  .data,
  .group_col
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | The data to split into a list |
| `.group_col` | column name (unquoted) | The grouping column to split by |

### Returns

A named list where each element is a data frame for one group, and names are the group values.

### Examples

#### Basic Usage

```r
library(healthyR)
library(dplyr)

# Group data by service line
grouped_data <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    n = n(),
    avg_los = mean(los, na.rm = TRUE),
    total_charges = sum(charges, na.rm = TRUE)
  )

# Create named list
sl_list <- named_item_list(
  .data = grouped_data,
  .group_col = service_line
)

# Access individual service lines
sl_list$cardiac
sl_list$orthopedics
```

#### Export to Excel with Multiple Sheets

```r
library(healthyR)
library(dplyr)

# Prepare data for each service line
service_line_data <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    encounters = n(),
    avg_los = mean(los, na.rm = TRUE),
    median_los = median(los, na.rm = TRUE),
    total_charges = sum(charges, na.rm = TRUE),
    avg_charges = mean(charges, na.rm = TRUE)
  )

# Create named list
sl_list <- named_item_list(
  .data = service_line_data,
  .group_col = service_line
)

# Export to Excel (each service line gets its own sheet)
save_to_excel(
  .data = sl_list,
  .file_name = "service_line_analysis"
)
```

#### Monthly Reports by Department

```r
# Create monthly reports for each department
monthly_reports <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(year == 2024, month == 10) %>%
  group_by(department) %>%
  summarise(
    volume = n(),
    avg_los = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_flag, na.rm = TRUE),
    avg_charges = mean(total_charges, na.rm = TRUE)
  )

# Create named list by department
dept_list <- named_item_list(
  .data = monthly_reports,
  .group_col = department
)

# Export to Excel
save_to_excel(
  .data = dept_list,
  .file_name = "october_2024_department_reports"
)
```

#### Complex Multi-Sheet Export

```r
library(dplyr)
library(purrr)

# Prepare multiple analyses
analyses <- list(
  # Summary by service line
  service_line = hospital_data %>%
    group_by(service_line) %>%
    summarise(n = n(), avg_los = mean(los)),
  
  # Top diagnoses
  top_dx = top_n_tbl(.data = hospital_data, .count_col = principal_dx, .n = 20),
  
  # Monthly trends
  monthly_trends = hospital_data %>%
    ts_signature_tbl(.date_col = discharge_date) %>%
    group_by(year, month.lbl) %>%
    summarise(volume = n(), .groups = "drop")
)

# Export all analyses
save_to_excel(
  .data = analyses,
  .file_name = "hospital_analysis_report"
)
```

### Use Cases

- Multi-sheet Excel reports
- Batch processing by category
- Automated report distribution
- Data organization for stakeholders
- Iterative analysis by group

### Benefits

1. **Organized Data**: Each group in its own structure
2. **Easy Excel Export**: One call exports multiple sheets
3. **Meaningful Names**: Sheet names match group values
4. **Programmatic Access**: Easy to loop through groups

### Tips

```r
# Inspect list structure
names(sl_list)
length(sl_list)

# Loop through list
map(sl_list, ~{
  # Do something with each data frame
  print(summary(.x))
})

# Combine back into one data frame if needed
bind_rows(sl_list, .id = "service_line")
```

### See Also

- [save_to_excel()](#save_to_excel) - Export to Excel
- [category_counts_tbl()](#category_counts_tbl) - Create grouped summaries
- [Utility Functions](Utility-Functions#save_to_excel)

---

## Navigation

- [← Performance Metrics Functions](Performance-Metrics-Functions)
- [Accessibility Features →](Accessibility-Features)
- [View Tutorials](Tutorial-Service-Line-Classification)

---

*Need help with data transformation? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
