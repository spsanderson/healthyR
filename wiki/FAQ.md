# Frequently Asked Questions (FAQ)

Common questions and answers about healthyR.

## Table of Contents

- [General Questions](#general-questions)
- [Installation & Setup](#installation--setup)
- [Data Preparation](#data-preparation)
- [Service Line Classification](#service-line-classification)
- [Time Series Analysis](#time-series-analysis)
- [Visualization](#visualization)
- [Performance & Optimization](#performance--optimization)
- [Troubleshooting](#troubleshooting)

---

## General Questions

### What is healthyR?

healthyR is an R package designed for hospital data analysis workflows. It provides functions for:
- Time series analysis of hospital metrics
- Service line classification
- Performance visualization
- Data transformation and reporting

### Who should use healthyR?

- Hospital data analysts
- Healthcare quality analysts
- Health system researchers
- Healthcare consultants
- Anyone working with hospital administrative data

### Is healthyR free?

Yes! healthyR is open source and released under the MIT license. It's free to use for any purpose.

### Where can I get help?

1. Check this FAQ
2. Review the [wiki documentation](Home.md)
3. Search [existing issues](https://github.com/spsanderson/healthyR/issues)
4. Ask a question by [opening a new issue](https://github.com/spsanderson/healthyR/issues/new)

### How do I cite healthyR?

```r
citation("healthyR")
```

---

## Installation & Setup

### How do I install healthyR?

From CRAN (recommended):
```r
install.packages("healthyR")
```

From GitHub (development version):
```r
devtools::install_github("spsanderson/healthyR")
```

See the [Installation Guide](Installation-Guide.md) for details.

### What version of R do I need?

R version 3.3 or higher.

### Do I need other packages?

healthyR automatically installs required dependencies. Optional packages like `healthyR.data` provide additional datasets for practice.

### Can I use healthyR with RStudio?

Yes! healthyR works great with RStudio. All examples in the documentation are compatible with RStudio.

---

## Data Preparation

### What format should my data be in?

healthyR works with tidy data frames where:
- Each row is an observation (patient encounter)
- Each column is a variable
- Data is in a tibble or data.frame

See [Core Concepts: Tidy Data](Core-Concepts.md#tidy-data-principles).

### What columns do I need?

It depends on what you're analyzing:

**For time series analysis:**
- Date column (admission_date, discharge_date, etc.)
- Numeric value column (los, charges, etc.)

**For service line classification:**
- Principal diagnosis (ICD-10-CM format)
- Principal procedure (ICD-10-PCS format)
- DRG number

### How should dates be formatted?

healthyR accepts dates in standard R formats:
- `Date` class: `as.Date("2024-01-01")`
- `POSIXct` datetime
- Character dates (automatically converted)

### How do I handle missing values?

Most healthyR functions handle missing values automatically:
```r
# Plotting functions exclude NAs
ts_alos_plt(.data = data, .date_col = date, .value_col = los)

# Summary functions use na.rm = TRUE
mean(data$los, na.rm = TRUE)
```

For critical columns (like dates), filter beforehand:
```r
data_clean <- data %>% filter(!is.na(discharge_date))
```

### Can I use healthyR with database data?

Yes! Query your database, then use healthyR:
```r
library(DBI)
library(healthyR)

con <- dbConnect(...)
data <- dbGetQuery(con, "SELECT * FROM encounters")
data <- as_tibble(data)

# Now use healthyR functions
```

---

## Service Line Classification

### What ICD version does healthyR support?

Currently, only ICD-10 (ICD-10-CM for diagnoses, ICD-10-PCS for procedures).

### Why are all my patients classified as "other"?

Common causes:

1. **Code format issue:**
```r
# WRONG
principal_dx = "I509"  # Missing period

# CORRECT
principal_dx = "I50.9"  # Has period
```

2. **Wrong column type:**
```r
# Check your data
str(data$drg)  # Should be character, not numeric
```

3. **ICD-9 codes:**
```r
# healthyR only supports ICD-10
# If you have ICD-9, you need to convert or exclude
```

### How do I see what a code maps to?

```r
library(healthyR)

# Check diagnosis
dx_cc_mapping %>%
  filter(ICD_Code == "I50.9", ICD_Ver_Flag == "10")

# Check procedure  
px_cc_mapping %>%
  filter(ICD_Code == "0SR90J9", ICD_Ver_Flag == "10")
```

### Can I customize service line mappings?

The built-in mappings can't be modified, but you can create custom classifications:

```r
data %>%
  mutate(
    custom_sl = case_when(
      service_line %in% c("cardiac", "pulmonary") ~ "Cardiopulmonary",
      service_line == "orthopedics" ~ "Surgical",
      TRUE ~ "Other"
    )
  )
```

### What's the difference between service_line_vec() and service_line_augment()?

- **service_line_vec()**: Returns a vector, use in `mutate()`
- **service_line_augment()**: Returns the full data frame with service_line added

Both produce the same classification.

---

## Time Series Analysis

### What time groupings are supported?

- `"day"` - Daily
- `"week"` - Weekly  
- `"month"` - Monthly
- `"quarter"` - Quarterly
- `"year"` - Yearly

Used in functions like `ts_alos_plt()`:
```r
ts_alos_plt(.data = data, .date_col = date, .value_col = los, .by_grouping = "month")
```

### How do I analyze by fiscal year instead of calendar year?

Add fiscal year before analysis:
```r
data_fiscal <- data %>%
  mutate(
    fiscal_year = if_else(
      month(discharge_date) >= 10,  # FY starts in October
      year(discharge_date) + 1,
      year(discharge_date)
    )
  )
```

### My time series has gaps. What should I do?

Use `.pad_time = TRUE` to fill gaps:
```r
ts_signature_tbl(
  .data = data,
  .date_col = date,
  .pad_time = TRUE,
  patient_id  # Group by if needed
)
```

### How do I compare year-over-year trends?

```r
library(ggplot2)

data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, month) %>%
  summarise(avg_los = mean(los), .groups = "drop") %>%
  ggplot(aes(x = month, y = avg_los, color = factor(year), group = year)) +
  geom_line() +
  geom_point() +
  hr_scale_color_colorblind()
```

---

## Visualization

### Can I customize healthyR plots?

Yes! All plotting functions return ggplot2 objects:
```r
p <- ts_alos_plt(...)

p + 
  labs(title = "My Custom Title") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

### How do I make plots interactive?

Set `.interactive = TRUE`:
```r
ts_alos_plt(
  .data = data,
  .date_col = date,
  .value_col = los,
  .by_grouping = "month",
  .interactive = TRUE  # Creates plotly chart
)
```

### Can I use healthyR colors in my own plots?

Yes:
```r
# Get the color palette
colors <- color_blind()

# Use in ggplot2
ggplot(...) +
  scale_fill_manual(values = color_blind()) +
  # or
  hr_scale_fill_colorblind()
```

### How do I save plots?

```r
# Save ggplot2 plot
p <- ts_alos_plt(...)
ggsave("my_plot.png", plot = p, width = 10, height = 6)

# Save interactive plot
p_interactive <- ts_alos_plt(..., .interactive = TRUE)
htmlwidgets::saveWidget(p_interactive, "my_plot.html")
```

### Why do my plots look different from the examples?

Could be due to:
- Different ggplot2 theme
- Different data
- Screen resolution/size

Try explicitly setting theme:
```r
p + theme_minimal()
```

---

## Performance & Optimization

### healthyR is slow with my large dataset. What can I do?

1. **Filter before processing:**
```r
# Filter first
recent_data <- data %>%
  filter(year(discharge_date) >= 2023)

# Then analyze
ts_alos_plt(.data = recent_data, ...)
```

2. **Aggregate if appropriate:**
```r
# Instead of plotting 1M rows daily, aggregate to monthly
monthly_summary <- data %>%
  group_by(year_month = floor_date(discharge_date, "month")) %>%
  summarise(avg_los = mean(los))
```

3. **Use data.table for very large datasets:**
```r
library(data.table)
dt <- as.data.table(data)
# Process with data.table, then convert back
```

### How much data can healthyR handle?

healthyR itself has no hard limits, but performance depends on:
- Your computer's RAM
- Complexity of operations
- R's limitations

Generally works well up to several million rows.

### Should I use .pad_time = TRUE or FALSE?

- **TRUE**: Fills missing time periods (good for complete time series)
- **FALSE**: Uses only existing dates (faster, good for large datasets)

Default is TRUE. Set to FALSE for better performance if gaps don't matter.

---

## Troubleshooting

### I get "object not found" errors

Common causes:

1. **Package not loaded:**
```r
library(healthyR)  # Load first!
```

2. **Column name typo:**
```r
# Check column names
names(data)
```

3. **Using wrong data object:**
```r
# Make sure your data frame has the correct name
```

### Function returns unexpected results

1. **Check data types:**
```r
str(data)  # Verify column types
```

2. **Look for NAs:**
```r
summary(data)
```

3. **Check for outliers:**
```r
summary(data$los)
boxplot(data$los)
```

### Plots don't display

1. **In RStudio:** Check the "Plots" pane
2. **In R console:** Make sure you have a graphics device
3. **Try explicitly printing:**
```r
p <- ts_alos_plt(...)
print(p)
```

### Error: "argument is of length zero"

Usually means a required column is missing or empty:
```r
# Check your data
head(data)
nrow(data)  # Make sure you have data

# Check specific columns
summary(data$date_col)
```

### Excel export fails

1. **Check file permissions:** Close Excel file if open
2. **Check disk space**
3. **Use different filename**
4. **Check for problematic data types:**
```r
str(data)  # Look for list-columns or other complex types
```

### Getting "package not available" error

1. **Check package name:** `healthyR` (capital R)
2. **Update R:** Some packages need recent R versions
3. **Check CRAN status:** Try different mirror
4. **Try GitHub version:**
```r
devtools::install_github("spsanderson/healthyR")
```

---

## Still Have Questions?

Can't find an answer here?

1. **Search the documentation:** Use the search function on the wiki
2. **Check existing issues:** [GitHub Issues](https://github.com/spsanderson/healthyR/issues)
3. **Ask a question:** [Open a new issue](https://github.com/spsanderson/healthyR/issues/new)
4. **Email the maintainer:** See DESCRIPTION file for contact info

When asking for help, please include:
- Your R version: `R.version.string`
- healthyR version: `packageVersion("healthyR")`
- Reproducible example
- Complete error message
- What you expected vs. what happened

---

## Navigation

- [← Tutorial: Service Line Classification](Tutorial-Service-Line-Classification.md)
- [Contributing Guidelines →](Contributing.md)
- [Home](Home.md)

---

*Have a question not listed here? [Open an issue](https://github.com/spsanderson/healthyR/issues/new) and we'll add it!*
