# Troubleshooting Guide

Common problems and their solutions when using healthyR.

## Table of Contents

- [Installation Issues](#installation-issues)
- [Data Loading Problems](#data-loading-problems)
- [Service Line Classification Issues](#service-line-classification-issues)
- [Plotting Problems](#plotting-problems)
- [Performance Issues](#performance-issues)
- [Error Messages](#error-messages)
- [Getting More Help](#getting-more-help)

---

## Installation Issues

### Problem: "package 'healthyR' is not available"

**Solution:**

1. **Check spelling:** It's `healthyR` with capital R
```r
install.packages("healthyR")  # Correct
install.packages("healthyr")  # Wrong
```

2. **Update R:** Some packages require recent R versions
```r
R.version.string  # Check your R version
```

3. **Try different CRAN mirror:**
```r
chooseCRANmirror()  # Select different mirror
install.packages("healthyR")
```

4. **Install from GitHub:**
```r
devtools::install_github("spsanderson/healthyR")
```

### Problem: Dependency installation fails

**Solution:**

Install dependencies manually:
```r
# Install core dependencies
install.packages(c(
  "magrittr", "rlang", "tibble", "timetk", "ggplot2",
  "dplyr", "lubridate", "purrr", "stringr", "writexl",
  "cowplot", "scales", "sqldf", "plotly"
))

# Then install healthyR
install.packages("healthyR")
```

On Linux, may need system libraries:
```bash
# Ubuntu/Debian
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev

# Fedora/CentOS
sudo yum install libcurl-devel openssl-devel libxml2-devel
```

### Problem: "Permission denied" during installation

**Solution:**

- **Windows:** Run RStudio/R as Administrator
- **Mac/Linux:** Create personal library
```r
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
.libPaths()  # Verify new library path
```

---

## Data Loading Problems

### Problem: Data won't load / CSV import issues

**Solution:**

```r
# Method 1: readr (recommended)
library(readr)
data <- read_csv("file.csv", guess_max = 10000)

# Method 2: base R
data <- read.csv("file.csv", stringsAsFactors = FALSE)

# Method 3: data.table (for large files)
library(data.table)
data <- fread("file.csv")
data <- as_tibble(data)

# Check the result
str(data)
head(data)
```

### Problem: Date columns not recognized as dates

**Solution:**

```r
library(lubridate)

# Parse various date formats
data <- data %>%
  mutate(
    date = mdy(date_column),        # "01/15/2024"
    # or
    date = ymd(date_column),        # "2024-01-15"
    # or
    date = mdy_hms(datetime_column) # "01/15/2024 10:30:00"
  )

# Verify
class(data$date)  # Should be "Date" or "POSIXct"
```

### Problem: Column names have spaces or special characters

**Solution:**

```r
library(janitor)

# Clean names automatically
data <- clean_names(data)

# Or manually
names(data) <- make.names(names(data))

# Or rename specific columns
data <- data %>%
  rename(
    discharge_date = `Discharge Date`,
    principal_dx = `Principal Diagnosis`
  )
```

---

## Service Line Classification Issues

### Problem: All patients classified as "other"

**Diagnosis:**
```r
# Check your data
head(data %>% select(principal_dx, principal_px, drg))

# Check data types
str(data %>% select(principal_dx, principal_px, drg))
```

**Common Causes & Solutions:**

1. **Missing period in ICD-10 codes:**
```r
# WRONG
principal_dx = "I509"

# CORRECT
principal_dx = "I50.9"

# Fix it:
data <- data %>%
  mutate(
    principal_dx = ifelse(
      nchar(principal_dx) == 4 & !grepl("\\.", principal_dx),
      paste0(substr(principal_dx, 1, 3), ".", substr(principal_dx, 4, 4)),
      principal_dx
    )
  )
```

2. **DRG as numeric instead of character:**
```r
# Check
class(data$drg)

# Fix
data <- data %>%
  mutate(drg = as.character(drg))
```

3. **ICD-9 codes (not supported):**
```r
# healthyR only supports ICD-10
# Filter to ICD-10 only or convert codes
```

### Problem: Classification doesn't match expectations

**Solution:**

Verify the mapping:
```r
library(healthyR)

# Check what your code maps to
dx_cc_mapping %>%
  filter(ICD_Code == "I50.9", ICD_Ver_Flag == "10")

px_cc_mapping %>%
  filter(ICD_Code == "0SR90J9", ICD_Ver_Flag == "10")

# Check service line assignment order:
# 1. DRG checked first
# 2. Then procedure
# 3. Finally diagnosis
```

### Problem: Can't see how classification was determined

**Solution:**

Manual lookup to understand the logic:
```r
# Create a diagnostic function
check_service_line <- function(dx, px, drg) {
  
  # Check DRG
  drg_match <- # lookup logic here
  
  # Check PX
  px_match <- # lookup logic here
  
  # Check DX
  dx_match <- # lookup logic here
  
  # Return results
  list(
    drg_match = drg_match,
    px_match = px_match,
    dx_match = dx_match
  )
}
```

---

## Plotting Problems

### Problem: Plot doesn't display

**Solution:**

```r
# Explicitly print the plot
p <- ts_alos_plt(...)
print(p)

# In RStudio, check the Plots pane

# Save to file to verify
ggsave("test_plot.png", plot = p, width = 10, height = 6)
```

### Problem: "Error in grid.Call" or plotting device errors

**Solution:**

```r
# Reset graphics device
dev.off()

# Rerun plot
ts_alos_plt(...)

# If using RStudio, restart R session
# Session -> Restart R
```

### Problem: Colors don't look right

**Solution:**

```r
# Ensure color-blind palette is applied
library(ggplot2)

p <- ggplot(...) +
  geom_col(aes(fill = category)) +
  hr_scale_fill_colorblind()  # Add this

# Or for lines/points
p <- ggplot(...) +
  geom_line(aes(color = category)) +
  hr_scale_color_colorblind()  # Add this
```

### Problem: Plot labels are cut off

**Solution:**

```r
# Adjust plot margins
p + theme(
  plot.margin = margin(20, 20, 20, 20)
)

# Or adjust text angle
p + theme(
  axis.text.x = element_text(angle = 45, hjust = 1)
)

# Save with larger dimensions
ggsave("plot.png", width = 12, height = 8)
```

### Problem: Interactive plot doesn't work

**Solution:**

```r
# Ensure plotly is installed
install.packages("plotly")

# Create interactive version
library(plotly)

# Method 1: Use .interactive = TRUE
ts_alos_plt(..., .interactive = TRUE)

# Method 2: Convert ggplot to plotly
p <- ts_alos_plt(..., .interactive = FALSE)
ggplotly(p)
```

---

## Performance Issues

### Problem: Functions are slow with large datasets

**Solution:**

1. **Filter data first:**
```r
# Instead of this
ts_alos_plt(.data = all_data, ...)

# Do this
recent_data <- all_data %>%
  filter(year(discharge_date) >= 2023)

ts_alos_plt(.data = recent_data, ...)
```

2. **Aggregate before plotting:**
```r
# Pre-aggregate
monthly_summary <- data %>%
  group_by(year_month = floor_date(discharge_date, "month")) %>%
  summarise(avg_los = mean(los, na.rm = TRUE))

# Then plot aggregated data
ts_plt(.data = monthly_summary, .date_col = year_month, .value_col = avg_los)
```

3. **Disable time padding:**
```r
# Faster without padding
ts_signature_tbl(
  .data = data,
  .date_col = date,
  .pad_time = FALSE  # Skip padding
)
```

4. **Use data.table for large operations:**
```r
library(data.table)

# Convert to data.table
dt <- as.data.table(large_data)

# Fast operations
dt[, avg_los := mean(los), by = service_line]

# Convert back
data <- as_tibble(dt)
```

### Problem: R runs out of memory

**Solution:**

```r
# Check memory usage
object.size(data)
mem_used()

# Increase memory limit (Windows)
memory.limit(size = 16000)  # 16GB

# Process in chunks
chunk_size <- 100000
n_chunks <- ceiling(nrow(data) / chunk_size)

results <- map_dfr(1:n_chunks, ~{
  start_row <- (.x - 1) * chunk_size + 1
  end_row <- min(.x * chunk_size, nrow(data))
  chunk <- data[start_row:end_row, ]
  
  # Process chunk
  process_chunk(chunk)
})

# Or read/process in chunks
library(readr)
read_csv_chunked("large_file.csv", DataFrameCallback$new(process_chunk), chunk_size = 10000)
```

---

## Error Messages

### "object not found"

**Cause:** Package not loaded or variable doesn't exist

**Solution:**
```r
# Load package
library(healthyR)

# Check object exists
exists("my_object")

# List available objects
ls()
```

### "could not find function"

**Cause:** Package not loaded or function name wrong

**Solution:**
```r
# Load package
library(healthyR)

# Check function name
?service_line_vec  # Opens help

# See all healthyR functions
ls("package:healthyR")
```

### "argument is of length zero"

**Cause:** Variable is empty or NULL

**Solution:**
```r
# Check the variable
print(my_var)
length(my_var)

# Ensure data has rows
nrow(data)

# Check for NULLs
is.null(my_var)
```

### "non-numeric argument to binary operator"

**Cause:** Trying math operation on non-numeric data

**Solution:**
```r
# Check data type
class(data$column)

# Convert to numeric
data <- data %>%
  mutate(column = as.numeric(column))

# Or parse if it's a character
data <- data %>%
  mutate(column = parse_number(column))
```

### "Error in eval: object '.data' not found"

**Cause:** Incorrect use of .data pronoun or tidyeval

**Solution:**
```r
# Don't use .data in place of actual data
# WRONG
service_line_vec(.data = .data, ...)

# CORRECT
service_line_vec(.data = hospital_data, ...)
```

### "Cannot add ggproto objects together"

**Cause:** Trying to combine incompatible ggplot elements

**Solution:**
```r
# Make sure you're using + not %>%
# WRONG
p %>% hr_scale_fill_colorblind()

# CORRECT
p + hr_scale_fill_colorblind()
```

---

## Getting More Help

### Before Asking for Help

1. **Search existing issues:** [GitHub Issues](https://github.com/spsanderson/healthyR/issues)
2. **Check the FAQ:** [FAQ](FAQ)
3. **Review documentation:** Function help files and wiki
4. **Create a reprex:** Minimal reproducible example

### Creating a Good Reprex

```r
# Install reprex package
install.packages("reprex")

# Create reproducible example
reprex::reprex({
  library(healthyR)
  library(dplyr)
  
  # Minimal data that reproduces the issue
  data <- tibble(
    date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
    value = rnorm(365, 100, 10)
  )
  
  # Code that produces the error
  result <- ts_alos_plt(.data = data, .date_col = date, .value_col = value)
})

# Copy output and paste in GitHub issue
```

### Information to Include

When asking for help, provide:

1. **R version:**
```r
R.version.string
```

2. **Package version:**
```r
packageVersion("healthyR")
```

3. **Operating System:**
```r
Sys.info()["sysname"]
```

4. **Complete error message:** Copy the entire error, not just part of it

5. **Reproducible example:** Use `reprex` or minimal data

6. **What you've tried:** List troubleshooting steps already taken

### Where to Ask

- **GitHub Issues:** [Open an issue](https://github.com/spsanderson/healthyR/issues/new)
- **Stack Overflow:** Tag with [r] and [healthyR]
- **RStudio Community:** [community.rstudio.com](https://community.rstudio.com)

---

## Common Fixes Summary

Quick reference for common solutions:

```r
# Package not loading
library(healthyR)

# Dates not working
data$date <- as.Date(data$date)

# Service line all "other"
data$drg <- as.character(data$drg)
data$principal_dx <- gsub("^([A-Z][0-9]{2})([0-9])", "\\1.\\2", data$principal_dx)

# Plot not showing
print(plot_object)

# Memory issues
rm(large_object)
gc()

# Reset graphics
dev.off()

# Restart R (in RStudio)
# Session -> Restart R
```

---

## Navigation

- [← Contributing](Contributing)
- [Changelog →](Changelog)
- [Home](Home)

---

*Still having issues? [Open an issue](https://github.com/spsanderson/healthyR/issues/new) with a reproducible example.*
