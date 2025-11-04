# Data Files Reference

Documentation for the reference datasets included with healthyR.

## Table of Contents

- [Overview](#overview)
- [dx_cc_mapping](#dx_cc_mapping)
- [px_cc_mapping](#px_cc_mapping)
- [Usage Examples](#usage-examples)
- [Understanding the Mappings](#understanding-the-mappings)

---

## Overview

healthyR includes two comprehensive reference datasets that map ICD-10 codes to clinical condition and procedure categories. These mappings are based on the Agency for Healthcare Research and Quality (AHRQ) Clinical Classifications Software (CCS).

### Purpose

These datasets enable:
- **Service line classification** - Grouping patients by clinical specialty
- **Condition categorization** - Aggregating diagnoses into meaningful groups
- **Procedure classification** - Grouping procedures by type
- **Research and analytics** - Standardized clinical groupings

### Data Source

The mappings are derived from:
- **AHRQ Clinical Classifications Software (CCS)**
- ICD-10-CM (Diagnosis codes)
- ICD-10-PCS (Procedure codes)

---

## dx_cc_mapping

Diagnosis code to condition category mapping.

### Description

A comprehensive mapping of ICD-10-CM diagnosis codes to clinical condition categories (CC). This dataset is used internally by `service_line_vec()` and `service_line_augment()` to classify patients.

### Structure

```r
data(dx_cc_mapping)
str(dx_cc_mapping)
```

### Columns

| Column | Type | Description |
|--------|------|-------------|
| `ICD_Code` | character | The ICD-10-CM diagnosis code (e.g., "I50.9", "F10.10") |
| `CC_Code` | character | The condition category code (e.g., "DX_660", "DX_108") |
| `CC_Description` | character | Human-readable description of the condition category |
| `ICD_Ver_Flag` | character | ICD version flag ("10" for ICD-10) |
| `Category` | character | High-level clinical category |

### Size

```r
# Check dataset size
nrow(dx_cc_mapping)  # 86,852 rows
ncol(dx_cc_mapping)  # 5 columns

# See first few rows
head(dx_cc_mapping)
```

### Examples

#### View Specific Diagnosis

```r
library(healthyR)
library(dplyr)

# Look up a specific ICD-10 code
dx_cc_mapping %>%
  filter(ICD_Code == "I50.9", ICD_Ver_Flag == "10")

# Output:
#   ICD_Code CC_Code CC_Description              ICD_Ver_Flag Category
#   I50.9    DX_108  Heart failure               10           cardiac
```

#### Find All Codes in a Category

```r
# All diagnosis codes for alcohol abuse
alcohol_codes <- dx_cc_mapping %>%
  filter(CC_Code == "DX_660", ICD_Ver_Flag == "10") %>%
  select(ICD_Code, CC_Description)

head(alcohol_codes, 10)
```

#### Explore Categories

```r
# Count codes by category
dx_cc_mapping %>%
  filter(ICD_Ver_Flag == "10") %>%
  count(Category, sort = TRUE)

# Output:
#   Category              n
#   other              15234
#   cardiac             8567
#   pulmonary           4321
#   orthopedics         3456
#   ...
```

#### Search by Description

```r
# Find all pneumonia-related codes
pneumonia_codes <- dx_cc_mapping %>%
  filter(
    ICD_Ver_Flag == "10",
    grepl("pneumonia", CC_Description, ignore.case = TRUE)
  ) %>%
  distinct(ICD_Code, CC_Description)

head(pneumonia_codes)
```

### Common Condition Categories

Some frequently used CC codes:

| CC_Code | Category | Description |
|---------|----------|-------------|
| DX_108 | Cardiac | Heart failure |
| DX_109 | Cardiac | Acute myocardial infarction |
| DX_122 | Pulmonary | Pneumonia |
| DX_127 | Pulmonary | COPD |
| DX_660 | Alcohol Abuse | Alcohol abuse and dependence |
| DX_180 | Obstetrics | Live birth |
| DX_203 | Orthopedics | Osteoarthritis |

---

## px_cc_mapping

Procedure code to procedure category mapping.

### Description

A comprehensive mapping of ICD-10-PCS procedure codes to clinical procedure categories. Used for surgical and procedural service line classification.

### Structure

```r
data(px_cc_mapping)
str(px_cc_mapping)
```

### Columns

| Column | Type | Description |
|--------|------|-------------|
| `ICD_Code` | character | The ICD-10-PCS procedure code (e.g., "0SR90J9", "021209W") |
| `CC_Code` | character | The procedure category code (e.g., "PX_108", "PX_152") |
| `CC_Description` | character | Human-readable description of the procedure category |
| `ICD_Ver_Flag` | character | ICD version flag ("10" for ICD-10) |
| `Category` | character | High-level clinical category |

### Size

```r
# Check dataset size
nrow(px_cc_mapping)  # 79,721 rows
ncol(px_cc_mapping)  # 5 columns

# See first few rows
head(px_cc_mapping)
```

### Examples

#### View Specific Procedure

```r
library(healthyR)
library(dplyr)

# Look up a specific procedure code
px_cc_mapping %>%
  filter(ICD_Code == "0SR90J9", ICD_Ver_Flag == "10")

# Output:
#   ICD_Code CC_Code CC_Description                    ICD_Ver_Flag Category
#   0SR90J9  PX_152  Hip replacement, total and partial 10          orthopedics
```

#### Find All Procedures in Category

```r
# All cardiac procedures
cardiac_procedures <- px_cc_mapping %>%
  filter(Category == "cardiac", ICD_Ver_Flag == "10") %>%
  distinct(CC_Code, CC_Description) %>%
  arrange(CC_Description)

head(cardiac_procedures, 10)
```

#### Search Procedures by Type

```r
# Find all joint replacement procedures
joint_replacements <- px_cc_mapping %>%
  filter(
    ICD_Ver_Flag == "10",
    grepl("replacement", CC_Description, ignore.case = TRUE)
  ) %>%
  distinct(ICD_Code, CC_Description, Category)

head(joint_replacements)
```

### Common Procedure Categories

Some frequently used PX codes:

| PX_Code | Category | Description |
|---------|----------|-------------|
| PX_45 | Cardiac | Coronary artery bypass graft (CABG) |
| PX_47 | Cardiac | Diagnostic cardiac catheterization |
| PX_152 | Orthopedics | Hip replacement |
| PX_153 | Orthopedics | Knee replacement |
| PX_167 | Bariatric | Bariatric surgery |
| PX_134 | Obstetrics | Cesarean section |

---

## Usage Examples

### In Service Line Classification

```r
library(healthyR)
library(dplyr)

# The service line functions use these mappings internally
hospital_data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg
    )
  )

# Behind the scenes, this:
# 1. Looks up principal_dx in dx_cc_mapping
# 2. Looks up principal_px in px_cc_mapping
# 3. Uses the Category column to assign service line
```

### Manual Mapping

You can use these datasets directly for custom classifications:

```r
# Custom diagnosis grouping
my_data_with_category <- hospital_data %>%
  left_join(
    dx_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10") %>%
      select(ICD_Code, CC_Description, Category),
    by = c("principal_dx" = "ICD_Code")
  ) %>%
  rename(
    dx_category = Category,
    dx_description = CC_Description
  )
```

### Create Custom Service Lines

```r
# Define your own service line logic
custom_service_line <- hospital_data %>%
  left_join(
    dx_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10") %>%
      select(ICD_Code, Category),
    by = c("principal_dx" = "ICD_Code")
  ) %>%
  mutate(
    custom_service_line = case_when(
      Category %in% c("cardiac") ~ "Cardiology",
      Category %in% c("orthopedics") ~ "Orthopedics",
      Category %in% c("pulmonary") ~ "Pulmonology",
      !is.na(Category) ~ "Other Medical",
      TRUE ~ "Unclassified"
    )
  )
```

### Validate Your Data

```r
# Check what percentage of your codes are mapped
validation_summary <- hospital_data %>%
  left_join(
    dx_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10") %>%
      distinct(ICD_Code),
    by = c("principal_dx" = "ICD_Code")
  ) %>%
  summarise(
    total_encounters = n(),
    mapped_encounters = sum(!is.na(ICD_Code)),
    pct_mapped = mean(!is.na(ICD_Code)) * 100
  )

print(validation_summary)
```

### Analyze Code Frequency

```r
# Most common diagnoses with their categories
top_dx_with_category <- hospital_data %>%
  left_join(
    dx_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10"),
    by = c("principal_dx" = "ICD_Code")
  ) %>%
  count(principal_dx, CC_Description, Category, sort = TRUE) %>%
  head(20)

print(top_dx_with_category)
```

---

## Understanding the Mappings

### Hierarchical Structure

The mappings follow a hierarchical structure:

```
Category (e.g., "cardiac")
  └─ CC_Code (e.g., "DX_108")
      └─ ICD_Codes (e.g., "I50.9", "I50.1", "I50.20")
```

### Many-to-One Relationship

- **Many ICD codes** → **One CC Code**
- **Many CC Codes** → **One Category**

Example:
```r
# Multiple ICD-10 codes map to the same CC
dx_cc_mapping %>%
  filter(CC_Code == "DX_108") %>%
  nrow()  # Many codes for "Heart failure"

# Multiple CCs map to same category
dx_cc_mapping %>%
  filter(Category == "cardiac") %>%
  distinct(CC_Code) %>%
  nrow()  # Many CC codes for "cardiac"
```

### Version Flags

Always filter by `ICD_Ver_Flag == "10"` for ICD-10 codes:

```r
# Correct
dx_cc_mapping %>%
  filter(ICD_Ver_Flag == "10")

# The datasets may contain historical ICD-9 codes
# Always specify version
```

### Missing or Unmapped Codes

Not all possible ICD-10 codes are in the mapping:

```r
# Check if a code exists
check_code <- function(code, type = "dx") {
  mapping <- if(type == "dx") dx_cc_mapping else px_cc_mapping
  
  result <- mapping %>%
    filter(ICD_Code == code, ICD_Ver_Flag == "10")
  
  if(nrow(result) == 0) {
    message("Code '", code, "' not found in mapping")
    return(NULL)
  } else {
    return(result)
  }
}

check_code("I50.9", "dx")
check_code("NOTACODE", "dx")  # Will message that it's not found
```

### Categories Available

```r
# List all available categories
unique_categories <- dx_cc_mapping %>%
  filter(ICD_Ver_Flag == "10") %>%
  distinct(Category) %>%
  arrange(Category) %>%
  pull(Category)

print(unique_categories)
```

### Updating the Mappings

The mappings are static datasets included with the package. If you need updated mappings:

1. Contact the package maintainer
2. Use a newer version of healthyR
3. Create your own custom mapping for new codes

---

## Advanced Topics

### Merging Both Mappings

```r
# Combine diagnosis and procedure classifications
complete_classification <- hospital_data %>%
  # Add diagnosis category
  left_join(
    dx_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10") %>%
      select(ICD_Code, dx_category = Category, dx_desc = CC_Description),
    by = c("principal_dx" = "ICD_Code")
  ) %>%
  # Add procedure category
  left_join(
    px_cc_mapping %>% 
      filter(ICD_Ver_Flag == "10") %>%
      select(ICD_Code, px_category = Category, px_desc = CC_Description),
    by = c("principal_px" = "ICD_Code")
  ) %>%
  # Create combined service line
  mutate(
    combined_service_line = coalesce(px_category, dx_category, "other")
  )
```

### Export Mappings for Reference

```r
# Create reference guides
dx_reference <- dx_cc_mapping %>%
  filter(ICD_Ver_Flag == "10") %>%
  arrange(Category, CC_Code, ICD_Code)

px_reference <- px_cc_mapping %>%
  filter(ICD_Ver_Flag == "10") %>%
  arrange(Category, CC_Code, ICD_Code)

# Export to Excel
library(writexl)
write_xlsx(
  list(
    dx_mappings = dx_reference,
    px_mappings = px_reference
  ),
  "ICD10_Category_Mappings.xlsx"
)
```

---

## Navigation

- [← Utility Functions](Utility-Functions)
- [Tutorial: Service Line Classification →](Tutorial-Service-Line-Classification)
- [Home](Home)

---

*Questions about the data files? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
