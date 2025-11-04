# Accessibility Features

healthyR includes built-in support for creating accessible, color-blind friendly visualizations.

## Table of Contents

- [Overview](#overview)
- [color_blind()](#color_blind)
- [hr_scale_fill_colorblind()](#hr_scale_fill_colorblind)
- [hr_scale_color_colorblind()](#hr_scale_color_colorblind)
- [Best Practices](#best-practices)

---

## Overview

Approximately 8% of men and 0.5% of women have some form of color vision deficiency. Creating color-blind friendly visualizations ensures your charts and graphs are accessible to all users.

### Why Accessibility Matters

- **Inclusivity**: Ensures everyone can interpret your visualizations
- **Professionalism**: Demonstrates attention to detail and user needs
- **Compliance**: Many organizations require accessible data visualizations
- **Better Communication**: Color-blind friendly palettes are often clearer for everyone

### healthyR's Approach

healthyR provides a carefully selected color palette that:
- Is distinguishable for people with various types of color blindness
- Maintains good contrast and readability
- Works well in both digital and print formats
- Integrates seamlessly with ggplot2

---

## color_blind()

Get a vector of color-blind friendly colors.

### Description

Returns a character vector of hex color codes that are distinguishable for people with color vision deficiencies.

### Usage

```r
color_blind(n = 8)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `n` | numeric | Number of colors to return (1-8). Default: 8 |

### Returns

A character vector of hex color codes.

### Color Palette

The palette includes 8 carefully selected colors:

1. `#000000` - Black
2. `#E69F00` - Orange
3. `#56B4E9` - Sky Blue
4. `#009E73` - Bluish Green
5. `#F0E442` - Yellow
6. `#0072B2` - Blue
7. `#D55E00` - Vermillion
8. `#CC79A7` - Reddish Purple

### Examples

#### Basic Usage

```r
library(healthyR)

# Get all 8 colors
all_colors <- color_blind()
print(all_colors)

# Get first 5 colors
five_colors <- color_blind(n = 5)
```

#### Manual ggplot2 Usage

```r
library(ggplot2)
library(healthyR)

# Use colors manually in a plot
ggplot(hospital_data, aes(x = service_line, y = avg_los, fill = service_line)) +
  geom_col() +
  scale_fill_manual(values = color_blind()) +
  theme_minimal() +
  labs(title = "Average LOS by Service Line")
```

#### Visualize the Palette

```r
library(ggplot2)

# Create a palette visualization
palette_df <- data.frame(
  color = color_blind(),
  position = 1:8,
  name = c("Black", "Orange", "Sky Blue", "Bluish Green", 
           "Yellow", "Blue", "Vermillion", "Reddish Purple")
)

ggplot(palette_df, aes(x = position, y = 1, fill = color)) +
  geom_tile(color = "white", size = 2) +
  geom_text(aes(label = name), vjust = 2.5) +
  scale_fill_identity() +
  theme_void() +
  labs(title = "healthyR Color-Blind Friendly Palette")
```

### Use Cases

- Custom color assignments
- Creating consistent color schemes across multiple plots
- Integrating with non-ggplot2 visualizations
- Defining organization-wide color standards

### Notes

- Colors are based on research by Okabe and Ito (2008)
- Palette is designed to be distinguishable for:
  - Protanopia (red-blind)
  - Deuteranopia (green-blind)
  - Tritanopia (blue-blind)
- Maximum of 8 colors; consider alternative visualization approaches for more categories

### See Also

- [hr_scale_fill_colorblind()](#hr_scale_fill_colorblind) - ggplot2 fill scale
- [hr_scale_color_colorblind()](#hr_scale_color_colorblind) - ggplot2 color scale
- [Best Practices](#best-practices)

---

## hr_scale_fill_colorblind()

Apply color-blind friendly fill colors to ggplot2 plots.

### Description

A ggplot2 scale function that applies the healthyR color-blind friendly palette to fill aesthetics.

### Usage

```r
hr_scale_fill_colorblind(...)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `...` | various | Additional arguments passed to `ggplot2::discrete_scale()` |

### Returns

A ggplot2 scale object.

### Examples

#### Basic Bar Chart

```r
library(healthyR)
library(ggplot2)
library(dplyr)

# Create summary data
service_line_summary <- hospital_data %>%
  group_by(service_line) %>%
  summarise(encounters = n(), .groups = "drop") %>%
  filter(!is.na(service_line))

# Plot with color-blind friendly colors
ggplot(service_line_summary, aes(x = reorder(service_line, encounters), 
                                   y = encounters, 
                                   fill = service_line)) +
  geom_col() +
  hr_scale_fill_colorblind() +  # Apply color-blind palette
  coord_flip() +
  theme_minimal() +
  labs(title = "Encounters by Service Line",
       x = "Service Line", y = "Number of Encounters") +
  theme(legend.position = "none")
```

#### Stacked Bar Chart

```r
library(healthyR)
library(ggplot2)

# Prepare data
monthly_data <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  filter(year == 2024) %>%
  group_by(month.lbl, service_line) %>%
  summarise(encounters = n(), .groups = "drop") %>%
  filter(!is.na(service_line))

# Stacked bar chart
ggplot(monthly_data, aes(x = month.lbl, y = encounters, fill = service_line)) +
  geom_col() +
  hr_scale_fill_colorblind() +
  theme_minimal() +
  labs(title = "Monthly Encounters by Service Line",
       x = "Month", y = "Encounters", fill = "Service Line") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

#### Box Plot

```r
# Distribution of LOS by service line
ggplot(hospital_data, aes(x = service_line, y = los, fill = service_line)) +
  geom_boxplot() +
  hr_scale_fill_colorblind() +
  theme_minimal() +
  labs(title = "Length of Stay Distribution by Service Line",
       x = "Service Line", y = "Length of Stay (Days)") +
  theme(legend.position = "none")
```

#### Area Chart

```r
# Cumulative encounters over time
cumulative_data <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(date, service_line) %>%
  summarise(encounters = n(), .groups = "drop") %>%
  arrange(date) %>%
  group_by(service_line) %>%
  mutate(cumulative = cumsum(encounters))

ggplot(cumulative_data, aes(x = date, y = cumulative, fill = service_line)) +
  geom_area() +
  hr_scale_fill_colorblind() +
  theme_minimal() +
  labs(title = "Cumulative Encounters by Service Line",
       x = "Date", y = "Cumulative Encounters", fill = "Service Line")
```

### Use Cases

- Any ggplot2 visualization using fill aesthetic
- Bar charts, histograms, box plots
- Stacked or grouped visualizations
- Area charts and density plots
- Heatmaps with discrete categories

### Integration with healthyR Functions

Most healthyR plotting functions support adding custom scales:

```r
# Add to healthyR plot
ts_alos_plt(
  .data = hospital_data,
  .date_col = discharge_date,
  .value_col = los,
  .by_grouping = "month"
) +
  hr_scale_fill_colorblind()
```

### See Also

- [hr_scale_color_colorblind()](#hr_scale_color_colorblind) - For color aesthetic
- [color_blind()](#color_blind) - Get raw color values

---

## hr_scale_color_colorblind()

Apply color-blind friendly colors to ggplot2 plots (color aesthetic).

### Description

A ggplot2 scale function that applies the healthyR color-blind friendly palette to color aesthetics (lines, points, text).

### Usage

```r
hr_scale_color_colorblind(...)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `...` | various | Additional arguments passed to `ggplot2::discrete_scale()` |

### Returns

A ggplot2 scale object.

### Examples

#### Line Chart

```r
library(healthyR)
library(ggplot2)
library(dplyr)

# Monthly trends by service line
monthly_trends <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, month, service_line) %>%
  summarise(avg_los = mean(los, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(service_line))

ggplot(monthly_trends, aes(x = month, y = avg_los, 
                             color = service_line, 
                             group = service_line)) +
  geom_line(size = 1) +
  geom_point() +
  hr_scale_color_colorblind() +  # Color-blind friendly line colors
  facet_wrap(~year) +
  theme_minimal() +
  labs(title = "Average LOS Trends by Service Line",
       x = "Month", y = "Average LOS (Days)", color = "Service Line")
```

#### Scatter Plot

```r
# Performance scatter plot
performance_data <- hospital_data %>%
  group_by(service_line, provider) %>%
  summarise(
    avg_los = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(service_line))

ggplot(performance_data, aes(x = avg_los, y = readmit_rate, 
                              color = service_line)) +
  geom_point(size = 3, alpha = 0.7) +
  hr_scale_color_colorblind() +
  theme_minimal() +
  labs(title = "Provider Performance by Service Line",
       x = "Average LOS", y = "Readmission Rate", color = "Service Line")
```

#### Point and Line Combination

```r
# Quarterly volume with markers
quarterly_volume <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, quarter, service_line) %>%
  summarise(volume = n(), .groups = "drop")

ggplot(quarterly_volume, aes(x = quarter, y = volume, 
                              color = service_line, 
                              group = service_line)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  hr_scale_color_colorblind() +
  facet_wrap(~year) +
  theme_minimal() +
  labs(title = "Quarterly Volume by Service Line",
       x = "Quarter", y = "Volume", color = "Service Line")
```

#### Text Labels

```r
# Add labeled points
top_performers <- performance_data %>%
  arrange(desc(readmit_rate)) %>%
  head(10)

ggplot(performance_data, aes(x = avg_los, y = readmit_rate, 
                              color = service_line)) +
  geom_point(alpha = 0.3) +
  geom_text(data = top_performers, 
            aes(label = provider), 
            size = 3, vjust = -0.5) +
  hr_scale_color_colorblind() +
  theme_minimal() +
  labs(title = "Provider Performance (Top 10 Labeled)",
       color = "Service Line")
```

### Use Cases

- Line charts and time series
- Scatter plots
- Point markers
- Text labels with color coding
- Any visualization using color aesthetic (not fill)

### Difference: color vs fill

- **color**: Used for lines, points, text outlines
- **fill**: Used for solid areas (bars, boxes, areas)

```r
# Both can be used together
ggplot(data, aes(x = x, y = y, fill = category, color = category)) +
  geom_col() +
  hr_scale_fill_colorblind() +
  hr_scale_color_colorblind() +
  theme_minimal()
```

### See Also

- [hr_scale_fill_colorblind()](#hr_scale_fill_colorblind) - For fill aesthetic
- [color_blind()](#color_blind) - Get raw color values

---

## Best Practices

### General Guidelines

1. **Always Consider Accessibility**
   - Use color-blind friendly palettes as the default
   - Don't rely solely on color to convey information
   - Add patterns, shapes, or labels when possible

2. **Test Your Visualizations**
   ```r
   # Use colorblindcheck package to simulate
   library(colorblindcheck)
   
   p <- ggplot(...) + hr_scale_fill_colorblind()
   palette_check(color_blind(), plot = TRUE)
   ```

3. **Combine Multiple Visual Cues**
   ```r
   # Good: Color + shape + labels
   ggplot(data, aes(x = x, y = y, color = group, shape = group)) +
     geom_point(size = 3) +
     hr_scale_color_colorblind() +
     geom_text_repel(aes(label = label))
   ```

### Specific Recommendations

#### For Line Charts

```r
# Add point markers and use different line types
ggplot(data, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  hr_scale_color_colorblind() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
```

#### For Bar Charts

```r
# Add value labels and use patterns if needed
library(ggpattern)

ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_col_pattern(aes(pattern = category)) +
  hr_scale_fill_colorblind() +
  geom_text(aes(label = value), vjust = -0.5)
```

#### For Maps and Heatmaps

```r
# Use sequential or diverging palettes from viridis
# For categorical data with limited colors, use healthyR palette
ggplot(data, aes(x = x, y = y, fill = category)) +
  geom_tile() +
  hr_scale_fill_colorblind() +
  theme_minimal()
```

### When You Have More Than 8 Categories

If you have more than 8 categories, consider:

1. **Grouping**: Combine similar categories into broader groups
2. **Filtering**: Show only top N categories, group rest as "Other"
3. **Faceting**: Split into multiple smaller plots
4. **Alternative Visualizations**: Use tables or interactive plots

```r
# Example: Group to top 5 + Other
data_summarized <- data %>%
  mutate(
    category_grouped = case_when(
      category %in% top_5_categories ~ category,
      TRUE ~ "Other"
    )
  )
```

### Accessibility Checklist

- [ ] Use color-blind friendly palette
- [ ] Ensure sufficient contrast (text should be readable)
- [ ] Don't rely on color alone (add labels, shapes, patterns)
- [ ] Test with color-blind simulation tools
- [ ] Provide alternative text descriptions
- [ ] Use meaningful axis labels and titles
- [ ] Consider font size and readability
- [ ] Test in grayscale/print preview

### Resources

- [ColorBrewer](https://colorbrewer2.org/) - Additional color palettes
- [Viz Palette](https://projects.susielu.com/viz-palette) - Test your palette
- [Color Oracle](https://colororacle.org/) - Color blindness simulator
- [WebAIM](https://webaim.org/resources/contrastchecker/) - Contrast checker

---

## Navigation

- [← Data Transformation Functions](Data-Transformation-Functions)
- [Utility Functions →](Utility-Functions)
- [View All Tutorials](Tutorial-ALOS-Analysis)

---

*Questions about accessibility? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
