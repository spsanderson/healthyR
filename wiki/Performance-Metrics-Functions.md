# Performance Metrics Functions

Comprehensive reference for performance analysis and visualization functions in healthyR.

## Table of Contents

- [los_ra_index_summary_tbl()](#los_ra_index_summary_tbl)
- [los_ra_index_plt()](#los_ra_index_plt)
- [gartner_magic_chart_plt()](#gartner_magic_chart_plt)
- [diverging_bar_plt()](#diverging_bar_plt)
- [diverging_lollipop_plt()](#diverging_lollipop_plt)

---

## los_ra_index_summary_tbl()

Calculate Length of Stay and Readmission performance indices.

### Description

Creates a summary table combining LOS and readmission metrics into performance indices, useful for identifying high-risk service lines or providers.

### Usage

```r
los_ra_index_summary_tbl(
  .data,
  .max_los,
  .alos,
  .readmit_rate
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Summary data with LOS and readmission rates by category |
| `.max_los` | numeric | Maximum LOS threshold for index calculation |
| `.alos` | column name (unquoted) | Column containing average length of stay |
| `.readmit_rate` | column name (unquoted) | Column containing readmission rate (as decimal, e.g., 0.15 for 15%) |

### Returns

A tibble with added index columns:
- `los_index` - Standardized LOS performance metric
- `readmit_index` - Standardized readmission performance metric
- `performance_index` - Combined performance score

### Index Calculation

The indices are calculated as:

- **LOS Index**: `(ALOS / Max_LOS) * 100`
- **Readmit Index**: `Readmission_Rate * 100`
- **Performance Index**: `(LOS_Index + Readmit_Index) / 2`

Lower index values indicate better performance.

### Examples

#### Basic Usage

```r
library(healthyR)
library(dplyr)

# Calculate service line metrics
sl_summary <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    n_encounters = n(),
    alos = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_flag, na.rm = TRUE)
  )

# Calculate indices
index_data <- los_ra_index_summary_tbl(
  .data = sl_summary,
  .max_los = 15,
  .alos = alos,
  .readmit_rate = readmit_rate
)

print(index_data)
```

#### By Provider

```r
# Analyze provider performance
provider_indices <- hospital_data %>%
  group_by(attending_provider) %>%
  summarise(
    n_cases = n(),
    alos = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_30d, na.rm = TRUE)
  ) %>%
  filter(n_cases >= 30) %>%  # Minimum volume threshold
  los_ra_index_summary_tbl(
    .max_los = 10,
    .alos = alos,
    .readmit_rate = readmit_rate
  ) %>%
  arrange(performance_index)

# Top performers
head(provider_indices, 10)

# Need improvement
tail(provider_indices, 10)
```

#### Quarterly Trends

```r
# Track performance over time
quarterly_performance <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, quarter, service_line) %>%
  summarise(
    alos = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_30d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  los_ra_index_summary_tbl(
    .max_los = 12,
    .alos = alos,
    .readmit_rate = readmit_rate
  )

# Visualize trend
library(ggplot2)
quarterly_performance %>%
  filter(service_line == "cardiac") %>%
  ggplot(aes(x = quarter, y = performance_index, color = factor(year))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Cardiac Service Line Performance Index",
       y = "Performance Index (Lower is Better)")
```

### Use Cases

- Service line performance comparison
- Provider profiling and benchmarking
- Quality improvement tracking
- Executive dashboards
- Pay-for-performance programs

### Interpretation

- **Low Index (< 50)**: Excellent performance - short LOS and low readmissions
- **Medium Index (50-75)**: Average performance
- **High Index (> 75)**: Needs improvement - high LOS and/or high readmissions

### Notes

- Choose `.max_los` based on your hospital's typical LOS patterns
- Consider case-mix adjustment for fair comparisons
- Minimum volume thresholds recommended for provider-level analysis
- Readmission rates should be decimal (0.15 = 15%)

### See Also

- [los_ra_index_plt()](#los_ra_index_plt) - Visualize the indices
- [gartner_magic_chart_plt()](#gartner_magic_chart_plt) - Quadrant analysis
- [Tutorial: Performance Dashboard](Tutorial-Performance-Dashboard)

---

## los_ra_index_plt()

Visualize LOS and readmission performance indices.

### Description

Creates a visualization of the LOS/Readmission performance indices, typically as a scatter or bar plot.

### Usage

```r
los_ra_index_plt(.data)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Output from `los_ra_index_summary_tbl()` |

### Returns

A `ggplot2` object.

### Examples

#### Basic Plot

```r
library(healthyR)

# Calculate indices
index_data <- los_ra_index_summary_tbl(
  .data = summary_data,
  .max_los = 15,
  .alos = avg_los,
  .readmit_rate = readmit_pct
)

# Plot
los_ra_index_plt(.data = index_data)
```

#### Customized Plot

```r
library(ggplot2)

# Create base plot
p <- los_ra_index_plt(.data = index_data)

# Customize
p +
  labs(title = "Service Line Performance Analysis",
       subtitle = "Q4 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )
```

### Use Cases

- Executive presentations
- Quality committee reports
- Service line reviews
- Benchmarking reports

### See Also

- [los_ra_index_summary_tbl()](#los_ra_index_summary_tbl) - Calculate the indices
- [gartner_magic_chart_plt()](#gartner_magic_chart_plt) - Alternative visualization

---

## gartner_magic_chart_plt()

Create Gartner Magic Quadrant-style performance charts.

### Description

Generates a quadrant chart comparing two continuous metrics, with customizable labels for each quadrant. Useful for strategic analysis and performance positioning.

### Usage

```r
gartner_magic_chart_plt(
  .data,
  .x_col,
  .y_col,
  .point_size_col = NULL,
  .x_lab = "",
  .y_lab = "",
  .plot_title = "",
  .top_right_label = "",
  .top_left_label = "",
  .bottom_right_label = "",
  .bottom_left_label = ""
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Data with metrics to plot |
| `.x_col` | column name (unquoted) | Column for x-axis |
| `.y_col` | column name (unquoted) | Column for y-axis |
| `.point_size_col` | column name (unquoted) | Optional column to size points. Default: NULL |
| `.x_lab` | character | X-axis label. Default: "" |
| `.y_lab` | character | Y-axis label. Default: "" |
| `.plot_title` | character | Plot title. Default: "" |
| `.top_right_label` | character | Label for top-right quadrant. Default: "" |
| `.top_left_label` | character | Label for top-left quadrant. Default: "" |
| `.bottom_right_label` | character | Label for bottom-right quadrant. Default: "" |
| `.bottom_left_label` | character | Label for bottom-left quadrant. Default: "" |

### Returns

A `ggplot2` object with quadrant divisions at the median of each axis.

### Examples

#### LOS vs Readmission Analysis

```r
library(healthyR)
library(dplyr)

# Calculate service line metrics
sl_metrics <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    avg_los = mean(los, na.rm = TRUE),
    readmit_rate = mean(readmit_30d, na.rm = TRUE),
    volume = n()
  ) %>%
  filter(!is.na(service_line))

# Create Gartner chart
gartner_magic_chart_plt(
  .data = sl_metrics,
  .x_col = avg_los,
  .y_col = readmit_rate,
  .point_size_col = volume,
  .x_lab = "Average Length of Stay (Days)",
  .y_lab = "30-Day Readmission Rate",
  .plot_title = "Service Line Performance Matrix",
  .top_right_label = "High LOS & High Readmits\n(Needs Improvement)",
  .top_left_label = "Low LOS & High Readmits\n(Focus on Quality)",
  .bottom_left_label = "Best Performers\n(Low LOS & Low Readmits)",
  .bottom_right_label = "High LOS & Low Readmits\n(Focus on Efficiency)"
)
```

#### Cost vs Quality

```r
# Quality vs cost analysis
quality_cost <- hospital_data %>%
  group_by(drg) %>%
  summarise(
    avg_cost = mean(total_charges, na.rm = TRUE),
    complication_rate = mean(complication_flag, na.rm = TRUE),
    volume = n()
  ) %>%
  filter(volume >= 20)

gartner_magic_chart_plt(
  .data = quality_cost,
  .x_col = avg_cost,
  .y_col = complication_rate,
  .point_size_col = volume,
  .x_lab = "Average Total Charges ($)",
  .y_lab = "Complication Rate",
  .plot_title = "DRG Performance: Cost vs Quality",
  .top_right_label = "High Cost, High Complications",
  .top_left_label = "Low Cost, High Complications",
  .bottom_left_label = "Optimal: Low Cost, Low Complications",
  .bottom_right_label = "High Cost, Low Complications"
)
```

#### Volume vs Margin

```r
# Financial analysis
financial_perf <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    total_volume = n(),
    profit_margin = mean(net_revenue / total_charges, na.rm = TRUE)
  )

gartner_magic_chart_plt(
  .data = financial_perf,
  .x_col = total_volume,
  .y_col = profit_margin,
  .x_lab = "Total Volume",
  .y_lab = "Profit Margin",
  .plot_title = "Service Line Strategic Position",
  .top_right_label = "Stars\n(High Volume, High Margin)",
  .top_left_label = "Question Marks\n(Low Volume, High Margin)",
  .bottom_left_label = "Dogs\n(Low Volume, Low Margin)",
  .bottom_right_label = "Cash Cows\n(High Volume, Low Margin)"
)
```

#### Without Point Sizing

```r
gartner_magic_chart_plt(
  .data = performance_data,
  .x_col = metric_x,
  .y_col = metric_y,
  .point_size_col = NULL,  # All points same size
  .x_lab = "Efficiency Score",
  .y_lab = "Quality Score",
  .plot_title = "Department Performance"
)
```

### Quadrant Interpretation

The chart divides the plot into four quadrants based on median values:

- **Top Right**: High on both metrics
- **Top Left**: High on Y-axis metric, low on X-axis metric
- **Bottom Left**: Low on both metrics
- **Bottom Right**: High on X-axis metric, low on Y-axis metric

### Use Cases

- Strategic planning and positioning
- Service line portfolio analysis
- Provider performance comparisons
- BCG matrix-style analysis
- Quality vs efficiency trade-offs
- Cost vs quality analysis

### Customization Tips

1. **Meaningful Labels**: Make quadrant labels action-oriented
2. **Point Sizing**: Use volume or importance to size points
3. **Color Coding**: Add color aesthetics for additional dimensions
4. **Interactive Version**: Convert to plotly for interactivity

```r
library(plotly)

p <- gartner_magic_chart_plt(...)
ggplotly(p)
```

### Notes

- Quadrants are divided at the median of each metric
- Consider log-transforming skewed data
- Add text labels for important points:

```r
library(ggrepel)

p <- gartner_magic_chart_plt(...) +
  geom_text_repel(aes(label = service_line), size = 3)
```

### See Also

- [los_ra_index_plt()](#los_ra_index_plt) - Specialized LOS/Readmit visualization
- [diverging_bar_plt()](#diverging_bar_plt) - Show deviations
- [Tutorial: Performance Dashboard](Tutorial-Performance-Dashboard)

---

## diverging_bar_plt()

Create diverging bar charts to show positive and negative deviations.

### Description

Generates a horizontal bar chart where bars extend left (negative) or right (positive) from a central axis, ideal for showing variance from a target or benchmark.

### Usage

```r
diverging_bar_plt(
  .data,
  .x_col,
  .y_col,
  .plot_title = "",
  .x_lab = "",
  .y_lab = ""
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Data with categories and values |
| `.x_col` | column name (unquoted) | Numeric column (deviation values) |
| `.y_col` | column name (unquoted) | Category column |
| `.plot_title` | character | Plot title. Default: "" |
| `.x_lab` | character | X-axis label. Default: "" |
| `.y_lab` | character | Y-axis label. Default: "" |

### Returns

A `ggplot2` object.

### Examples

#### Variance from Target

```r
library(healthyR)
library(dplyr)

# Calculate variance from target
target_alos <- 6.5

variance_data <- hospital_data %>%
  group_by(service_line) %>%
  summarise(
    actual_alos = mean(los, na.rm = TRUE)
  ) %>%
  mutate(
    variance = actual_alos - target_alos,
    service_line = factor(service_line)
  )

# Create diverging bar plot
diverging_bar_plt(
  .data = variance_data,
  .x_col = variance,
  .y_col = service_line,
  .plot_title = "Service Line ALOS Variance from Target",
  .x_lab = "Days Above/Below Target",
  .y_lab = "Service Line"
)
```

#### Year-over-Year Change

```r
# Calculate YoY change
yoy_change <- hospital_data %>%
  ts_signature_tbl(.date_col = discharge_date) %>%
  group_by(year, service_line) %>%
  summarise(volume = n(), .groups = "drop") %>%
  arrange(service_line, year) %>%
  group_by(service_line) %>%
  mutate(
    pct_change = (volume / lag(volume) - 1) * 100
  ) %>%
  filter(year == max(year), !is.na(pct_change))

diverging_bar_plt(
  .data = yoy_change,
  .x_col = pct_change,
  .y_col = service_line,
  .plot_title = "Year-over-Year Volume Change by Service Line",
  .x_lab = "Percent Change (%)",
  .y_lab = ""
)
```

### Use Cases

- Show variance from targets or benchmarks
- Display year-over-year changes
- Compare performance to standards
- Highlight areas above/below expectations

### Notes

- Negative values extend left, positive values extend right
- Bars are automatically colored by direction (typically red for negative, green for positive)
- Sort your data before plotting for better readability

### See Also

- [diverging_lollipop_plt()](#diverging_lollipop_plt) - Alternative visualization
- [gartner_magic_chart_plt()](#gartner_magic_chart_plt) - Two-dimensional comparisons

---

## diverging_lollipop_plt()

Create diverging lollipop charts for variance analysis.

### Description

Similar to diverging bar charts but with a lollipop (circle at the end of a line) style, which can be easier to read with many categories or when space is limited.

### Usage

```r
diverging_lollipop_plt(
  .data,
  .x_col,
  .y_col,
  .plot_title = "",
  .x_lab = "",
  .y_lab = ""
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `.data` | data.frame/tibble | Data with categories and values |
| `.x_col` | column name (unquoted) | Numeric column (deviation values) |
| `.y_col` | column name (unquoted) | Category column |
| `.plot_title` | character | Plot title. Default: "" |
| `.x_lab` | character | X-axis label. Default: "" |
| `.y_lab` | character | Y-axis label. Default: "" |

### Returns

A `ggplot2` object.

### Examples

#### Budget Variance

```r
library(healthyR)

# Budget vs actual
budget_variance <- departments %>%
  mutate(
    variance_pct = (actual_spend / budget_spend - 1) * 100
  )

diverging_lollipop_plt(
  .data = budget_variance,
  .x_col = variance_pct,
  .y_col = department,
  .plot_title = "Department Budget Variance",
  .x_lab = "% Over/Under Budget",
  .y_lab = "Department"
)
```

#### Performance vs Benchmark

```r
# Compare to national benchmark
benchmark_comparison <- hospital_data %>%
  group_by(drg) %>%
  summarise(
    hospital_alos = mean(los, na.rm = TRUE)
  ) %>%
  left_join(national_benchmarks, by = "drg") %>%
  mutate(
    difference = hospital_alos - national_alos
  ) %>%
  arrange(desc(abs(difference))) %>%
  head(20)  # Top 20 differences

diverging_lollipop_plt(
  .data = benchmark_comparison,
  .x_col = difference,
  .y_col = drg,
  .plot_title = "Top 20 DRGs: ALOS vs National Benchmark",
  .x_lab = "Days Above/Below Benchmark"
)
```

### Use Cases

- Budget variance analysis
- Benchmark comparisons
- Performance against targets
- Survey result deviations
- Any metric with positive/negative deviations

### When to Use Lollipop vs Bar

- **Use lollipop when**:
  - You have many categories (> 10)
  - You want a cleaner, less cluttered look
  - Precise endpoint values are most important

- **Use bar when**:
  - You have fewer categories
  - You want to emphasize magnitude
  - You need maximum visual impact

### See Also

- [diverging_bar_plt()](#diverging_bar_plt) - Bar chart version
- [gartner_magic_chart_plt()](#gartner_magic_chart_plt) - Quadrant analysis

---

## Navigation

- [← Time Series Functions](Time-Series-and-Plotting-Functions)
- [Data Transformation Functions →](Data-Transformation-Functions)
- [View Tutorials](Tutorial-Performance-Dashboard)

---

*Need help with performance metrics? Check the [FAQ](FAQ) or [open an issue](https://github.com/spsanderson/healthyR/issues).*
