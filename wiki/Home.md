# Welcome to the healthyR Wiki

<img src="https://raw.githubusercontent.com/spsanderson/healthyR/master/man/figures/logo.png" align="right" width="147" height="170" alt="healthyR logo" />

Welcome to the comprehensive documentation for **healthyR**, an R package designed to streamline hospital data analysis workflows.

## 📚 Quick Navigation

### Getting Started
- [Installation Guide](Installation-Guide.md) - How to install and set up healthyR
- [Quick Start](Quick-Start.md) - Get up and running in 5 minutes
- [Core Concepts](Core-Concepts.md) - Understanding healthyR's approach to hospital data

### Function Reference
- [Time Series & Plotting Functions](Time-Series-and-Plotting-Functions.md)
- [Performance Metrics Functions](Performance-Metrics-Functions.md)
- [Data Transformation Functions](Data-Transformation-Functions.md)
- [Accessibility Features](Accessibility-Features.md)
- [Utility Functions](Utility-Functions.md)

### Tutorials & Examples
- [Average Length of Stay Analysis](Tutorial-ALOS-Analysis.md)
- [Readmission Rate Analysis](Tutorial-Readmission-Analysis.md)
- [Service Line Classification](Tutorial-Service-Line-Classification.md)
- [Performance Dashboard Creation](Tutorial-Performance-Dashboard.md)
- [Census & Capacity Planning](Tutorial-Census-Planning.md)

### Reference Documentation
- [Data Files Reference](Data-Files-Reference.md)
- [API Reference](API-Reference.md)
- [Changelog](Changelog.md)

### Help & Support
- [FAQ](FAQ.md)
- [Troubleshooting](Troubleshooting.md)
- [Contributing Guidelines](Contributing.md)

## 📊 What is healthyR?

healthyR is a comprehensive R package designed to streamline hospital data analysis workflows. It provides a consistent, intuitive framework for analyzing common administrative and clinical data problems, helping healthcare analysts and data scientists quickly generate insights from hospital data.

### Key Features

- **📊 Time Series Analysis**: Advanced tools for analyzing hospital census, length of stay (LOS), readmission rates, and other temporal metrics
- **📈 Visualization**: Ready-to-use plotting functions for common healthcare analytics use cases
- **🏥 Service Line Grouping**: Automated patient classification into service lines based on ICD-10 codes and DRG
- **📉 Performance Metrics**: Calculate and visualize key hospital performance indicators
- **🎨 Accessible Design**: Color-blind friendly palettes and themes for inclusive data visualization
- **🔧 Utility Functions**: Helper functions for data manipulation, Excel export, and SQL-style string operations

### What Problems Does healthyR Solve?

healthyR takes the guesswork out of common hospital data analysis tasks:

- Calculate average length of stay across different time periods and patient populations
- Analyze readmission rates and identify trends
- Create service line classifications from diagnosis and procedure codes
- Generate publication-ready visualizations of hospital metrics
- Perform census and capacity planning analyses
- Identify outliers and excess utilization patterns

## 🚀 Quick Example

```r
library(healthyR)
library(dplyr)

# Load your hospital data and create service line classifications
data_with_service_line <- your_data %>%
  mutate(
    service_line = service_line_vec(
      .data = .,
      .dx_col = principal_dx,
      .px_col = principal_px,
      .drg_col = drg_number
    )
  )

# Visualize average length of stay trends
ts_alos_plt(
  .data = your_data,
  .date_col = discharge_date,
  .value_col = length_of_stay,
  .by_grouping = "month"
)
```

## 📖 Package Information

- **CRAN**: https://cran.r-project.org/package=healthyR
- **GitHub**: https://github.com/spsanderson/healthyR
- **Website**: https://www.spsanderson.com/healthyR/
- **Author**: Steven P. Sanderson II, MPH
- **License**: MIT

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](Contributing.md) for details on:

- Reporting bugs
- Suggesting new features
- Submitting pull requests
- Code of conduct

## 📧 Getting Help

If you encounter issues or have questions:

1. Check the [FAQ](FAQ.md) page
2. Review the [Troubleshooting](Troubleshooting.md) guide
3. Search [existing issues](https://github.com/spsanderson/healthyR/issues)
4. Open a [new issue](https://github.com/spsanderson/healthyR/issues/new) with a reproducible example

## 📚 Additional Resources

- [Getting Started Vignette](https://www.spsanderson.com/healthyR/articles/getting-started.html)
- [Function Reference](https://www.spsanderson.com/healthyR/reference/index.html)
- [News & Updates](https://www.spsanderson.com/healthyR/news/index.html)

---

*This wiki is maintained by the healthyR development team. Last updated: 2025*
