# Installation Guide

This guide will help you install healthyR and its dependencies on your system.

## Table of Contents

- [System Requirements](#system-requirements)
- [Installing from CRAN](#installing-from-cran)
- [Installing Development Version](#installing-development-version)
- [Dependencies](#dependencies)
- [Verifying Installation](#verifying-installation)
- [Troubleshooting](#troubleshooting)

## System Requirements

healthyR requires:

- **R**: Version 3.3 or higher
- **Operating System**: Windows, macOS, or Linux
- **Memory**: At least 4GB RAM recommended for large datasets

## Installing from CRAN

The easiest way to install healthyR is from CRAN (Comprehensive R Archive Network). This ensures you get the stable, released version.

### Basic Installation

```r
install.packages("healthyR")
```

### Loading the Package

After installation, load the package:

```r
library(healthyR)
```

You should see a startup message with version information.

## Installing Development Version

To get the latest features and bug fixes before they are released on CRAN, you can install the development version from GitHub.

### Prerequisites

First, install the `devtools` package if you don't have it:

```r
install.packages("devtools")
```

### Install from GitHub

```r
devtools::install_github("spsanderson/healthyR")
```

### Install Specific Version or Branch

To install a specific version:

```r
devtools::install_github("spsanderson/healthyR@v0.2.2")
```

To install from a specific branch:

```r
devtools::install_github("spsanderson/healthyR@dev")
```

## Dependencies

healthyR automatically installs required dependencies, but it's helpful to understand what they are:

### Required Dependencies (Imports)

These packages are automatically installed:

- **magrittr** - Pipe operators for cleaner code
- **rlang** (>= 0.1.2) - Programming tools for tidy evaluation
- **tibble** - Modern data frames
- **timetk** - Time series toolkit
- **ggplot2** - Data visualization
- **dplyr** - Data manipulation
- **lubridate** - Date-time manipulation
- **graphics** - Base R graphics
- **purrr** - Functional programming tools
- **stringr** - String manipulation
- **writexl** - Excel file writing
- **cowplot** - Publication-ready plots
- **scales** - Scale functions for visualization
- **sqldf** - SQL operations on data frames
- **plotly** - Interactive plots

### Suggested Dependencies

These packages are recommended but not required:

- **knitr** - Dynamic report generation
- **rmarkdown** - R Markdown support
- **pacman** - Package management
- **healthyR.data** - Additional healthcare datasets
- **broom** - Tidy model outputs
- **tidyselect** - Select helpers for tidyverse

To install suggested packages:

```r
install.packages(c("knitr", "rmarkdown", "pacman", "broom", "tidyselect"))
```

To install the companion data package:

```r
install.packages("healthyR.data")
```

Or from GitHub:

```r
devtools::install_github("spsanderson/healthyR.data")
```

## Verifying Installation

To verify that healthyR is installed correctly:

### Check Version

```r
packageVersion("healthyR")
```

### Load and Test

```r
library(healthyR)

# Test with a simple function
library(timetk)
result <- ts_signature_tbl(.data = m4_daily, .date_col = date)
print(head(result))
```

If this runs without errors, your installation is successful!

### Check All Functions

```r
# List all exported functions
ls("package:healthyR")
```

### View Package Info

```r
help(package = "healthyR")
```

## Troubleshooting

### Common Issues and Solutions

#### Issue: Package Dependencies Not Installing

**Solution**: Install dependencies manually:

```r
# Install all dependencies at once
install.packages(c(
  "magrittr", "rlang", "tibble", "timetk", "ggplot2", 
  "dplyr", "lubridate", "graphics", "purrr", "stringr", 
  "writexl", "cowplot", "scales", "sqldf", "plotly"
))
```

#### Issue: devtools::install_github() Fails

**Solution**: Try these steps:

1. Update devtools:
```r
install.packages("devtools")
```

2. If you're behind a proxy, configure it:
```r
Sys.setenv(http_proxy = "http://proxy.example.com:8080")
Sys.setenv(https_proxy = "https://proxy.example.com:8080")
```

3. Try using remotes instead:
```r
install.packages("remotes")
remotes::install_github("spsanderson/healthyR")
```

#### Issue: "Package Not Found" Error

**Solution**: 
- Make sure you're connected to the internet
- Check if CRAN is accessible: https://cran.r-project.org/
- Try a different CRAN mirror:
```r
chooseCRANmirror()
```

#### Issue: Compilation Errors on Linux

**Solution**: Install system dependencies first. On Ubuntu/Debian:

```bash
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
```

On Fedora/CentOS:

```bash
sudo yum install libcurl-devel openssl-devel libxml2-devel
```

#### Issue: Permission Denied Errors

**Solution**: 
- On Windows: Run R/RStudio as Administrator
- On macOS/Linux: Check library permissions
```r
.libPaths()  # Check where packages are being installed
```

You might need to create a personal library:
```r
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
```

### Getting More Help

If you continue to experience issues:

1. Check the [FAQ](FAQ.md) page
2. Search [existing issues](https://github.com/spsanderson/healthyR/issues)
3. Open a [new issue](https://github.com/spsanderson/healthyR/issues/new) with:
   - Your R version (`R.version.string`)
   - Your operating system
   - The complete error message
   - A reproducible example

## Keeping healthyR Updated

### Check for Updates

```r
# Check if newer version available on CRAN
old.packages()
```

### Update from CRAN

```r
update.packages("healthyR")
```

### Update Development Version

```r
devtools::install_github("spsanderson/healthyR")
```

## Uninstalling healthyR

If you need to remove healthyR:

```r
remove.packages("healthyR")
```

To remove all dependencies that are no longer needed:

```r
# This requires the 'pak' package
install.packages("pak")
pak::pkg_remove("healthyR", dependencies = TRUE)
```

## Next Steps

After successful installation:

1. Review the [Quick Start](Quick-Start.md) guide
2. Explore [Core Concepts](Core-Concepts.md)
3. Try the [Tutorials](Tutorial-ALOS-Analysis.md)
4. Browse the [Function Reference](Time-Series-and-Plotting-Functions.md)

---

*Having installation issues? Check the [Troubleshooting](Troubleshooting.md) page or [open an issue](https://github.com/spsanderson/healthyR/issues).*
