# Contributing to healthyR

Thank you for your interest in contributing to healthyR! This guide will help you get started.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Pull Requests](#pull-requests)
- [Development Setup](#development-setup)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)

---

## Code of Conduct

This project follows a Code of Conduct. By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

### Our Standards

- Be respectful and inclusive
- Welcome newcomers
- Focus on what is best for the community
- Show empathy towards other community members

See [CODE_OF_CONDUCT.md](https://github.com/spsanderson/healthyR/blob/master/CODE_OF_CONDUCT.md) for details.

---

## How Can I Contribute?

There are many ways to contribute to healthyR:

### 1. Report Bugs

Found a bug? [Open an issue](https://github.com/spsanderson/healthyR/issues/new) with:
- Clear description of the problem
- Steps to reproduce
- Expected vs actual behavior
- Your environment (R version, OS, etc.)

### 2. Suggest Features

Have an idea? [Open an issue](https://github.com/spsanderson/healthyR/issues/new) describing:
- The problem your feature would solve
- How it would work
- Example use cases
- Why it benefits healthyR users

### 3. Improve Documentation

Documentation can always be better! You can:
- Fix typos or clarify existing docs
- Add examples
- Write tutorials
- Improve function documentation

### 4. Submit Code

Fix bugs or implement features via pull requests.

### 5. Help Others

- Answer questions in [issues](https://github.com/spsanderson/healthyR/issues)
- Share your healthyR experiences
- Write blog posts or tutorials

---

## Reporting Bugs

### Before Submitting

1. **Check existing issues:** Someone may have already reported it
2. **Try the latest version:** Bug might be fixed already
3. **Create a minimal reproducible example (reprex)**

### Creating a Good Bug Report

Use this template:

```markdown
## Bug Description
A clear description of the bug.

## To Reproduce
Steps to reproduce the behavior:
1. Load data: `data <- ...`
2. Run function: `result <- service_line_vec(...)`
3. See error

## Expected Behavior
What you expected to happen.

## Actual Behavior
What actually happened.

## Reproducible Example
```r
# Minimal code that reproduces the issue
library(healthyR)
data <- tibble(...)
result <- function_call(...)
```

## Environment
- healthyR version: 0.2.2
- R version: 4.3.0
- Operating System: Windows 10

## Additional Context
Any other relevant information.
```

### Creating a Reprex

Use the `reprex` package:

```r
# Install if needed
install.packages("reprex")

# Create reprex
reprex::reprex({
  library(healthyR)
  # Your minimal example here
})

# Paste output into GitHub issue
```

---

## Suggesting Enhancements

### Before Suggesting

1. **Check existing issues:** Your idea might already be proposed
2. **Consider if it fits healthyR's scope:** Hospital data analysis workflows
3. **Think about the API:** How would users interact with it?

### Creating a Good Enhancement Proposal

```markdown
## Feature Description
Clear description of the proposed feature.

## Motivation
What problem does this solve? Why is it needed?

## Proposed API
How would users use this feature?
```r
# Example usage
new_function(
  .data = data,
  .param1 = value1
)
```

## Use Cases
1. Use case 1
2. Use case 2
3. Use case 3

## Alternatives Considered
What alternatives did you consider?

## Additional Context
Any other relevant information.
```

---

## Pull Requests

### Process

1. **Fork the repository**
2. **Create a branch:** `git checkout -b feature/your-feature-name`
3. **Make your changes**
4. **Test your changes**
5. **Document your changes**
6. **Commit:** Use clear commit messages
7. **Push:** `git push origin feature/your-feature-name`
8. **Create Pull Request**

### Pull Request Guidelines

**Before submitting:**
- [ ] Code follows project style
- [ ] All tests pass
- [ ] Documentation is updated
- [ ] NEWS.md is updated (for user-facing changes)
- [ ] Commit messages are clear

**PR Description should include:**
- What the PR does
- Why the change is needed
- Related issue numbers (e.g., "Fixes #123")
- Any breaking changes

### PR Template

```markdown
## Description
Brief description of changes.

## Motivation and Context
Why is this change needed?

## Related Issue
Fixes #(issue number)

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update

## How Has This Been Tested?
Describe the tests you ran.

## Checklist
- [ ] My code follows the style guidelines
- [ ] I have performed a self-review
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] I have updated NEWS.md
```

---

## Development Setup

### Prerequisites

- R (>= 3.3)
- RStudio (recommended)
- Git
- Required R packages:
  - devtools
  - roxygen2
  - testthat
  - All packages listed in DESCRIPTION

### Setup Steps

1. **Fork and clone the repository:**
```bash
git clone https://github.com/YOUR-USERNAME/healthyR.git
cd healthyR
```

2. **Install development dependencies:**
```r
install.packages("devtools")
devtools::install_dev_deps()
```

3. **Load the package:**
```r
devtools::load_all()
```

4. **Run tests:**
```r
devtools::test()
```

5. **Check the package:**
```r
devtools::check()
```

### Development Workflow

```r
# 1. Load package
devtools::load_all()

# 2. Make changes to R files

# 3. Document changes
devtools::document()

# 4. Test changes
devtools::test()

# 5. Check package
devtools::check()

# 6. If all passes, commit and push
```

---

## Coding Standards

### R Style

Follow the [tidyverse style guide](https://style.tidyverse.org/):

**Good:**
```r
# Use meaningful names
calculate_average_los <- function(.data, .date_col, .value_col) {
  .data %>%
    group_by({{ .date_col }}) %>%
    summarise(avg = mean({{ .value_col }}, na.rm = TRUE))
}

# Use <- for assignment
result <- calculate_average_los(data, date, los)

# Consistent spacing
x <- 1 + 2
y <- c(1, 2, 3)
```

**Avoid:**
```r
# Cryptic names
calc_alos <- function(d, dc, vc) { ... }

# Using = for assignment
result = function_call()

# Inconsistent spacing
x<-1+2
y <- c(1,2,3)
```

### Function Design

1. **Use consistent parameter naming:**
   - `.data` for the main data frame
   - `.col_name` for column arguments (unquoted)
   - `.param_name` for other parameters

2. **Use tidyeval properly:**
```r
my_function <- function(.data, .col) {
  col_var <- rlang::enquo(.col)
  
  .data %>%
    filter(!is.na({{ .col }}))
}
```

3. **Include parameter validation:**
```r
my_function <- function(.data, .col) {
  # Check inputs
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame")
  }
  
  # Function logic
  ...
}
```

### Documentation

Use roxygen2 for all exported functions:

```r
#' Function Title
#'
#' @family Function Category
#'
#' @author Your Name
#'
#' @description
#' Brief description of what the function does.
#'
#' @details
#' More detailed explanation of how it works.
#'
#' @param .data The input data frame
#' @param .col Column to use (unquoted)
#'
#' @return Description of what is returned
#'
#' @examples
#' library(healthyR)
#' my_function(.data = mtcars, .col = mpg)
#'
#' @export
my_function <- function(.data, .col) {
  ...
}
```

---

## Testing

### Writing Tests

healthyR uses `testthat` for testing:

```r
# tests/testthat/test-my-function.R

test_that("my_function works with valid input", {
  data <- tibble(x = 1:10, y = 11:20)
  result <- my_function(data, x)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10)
})

test_that("my_function handles missing values", {
  data <- tibble(x = c(1, NA, 3), y = c(4, 5, NA))
  result <- my_function(data, x)
  
  expect_true(!any(is.na(result$x)))
})

test_that("my_function errors with invalid input", {
  expect_error(my_function("not a data frame", x))
})
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-my-function.R")

# Run with coverage
covr::package_coverage()
```

---

## Documentation

### Function Documentation

- Document all exported functions
- Include working examples
- Explain parameters clearly
- Note any side effects or warnings

### Vignettes

For major features, consider writing a vignette:

```r
# Create new vignette
usethis::use_vignette("my-feature")
```

### NEWS.md

Update NEWS.md for user-facing changes:

```markdown
# healthyR (development version)

## New Features
- Added `new_function()` for doing X (#123)

## Bug Fixes
- Fixed issue with `existing_function()` when Y (#124)

## Breaking Changes
- Changed `old_function()` parameter name from `old` to `new` (#125)
```

---

## Getting Help

Need help contributing?

- **Ask in issues:** [Open an issue](https://github.com/spsanderson/healthyR/issues/new)
- **Email maintainer:** See DESCRIPTION file
- **Read R packages book:** [r-pkgs.org](https://r-pkgs.org/)

---

## Recognition

Contributors will be:
- Listed in the package DESCRIPTION
- Acknowledged in release notes
- Part of the healthyR community!

---

## Thank You!

Thank you for contributing to healthyR. Your efforts help make hospital data analysis better for everyone!

---

## Navigation

- [← FAQ](FAQ.md)
- [Troubleshooting →](Troubleshooting.md)
- [Home](Home.md)

---

*Ready to contribute? [Fork the repository](https://github.com/spsanderson/healthyR/fork) and get started!*
