# Service Line Grouper Vectorized Function

Takes a few arguments from a data.frame/tibble and returns a service
line vector for a set of patients.

## Usage

``` r
service_line_vec(.data, .dx_col, .px_col, .drg_col)
```

## Arguments

- .data:

  The data being passed that will be augmented by the function.

- .dx_col:

  The column containing the Principal Diagnosis for the discharge.

- .px_col:

  The column containing the Principal Coded Procedure for the discharge.
  It is possible that this could be blank.

- .drg_col:

  The DRG Number coded to the inpatient discharge.

## Value

A vector of service line assignments.

## Details

This is a vectorized function in that it returns a vector. It can be
applied inside of a `mutate` statement when using `dplyr` if desired. A
data.frame/tibble is required, along with a principal diagnosis column,
a principal procedure column, and a column for the DRG number. These are
needed so that the function can join the dx_cc_mapping and px_cc_mapping
columns to provide the service line. This function only works on visits
that are coded using ICD Version 10 only.

Lets take an example discharge, the DRG is 896 and the Principal
Diagnosis code maps to DX_660, then this visit would get grouped to
`alcohol_abuse`

DRG 896: ALCOHOL, DRUG ABUSE OR DEPENDENCE WITHOUT REHABILITATION
THERAPY WITH MAJOR COMPLICATION OR COMORBIDITY (MCC)

DX_660 Maps to the following ICD-10 Codes ie F1010 Alcohol abuse,
uncomplicated:

    library(healthyR)
    dx_cc_mapping %>%
      filter(CC_Code == "DX_660", ICD_Ver_Flag == "10")

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
df <- data.frame(
  dx_col = "F10.10",
  px_col = NA,
  drg_col = "896"
)

service_line_vec(
  .data = df,
  .dx_col = dx_col,
  .px_col = px_col,
  .drg_col = drg_col
)
#> [1] "alcohol_abuse"
```
