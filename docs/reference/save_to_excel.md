# Save a file to Excel

Save a tibble/data.frame to an excel `.xlsx` file. The file will
automatically with a save_dtime in the format of 20201109_132416 for
November 11th, 2020 at 1:24:16PM.

## Usage

``` r
save_to_excel(.data, .file_name)
```

## Arguments

- .data:

  The tibble/data.frame that you want to save as an `.xlsx` file.

- .file_name:

  the name you want to give to the file.

## Value

A saved excel file

## Details

- Requires a tibble/data.frame to be passed to it.

## See also

Other Utilities:
[`opt_bin()`](https://www.spsanderson.com/healthyR/reference/opt_bin.md),
[`sql_left()`](https://www.spsanderson.com/healthyR/reference/sql_left.md),
[`sql_mid()`](https://www.spsanderson.com/healthyR/reference/sql_mid.md),
[`sql_right()`](https://www.spsanderson.com/healthyR/reference/sql_right.md)

## Author

Steven P. Sanderson II, MPH
