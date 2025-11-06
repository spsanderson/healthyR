# Use SQL MID type function

Perform an SQL SUBSTRING type function

## Usage

``` r
sql_mid(.text, .start_num, .num_char)
```

## Arguments

- .text:

  A piece of text/string to be manipulated

- .start_num:

  What place to start at

- .num_char:

  How many characters do you want to grab

## Details

- You must supply data that you want to manipulate.

## See also

Other Utilities:
[`opt_bin()`](https://www.spsanderson.com/healthyR/reference/opt_bin.md),
[`save_to_excel()`](https://www.spsanderson.com/healthyR/reference/save_to_excel.md),
[`sql_left()`](https://www.spsanderson.com/healthyR/reference/sql_left.md),
[`sql_right()`](https://www.spsanderson.com/healthyR/reference/sql_right.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
sql_mid("this is some text", 6, 2)
#> [1] "is"
```
