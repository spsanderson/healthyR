# Use SQL RIGHT type functions

Perform an SQL RIGHT type function

## Usage

``` r
sql_right(.text, .num_char)
```

## Arguments

- .text:

  A piece of text/string to be manipulated

- .num_char:

  How many characters do you want to grab

## Details

- You must supply data that you want to manipulate.

## See also

Other Utilities:
[`opt_bin()`](https://www.spsanderson.com/healthyR/reference/opt_bin.md),
[`save_to_excel()`](https://www.spsanderson.com/healthyR/reference/save_to_excel.md),
[`sql_left()`](https://www.spsanderson.com/healthyR/reference/sql_left.md),
[`sql_mid()`](https://www.spsanderson.com/healthyR/reference/sql_mid.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
sql_right("this is some more text", 3)
#> [1] "ext"
```
