# Use SQL LEFT type function

Perform an SQL LEFT() type function on a piece of text

## Usage

``` r
sql_left(.text, .num_char)
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
[`sql_mid()`](https://www.spsanderson.com/healthyR/reference/sql_mid.md),
[`sql_right()`](https://www.spsanderson.com/healthyR/reference/sql_right.md)

## Author

Steven P. Sanderson II, MPH

## Examples

``` r
sql_left("text", 3)
#> [1] "tex"
```
