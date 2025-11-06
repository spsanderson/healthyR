# Provide Colorblind Compliant Colors

8 Hex RGB color definitions suitable for charts for colorblind people.

## Usage

``` r
hr_scale_fill_colorblind(..., theme = "hr")
```

## Arguments

- ...:

  Data passed in from a `ggplot` object

- theme:

  Right now this is `hr` only. Anything else will render an error.

## Value

A `gggplot` layer

## Details

This function is used in others in order to help render plots for those
that are color blind.

## See also

Other Color Blind:
[`color_blind()`](https://www.spsanderson.com/healthyR/reference/color_blind.md),
[`hr_scale_color_colorblind()`](https://www.spsanderson.com/healthyR/reference/hr_scale_color_colorblind.md)

## Author

Steven P. Sanderson II, MPH
