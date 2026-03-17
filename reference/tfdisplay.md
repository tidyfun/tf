# Pretty printing and formatting for functional data

Prints and formats `tf`-objects for display. See details / examples for
options that give finer control.

## Usage

``` r
# S3 method for class 'tf'
print(x, n = 6, ...)

# S3 method for class 'tfd_reg'
print(x, n = 6, ...)

# S3 method for class 'tfd_irreg'
print(x, n = 6, ...)

# S3 method for class 'tfb'
print(x, n = 5, ...)

# S3 method for class 'tf'
format(
  x,
  digits = 2,
  nsmall = 0,
  width = options()$width,
  sparkline = TRUE,
  prefix = FALSE,
  ...
)
```

## Arguments

- x:

  any R object (conceptually); typically numeric.

- n:

  how many elements of `x` to print out at most, defaults to `6`.

- ...:

  handed over to `format.tf()`.

- digits:

  a positive integer indicating how many significant digits are to be
  used for numeric and complex `x`. The default, `NULL`, uses
  [`getOption`](https://rdrr.io/r/base/options.html)`("digits")`. This
  is a suggestion: enough decimal places will be used so that the
  smallest (in magnitude) number has this many significant digits, and
  also to satisfy `nsmall`. (For more, notably the interpretation for
  complex numbers see [`signif`](https://rdrr.io/r/base/Round.html).)

- nsmall:

  the minimum number of digits to the right of the decimal point in
  formatting real/complex numbers in non-scientific formats. Allowed
  values are `0 <= nsmall <= 20`.

- width:

  `default` method: the *minimum* field width or `NULL` or `0` for no
  restriction.

  `AsIs` method: the *maximum* field width for non-character objects.
  `NULL` corresponds to the default `12`.

- sparkline:

  use a sparkline representation? defaults to `TRUE` (not available for
  irregular data).

- prefix:

  prefix with names / index positions? defaults to `FALSE`.

## Value

**`print`**: prints out `x` and returns it invisibly.

a character representation of `x`.

## Details

By default, `tf` objects on regular grids are shown as "sparklines"
([`cli::spark_bar()`](https://cli.r-lib.org/reference/spark_bar.html)),
set `sparkline = FALSE` for a text representation.

Sparklines are based on running mean values of the function values, but
these don't check for non-equidistant grids, so the visual impression
will be misleading for very unequal grid distances.

Sparklines use `options()$width/3` bins for printing/formatting by
default, use `bins` argument to set the number of bins explicitly. For
[`tibble::glimpse()`](https://tibble.tidyverse.org/reference/reexports.html),
we use 8 bins by default for compact display.

## Examples

``` r
t <- seq(0, 1, l = 201)
cosine <- lapply(1:4, \(i) cos(i * pi * t)) |> tfd(arg = t)
cosine
#> tfd[4]: [0,1] -> [-1,1] based on 201 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ██████▇▇▇▆▆▅▅▄▄▃▃▂▂▂▁▁▁▁▁▁
#> [2]: ███▇▆▅▅▄▃▂▁▁▁▁▁▁▂▃▄▅▅▆▇███
#> [3]: ██▇▆▄▃▂▁▁▁▁▂▄▅▇████▇▆▅▃▂▁▁
#> [4]: █▇▆▄▂▁▁▁▂▄▆▇██▇▆▄▂▁▁▁▂▄▆▇█
tf_sparsify(cosine, dropout = .8)
#> irregular tfd[4]: [0,1] -> [-1,1] based on 32 to 50 (mean: 43) evaluations each
#> interpolation by tf_approx_linear 
#> [1]: (0.005,   1);(0.015,   1);(0.020,   1); ...
#> [2]: (0.015,1.00);(0.090,0.84);(0.125,0.71); ...
#> [3]: ( 0.02,0.98);( 0.06,0.84);( 0.07,0.79); ...
#> [4]: ( 0.00,1.00);( 0.01,0.99);( 0.03,0.93); ...

format(cosine, sparkline = FALSE)
#> [1] "(0.000,   1);(0.005,   1);(0.010,   1); ..."
#> [2] "(0.000,   1);(0.005,   1);(0.010,   1); ..."
#> [3] "(0.000,   1);(0.005,   1);(0.010,   1); ..."
#> [4] "(0.000,1.00);(0.005,1.00);(0.010,0.99); ..."
format(cosine, bins = 5)
#> [1] "█▇▄▂▁" "█▃▁▃█" "▇▁▄█▂" "▅▂▇▂▅"
format(cosine, bins = 40)
#> [1] "█████████▇▇▇▇▆▆▆▆▅▅▅▄▄▄▃▃▃▃▂▂▂▂▁▁▁▁▁▁▁▁▁"
#> [2] "█████▇▇▆▅▅▄▄▃▂▂▁▁▁▁▁▁▁▁▁▁▂▂▃▄▄▅▅▆▇▇█████"
#> [3] "███▇▆▆▅▄▃▂▁▁▁▁▁▁▂▂▃▄▅▆▇▇██████▇▆▅▄▃▃▂▁▁▁"
#> [4] "██▇▆▅▄▃▂▁▁▁▁▂▃▄▅▆▇████▇▆▅▄▃▂▁▁▁▁▂▃▄▅▆▇██"

#! very non-equidistant grids --> sparklines can mislead about actual shapes:
tfd(cosine, arg = t^3)
#> tfd[4]: [0,1] -> [-1,1] based on 201 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ████████████████▇▇▆▅▅▄▃▂▁▁
#> [2]: █████████████▇▆▅▄▃▂▁▁▁▂▄▇█
#> [3]: ███████████▇▆▆▄▃▁▁▁▂▄▇█▇▄▁
#> [4]: ██████████▇▆▅▄▂▁▁▁▄▇█▇▃▁▃▇
```
