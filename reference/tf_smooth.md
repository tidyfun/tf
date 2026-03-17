# Simple smoothing of `tf` objects

Apply running means or medians, `lowess` or Savitzky-Golay filtering to
smooth functional data. This does nothing for `tfb`-objects, which
should be smoothed by using a smaller basis / stronger penalty.

## Usage

``` r
tf_smooth(x, ...)

# S3 method for class 'tfb'
tf_smooth(x, verbose = TRUE, ...)

# S3 method for class 'tfd'
tf_smooth(
  x,
  method = c("lowess", "rollmean", "rollmedian", "savgol"),
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  a `tf` object containing functional data.

- ...:

  arguments for the respective `method`. See details.

- verbose:

  give lots of diagnostic messages? Defaults to `TRUE`.

- method:

  one of `"lowess"` (see
  [`stats::lowess()`](https://rdrr.io/r/stats/lowess.html)),
  `"rollmean"`, `"rollmedian"` (see
  [`zoo::rollmean()`](https://rdrr.io/pkg/zoo/man/rollmean.html)) or
  `"savgol"` (see
  [`pracma::savgol()`](https://rdrr.io/pkg/pracma/man/savgol.html)).

## Value

a smoothed version of the input. For some methods/options, the smoothed
functions may be shorter than the original ones (at both ends).

## Details

`tf_smooth.tfd` overrides/automatically sets some defaults of the used
methods:

- **`lowess`** uses a span parameter of `f` = 0.15 (instead of 0.75) by
  default.

- **`rollmean`/`median`** use a window size of `k` = \$\<\$number of
  grid points\$\>\$/20 (i.e., the nearest odd integer to that) and sets
  `fill= "extend"` (i.e., constant extrapolation to replace missing
  values at the extremes of the domain) by default. Use `fill= NA` for
  `zoo`'s default behavior of shortening the smoothed series.

- **`savgol`** uses a window size of `k` = \$\<\$number of grid
  points\$\>\$/10 (i.e., the nearest odd integer to that).

## Examples

``` r
library(zoo)
#> 
#> Attaching package: ‘zoo’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.Date, as.Date.numeric
library(pracma)
#> 
#> Attaching package: ‘pracma’
#> The following object is masked from ‘package:fdasrvf’:
#> 
#>     gradient
f <- tf_sparsify(tf_jiggle(tf_rgp(4, 201, nugget = 0.05)))
f_lowess <- tf_smooth(f, "lowess")
#> Using `f = 0.15` as smoother span for `lowess()`.
# these methods ignore the distances between arg-values:
f_mean <- tf_smooth(f, "rollmean")
#> ✖ Non-equidistant arg-values in `x` ignored by "rollmean".
#> Using `k = 5` observations for rolling data window.
#> Setting `fill = 'extend'` for start/end values.
f_median <- tf_smooth(f, "rollmedian", k = 31)
#> ✖ Non-equidistant arg-values in `x` ignored by "rollmedian".
#> Setting `fill = 'extend'` for start/end values.
f_sg <- tf_smooth(f, "savgol", fl = 31)
#> ✖ Non-equidistant arg-values in `x` ignored by "savgol".
layout(t(1:4))
plot(f, points = FALSE, main = "original")
plot(f_lowess,
  points = FALSE, col = "blue", main = "lowess (default,\n span 0.9 in red)"
)
lines(tf_smooth(f, "lowess", f = 0.9), col = "red", alpha = 0.2)
plot(f_mean,
  points = FALSE, col = "blue", main = "rolling means &\n medians (red)"
)
lines(f_median, col = "red", alpha = 0.2) # note constant extrapolation at both ends!
plot(f, points = FALSE, main = "original and\n savgol (red)")
lines(f_sg, col = "red")
```
