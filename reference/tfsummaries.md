# Functions that summarize `tf` objects across argument values

These will return a `tf` object containing the respective *functional*
statistic. See
[`tf_fwise()`](https://tidyfun.github.io/tf/reference/functionwise.md)
for scalar summaries (e.g. `tf_fmean` for means, `tf_fmax` for max.
values) of each entry in a `tf`-vector.

## Usage

``` r
# S3 method for class 'tf'
mean(x, ...)

# S3 method for class 'tf'
median(x, na.rm = FALSE, depth = "MBD", ...)

sd(x, na.rm = FALSE)

# Default S3 method
sd(x, na.rm = FALSE)

# S3 method for class 'tf'
sd(x, na.rm = FALSE)

var(x, y = NULL, na.rm = FALSE, use)

# Default S3 method
var(x, y = NULL, na.rm = FALSE, use)

# S3 method for class 'tf'
var(x, y = NULL, na.rm = FALSE, use)

# S3 method for class 'tf'
summary(object, ..., depth = "MBD")

# S3 method for class 'tf_mv'
summary(object, ..., depth = "MBD")
```

## Arguments

- x:

  a `tf` object.

- ...:

  optional additional arguments.

- na.rm:

  logical. Should missing values be removed?

- depth:

  depth method used for computing the median and central region. See
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md) for
  available methods, or pass a custom depth function. Defaults to
  `"MBD"`.

- y:

  `NULL` (default) or a vector, matrix or data frame with compatible
  dimensions to `x`. The default is equivalent to `y = x` (but more
  efficient).

- use:

  an optional character string giving a method for computing covariances
  in the presence of missing values. This must be (an abbreviation of)
  one of the strings `"everything"`, `"all.obs"`, `"complete.obs"`,
  `"na.or.complete"`, or `"pairwise.complete.obs"`.

- object:

  a `tfd` object

## Value

a `tf` object with the computed result.  
**`summary.tf`** returns a `tf`-vector with the mean function, the
functional median, the *pointwise* min and max of `x`, and the
*pointwise* min and max of the central half of the functions in `x`, as
defined by the chosen `depth` (default `"MBD"`, see
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md)).

## See also

[`tf_fwise()`](https://tidyfun.github.io/tf/reference/functionwise.md)

Other tidyfun summary functions:
[`fivenum()`](https://tidyfun.github.io/tf/reference/fivenum.md),
[`functionwise`](https://tidyfun.github.io/tf/reference/functionwise.md),
[`median.tf_mv()`](https://tidyfun.github.io/tf/reference/median.tf_mv.md)

## Examples

``` r
set.seed(123)
x <- tf_rgp(1) * 1:5
mean(x)
#> tfd[1]: [0,1] -> [-2.173123,2.158473] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ████▇▆▅▃▂▁▁▁▁▂▃▅▆▇▇▇▇▇▇▆▆▆
median(x, depth = "pointwise")
#> tfd[1]: [0,1] -> [-2.173123,2.158473] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ████▇▆▅▃▂▁▁▁▁▂▃▅▆▇▇▇▇▇▇▆▆▆
sd(x)
#> tfd[1]: [0,1] -> [0.02484437,1.145336] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ███▇▅▄▁▃▆▇██▇▆▃▁▃▅▆▆▆▅▅▃▃▃
var(x)
#> tfd[1]: [0,1] -> [0.0006172427,1.311796] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▇██▆▄▂▁▁▄▆██▆▄▁▁▂▃▅▅▄▃▃▂▂▁
summary(x)
#> tfd[6]: [0,1] -> [-3.621872,3.597455] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> min      : ▅▅▅▅▅▅▅▃▂▁▁▁▁▂▃▅▅▅▅▅▅▅▅▅▅▅
#> lower_mid: ▆▆▆▆▆▅▅▄▂▂▁▁▂▂▃▅▅▅▆▆▆▅▅▅▅▅
#> median   : ▇▇▇▆▆▅▅▄▃▂▂▂▃▃▄▅▅▆▆▆▆▆▆▅▅▅
#> mean     : ▇▇▇▆▆▅▅▄▃▂▂▂▃▃▄▅▅▆▆▆▆▆▆▅▅▅
#> upper_mid: ▇██▇▇▆▅▄▃▃▃▃▃▃▄▅▆▆▇▇▇▆▆▆▆▆
#> max      : ████▇▆▅▄▄▄▄▄▄▄▄▅▆▇▇▇▇▇▇▆▆▆
```
