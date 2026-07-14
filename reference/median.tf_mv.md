# Joint depth-median for vector-valued functional data

The median of a `tf_mv` vector is the single *observed* curve with
maximal joint depth (see
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md)): one
`which.max` index selects the same observation across every component,
so the result is never a "chimera" stitched together from different
curves. Note the deliberate divergence from
[`median.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
on ties: the univariate median *averages* tied maximal-depth curves, but
averaging components would break the observed-curve guarantee, so
`median.tf_mv` returns the first tied curve (with a message). On tied
data, `median(f)$x` and `median(f$x)` can therefore differ.

## Usage

``` r
# S3 method for class 'tf_mv'
median(x, na.rm = FALSE, depth = "MBD", ...)
```

## Arguments

- x:

  a `tf_mv` vector.

- na.rm:

  if `FALSE` (default), any `NA` observation makes the result `NA`; if
  `TRUE`, `NA` observations are dropped first.

- depth:

  the joint depth method, see
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md).

- ...:

  passed to
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md).

## Value

a length-1 `tf_mv`: the observed curve with maximal joint depth.

## See also

Other tidyfun summary functions:
[`fivenum()`](https://tidyfun.github.io/tf/reference/fivenum.md),
[`functionwise`](https://tidyfun.github.io/tf/reference/functionwise.md),
[`tfsummaries`](https://tidyfun.github.io/tf/reference/tfsummaries.md)

## Examples

``` r
set.seed(1)
f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
# the joint median is the observed curve with maximal joint depth:
median(f)
#> tfd_mv<d=2>[1] (x, y): [0, 1] -> [-0.4774177, 1.137399] x [-0.1362092, 1.230932]
#> components based on 51 evaluations each, interpolation by tf_approx_linear
#> [1]: ▆▇████▇▇▆▅▄▃▃▃▃▄▄▅▆▆▆▆▅▃▂▁ | ▃▃▃▃▃▃▃▃▃▃▂▂▁▁▁▁▁▁▂▃▄▆▇███
#> 
tf_depth(f)
#>     1     2     3     4     5 
#> 1.333 1.351 1.196 1.025 1.095 
```
