# Evaluate `tf`-vectors for given argument values

Also used internally by the `[`-operator for `tf` data (see
[`?tfbrackets`](https://tidyfun.github.io/tf/reference/tfbrackets.md))
to evaluate `object`, see examples.

## Usage

``` r
tf_evaluate(object, arg, ...)

# Default S3 method
tf_evaluate(object, arg, ...)

# S3 method for class 'tfd'
tf_evaluate(object, arg, evaluator = tf_evaluator(object), ...)

# S3 method for class 'tfb'
tf_evaluate(object, arg, ...)
```

## Arguments

- object:

  a `tf`, or a `data.frame`-like object with `tf` columns.

- arg:

  optional evaluation grid (vector or list of vectors). Defaults to
  `tf_arg(object)`, implicitly.

- ...:

  not used.

- evaluator:

  optional. The function to use for inter/extrapolating the `tfd`.
  Defaults to `tf_evaluator(object)`. See e.g.
  [`tf_approx_linear()`](https://tidyfun.github.io/tf/reference/tf_approx.md)
  for details.

## Value

A list of numeric vectors containing the function evaluations on `arg`.

## See also

Other tidyfun inter/extrapolation functions:
[`tf_approx_linear()`](https://tidyfun.github.io/tf/reference/tf_approx.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

## Examples

``` r
f <- tf_rgp(3, arg = seq(0, 1, length.out = 11))
tf_evaluate(f) |> str()
#> List of 3
#>  $ 1: num [1:11] -0.1397 -0.5901 -0.3813 -0.0129 0.2267 ...
#>  $ 2: num [1:11] -1.135 -1.411 -1.389 -1.125 -0.595 ...
#>  $ 3: num [1:11] 0.1873 0.0837 0.3048 0.481 0.4486 ...
tf_evaluate(f, arg = 0.5) |> str()
#> List of 3
#>  $ 1: num 0.339
#>  $ 2: num 0.132
#>  $ 3: num 0.17
# equivalent, as matrix:
f[, 0.5]
#>         0.5
#> 1 0.3387099
#> 2 0.1317312
#> 3 0.1697260
#> attr(,"arg")
#> [1] 0.5
new_grid <- seq(0, 1, length.out = 6)
tf_evaluate(f, arg = new_grid) |> str()
#> List of 3
#>  $ 1: num [1:6] -0.14 -0.381 0.227 0.415 1.282 ...
#>  $ 2: num [1:6] -1.135 -1.389 -0.595 0.871 1.148 ...
#>  $ 3: num [1:6] 0.187 0.305 0.449 -0.43 -1.048 ...
# equivalent, as matrix:
f[, new_grid]
#>            0        0.2        0.4        0.6       0.8          1
#> 1 -0.1396546 -0.3812539  0.2266799  0.4150361  1.282431  1.2963920
#> 2 -1.1354409 -1.3885457 -0.5951050  0.8710293  1.147886 -1.0038656
#> 3  0.1873073  0.3047708  0.4485756 -0.4302464 -1.048161  0.0992876
#> attr(,"arg")
#> [1] 0.0 0.2 0.4 0.6 0.8 1.0
```
