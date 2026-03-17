# Inter- and extrapolation functions for `tfd`-objects

These are exported evaluator callbacks for `tfd` objects. They control
how function values are inter-/extrapolated to previously unseen `arg`
values and are used by
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md).

In typical use, set an evaluator when constructing a `tfd`
(`tfd(..., evaluator = tf_approx_linear)`) or replace it later via
`tf_evaluator(x) <- tf_approx_none`.

These helpers are wrappers around
[`zoo::na.fill()`](https://rdrr.io/pkg/zoo/man/na.fill.html),
[`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html), etc.
and all share the same signature (`x`, `arg`, `evaluations`), so they
can also be called directly.

The list:

- `tf_approx_linear` for linear interpolation without extrapolation
  (i.e.,
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html) with
  `na.rm = FALSE`) – this is the default,

- `tf_approx_spline` for cubic spline interpolation, (i.e.,
  [`zoo::na.spline()`](https://rdrr.io/pkg/zoo/man/na.approx.html) with
  `na.rm = FALSE`),

- `tf_approx_none` in order to not inter-/extrapolate ever (i.e.,
  [`zoo::na.fill()`](https://rdrr.io/pkg/zoo/man/na.fill.html) with
  `fill = NA`)

- `tf_approx_fill_extend` for linear interpolation and constant
  extrapolation (i.e.,
  [`zoo::na.fill()`](https://rdrr.io/pkg/zoo/man/na.fill.html) with
  `fill = "extend"`)

- `tf_approx_locf` for "last observation carried forward" (i.e.,
  [`zoo::na.locf()`](https://rdrr.io/pkg/zoo/man/na.locf.html) with
  `na.rm = FALSE`)

- `tf_approx_nocb` for "next observation carried backward" (i.e.,
  [`zoo::na.locf()`](https://rdrr.io/pkg/zoo/man/na.locf.html) with
  `na.rm = FALSE, fromLast = TRUE`).

For implementing your own, see source code of `tf:::zoo_wrapper`.

## Usage

``` r
tf_approx_linear(x, arg, evaluations)

tf_approx_spline(x, arg, evaluations)

tf_approx_none(x, arg, evaluations)

tf_approx_fill_extend(x, arg, evaluations)

tf_approx_locf(x, arg, evaluations)

tf_approx_nocb(x, arg, evaluations)
```

## Arguments

- x:

  new `arg` values to approximate/interpolate/extrapolate the function
  for.

- arg:

  the `arg` values of the `evaluations`.

- evaluations:

  the function values at `arg`.

## Value

a vector of values of the function defined by the given \\(x_i,
f(x_i))\\=`(arg, evaluations)`-tuples at new argument values `x`.

## See also

[`tfd()`](https://tidyfun.github.io/tf/reference/tfd.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

Other tidyfun inter/extrapolation functions:
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
[`tf_interpolate()`](https://tidyfun.github.io/tf/reference/tf_interpolate.md)

## Examples

``` r
x <- tfd(matrix(c(0, 1), nrow = 1), arg = c(0, 1))
tf_evaluate(x, c(0, 0.5, 1))
#> [[1]]
#> [1] 0.0 0.5 1.0
#> 
tf_evaluator(x) <- tf_approx_none
tf_evaluate(x, c(0, 0.5, 1))
#> [[1]]
#> [1]  0 NA  1
#> 

tf_approx_linear(
  x = c(0, 0.5, 1),
  arg = c(0, 1),
  evaluations = c(0, 1)
)
#> [1] 0.0 0.5 1.0
```
