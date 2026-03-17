# Re-evaluate `tf`-objects on a new grid of argument values.

Change the internal representation of a `tf`-object so that it uses a
different grid of argument values (`arg`). Useful for

- thinning out dense grids to make data smaller

- filling out sparse grids to make derivatives/integrals and locating
  extrema or zero crossings more accurate (... *if* the interpolation
  works well ...)

- making irregular functional data into (more) regular data.

For `tfd`-objects, this is just syntactic sugar for
`tfd(object, arg = arg)`. To inter/extrapolate more reliably and avoid
`NA`s, call `tf_interpolate` with `evaluator = tf_approx_fill_extend`.  
For `tfb`-objects, this re-evaluates basis functions on the new grid
which can speed up subsequent computations if they all use that grid.
NB: **To reliably impute very irregular data on a regular, common grid,
you'll be better off doing FPCA-based imputation or other model-based
approaches in most cases.**

## Usage

``` r
tf_interpolate(object, arg, ...)

# S3 method for class 'tfb'
tf_interpolate(object, arg, ...)

# S3 method for class 'tfd'
tf_interpolate(object, arg, ...)
```

## Arguments

- object:

  an object inheriting from `tf`.

- arg:

  a vector of argument values on which to evaluate the functions in
  `object`.

- ...:

  additional arguments handed over to `tfd` or `tfb`, for the
  construction of the returned object.

## Value

a `tfd` or `tfb` object on the new grid given by `arg`.

## See also

[`tf_rebase()`](https://tidyfun.github.io/tf/reference/tf_rebase.md),
which is more general.

Other tidyfun inter/extrapolation functions:
[`tf_approx_linear()`](https://tidyfun.github.io/tf/reference/tf_approx.md),
[`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md)

## Examples

``` r
# \donttest{
# thinning out a densely observed tfd
dense <- tf_rgp(10, arg = seq(0, 1, length.out = 1001))
less_dense <- tf_interpolate(dense, arg = seq(0, 1, length.out = 101))
dense
#> tfd[10]: [0,1] -> [-2.322422,2.299105] based on 1001 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▅▅▄▄▃▃▃▂▂▂▂▂▃▃▃▃▄▄▄▄▄▄▄▅▆▆
#> 2: ▄▄▄▄▄▄▄▄▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▄▄▄
#> 3: ▆▆▅▅▅▅▅▅▅▆▆▆▇▇▇██████████▇
#> 4: ▆▆▅▅▄▄▃▃▃▂▂▂▂▂▂▂▃▃▃▃▃▃▃▃▃▃
#> 5: ▆▅▅▅▅▅▅▅▅▅▅▄▄▃▃▂▂▂▂▂▃▃▄▄▅▅
#> 6: ▄▄▄▃▃▃▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▃▃▃▃
#>     [....]   (4 not shown)
less_dense
#> tfd[10]: [0,1] -> [-2.27692,2.260684] based on 101 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▅▅▄▄▃▃▃▂▂▂▂▂▃▃▃▃▄▄▄▄▄▄▄▅▆▆
#> 2: ▄▄▄▄▄▄▄▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▃▄▄▄
#> 3: ▆▆▅▅▅▅▅▅▅▆▆▆▇▇███████████▇
#> 4: ▆▆▆▅▄▄▃▃▂▂▂▂▂▂▂▂▂▃▃▃▃▃▃▃▃▃
#> 5: ▆▅▅▅▅▅▅▅▅▅▅▄▄▃▃▂▂▂▂▂▃▃▄▄▅▅
#> 6: ▄▄▄▃▃▃▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▃▃▃▃
#>     [....]   (4 not shown)
# filling out sparse data (use a suitable evaluator-function!)
sparse <- tf_rgp(10, arg = seq(0, 5, length.out = 11))
plot(sparse, points = TRUE)
# change evaluator for better interpolation
tfd(sparse, evaluator = tf_approx_spline) |>
  tf_interpolate(arg = seq(0, 5, length.out = 201)) |>
  lines(col = 2, lty = 2)


set.seed(1860)
sparse_irregular <- tf_rgp(5) |>
  tf_sparsify(0.5) |>
  tf_jiggle()
tf_interpolate(sparse_irregular, arg = seq(0, 1, length.out = 51))
#> Warning: ℹ 33 evaluations were `NA`
#> ✖ Returning irregular <tfd>.
#> irregular tfd[5]: [0,1] -> [-2.063672,1.652864] based on 39 to 48 (mean: 44) evaluations each
#> interpolation by tf_approx_linear 
#> 1: (0.14,  -1.4);(0.16,  -1.3);(0.18,  -1.3); ...
#> 2: (0.02,   1.7);(0.04,   1.6);(0.06,   1.5); ...
#> 3: (0.08,  0.93);(0.10,  1.03);(0.12,  1.11); ...
#> 4: (0.04, -0.14);(0.06, -0.29);(0.08, -0.40); ...
#> 5: (0.12, 0.049);(0.14,-0.035);(0.16,-0.098); ...
# }
```
