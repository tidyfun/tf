# Apply warping functions to align functional data

`tf_align()` applies the *inverse* warping function to unregistered data
to obtain aligned (registered) functions.

## Usage

``` r
tf_align(x, warp, ...)

# S3 method for class 'tfd'
tf_align(x, warp, ..., keep_new_arg = FALSE)

# S3 method for class 'tfb'
tf_align(x, warp, ...)
```

## Arguments

- x:

  `tf` vector of functions. For
  [`tf_warp()`](https://tidyfun.github.io/tf/dev/reference/tf_warp.md),
  these should be registered/aligned functions and unaligned functions
  for `tf_align()`.

- warp:

  `tf` vector of warping functions used for transformation. See Details.

- ...:

  additional arguments passed to
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md).

- keep_new_arg:

  keep new `arg` values after (un)warping or return `tfd` vector on
  `arg` values of the input (default `FALSE` is the latter)? See
  Details.

## Value

the aligned `tf` vector (registered functions)

## See also

Other registration functions:
[`tf_estimate_warps()`](https://tidyfun.github.io/tf/dev/reference/tf_estimate_warps.md),
[`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/dev/reference/tf_landmarks_extrema.md),
[`tf_register()`](https://tidyfun.github.io/tf/dev/reference/tf_register.md),
[`tf_register_shape()`](https://tidyfun.github.io/tf/dev/reference/tf_register_shape.md),
[`tf_registration`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
[`tf_warp()`](https://tidyfun.github.io/tf/dev/reference/tf_warp.md)

## Examples

``` r
# Estimate warps, then align manually:
t <- seq(0, 2 * pi, length.out = 101)
x <- tfd(t(sapply(c(-0.3, 0, 0.3), function(s) sin(t + s))), arg = t)
warps <- tf_estimate_warps(x, method = "affine", type = "shift")
#> Warning: ℹ 10 evaluations were `NA`
#> ✖ Returning irregular <tfd>.
#> Warning: ℹ 10 evaluations were `NA`
#> ✖ Returning irregular <tfd>.
aligned <- tf_align(x, warps)
#> Warning: ℹ 10 evaluations were `NA`
#> ✖ Returning irregular <tfd>.
plot(aligned, col = 1:3)
```
