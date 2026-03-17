# Fourier basis for mgcv

A `mgcv`-style smooth constructor for Fourier bases, used internally by
[`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)
when `bs = "fourier"`.

## Usage

``` r
# S3 method for class 'fourier.smooth.spec'
smooth.construct(object, data, knots)

# S3 method for class 'fourier.smooth'
Predict.matrix(object, data)
```

## Arguments

- object:

  a fitted `fourier.smooth` object.

- data:

  a list containing the data vector for prediction.

- knots:

  not used.

## Value

a smooth specification object with the Fourier basis matrix `X` and
optional second-derivative penalty `S`.

a design matrix evaluated at the new data points.

## Examples

``` r
# \donttest{
# used internally via tfb_spline:
f <- c(sin(2 * pi * (0:100) / 100), cos(2 * pi * (0:100) / 100))
tf_smooth <- tfb_spline(f, bs = "fourier", k = 11)
#> Percentage of input data variability preserved in basis representation
#> (per functional observation, approximate):
#> Min. 1st Qu.  Median Mean 3rd Qu.  Max.
#> 95.9 95.9 95.9 95.9 95.9 95.9
# }
```
