# Invert a `tf` vector

Computes the functional inverse of each function in the `tf` vector,
such that if \\y = f(x)\\, then \\x = f^{-1}(y)\\.

## Usage

``` r
tf_invert(x, ...)
```

## Arguments

- x:

  a `tf` vector.

- ...:

  optional arguments for the returned object, see
  [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.md) /
  [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md)

## Value

a `tf` vector of the inverted functions.

## Examples

``` r
arg <- seq(0, 2, length.out = 50)
x <- tfd(rbind(2 * arg, arg^2), arg = arg)
x_inv <- tf_invert(x)
layout(t(1:2))
plot(x, main = "original functions", ylab = "")
plot(x_inv, main = "inverted functions", ylab = "", points = FALSE)
```
