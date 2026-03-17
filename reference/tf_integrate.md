# Integrals and anti-derivatives of functional data

Integrals of `tf`-objects are computed by simple quadrature (trapezoid
rule). By default the scalar definite integral
\\\int^{upper}\_{lower}f(s)ds\\ is returned (option `definite = TRUE`),
alternatively for `definite = FALSE` the *anti-derivative* on
`[lower, upper]`, e.g. a `tfd` or `tfb` object representing \\F(t)
\approx \int^{t}\_{lower}f(s)ds\\, for \\t \in\\`[lower, upper]`, is
returned.

## Usage

``` r
tf_integrate(f, arg, lower, upper, ...)

# Default S3 method
tf_integrate(f, arg, lower, upper, ...)

# S3 method for class 'tfd'
tf_integrate(
  f,
  arg = tf_arg(f),
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  ...
)

# S3 method for class 'tfb'
tf_integrate(
  f,
  arg = tf_arg(f),
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  ...
)
```

## Arguments

- f:

  a `tf`-object

- arg:

  (optional) grid to use for the quadrature.

- lower:

  lower limits of the integration range. For `definite = TRUE`, this can
  be a vector of the same length as `f`.

- upper:

  upper limits of the integration range (but see `definite` arg /
  description). For `definite = TRUE`, this can be a vector of the same
  length as `f`.

- ...:

  not used

- definite:

  should the definite integral be returned (default) or the
  antiderivative. See description.

## Value

For `definite = TRUE`, the definite integrals of the functions in `f`.
For `definite = FALSE` and `tf`-inputs, a `tf` object containing their
anti-derivatives

## See also

Other tidyfun calculus functions:
[`tf_derive()`](https://tidyfun.github.io/tf/reference/tf_derive.md)

## Examples

``` r
arg <- seq(0, 1, length.out = 11)
x <- tfd(rbind(arg, arg^2), arg = arg)
#> New names:
#> • `` -> `...2`
tf_integrate(x)
#>   arg  ...2 
#> 0.500 0.335 
anti <- tf_integrate(x, definite = FALSE)
tf_arg(anti)
#>  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
```
