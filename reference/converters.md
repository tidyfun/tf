# Convert functional data back to tabular data formats

Various converters to turn `tfb`- or `tfd`-vectors into data.frames or
matrices, or even an actual R function.

## Usage

``` r
# S3 method for class 'tf'
as.data.frame(x, row.names = NULL, optional = FALSE, unnest = FALSE, ...)

# S3 method for class 'tf'
as.matrix(x, arg, interpolate = FALSE, ...)

# S3 method for class 'tf'
as.function(x, ...)
```

## Arguments

- x:

  a `tf` object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  not used.

- unnest:

  if `TRUE`, the function will return a data.frame with the evaluated
  functions.

- ...:

  additional arguments to be passed to or from methods.

- arg:

  a vector of argument values / evaluation points for `x`. Defaults to
  `tf_arg(x)` (so for `x` on irregular grids, this will be the union of
  all observed `arg`-values by default).

- interpolate:

  should functions be evaluated (i.e., inter-/extrapolated) for values
  in `arg` for which no original data is available? Only relevant for
  the raw data class `tfd`, for which it defaults to `FALSE`.
  Basis-represented functional data `tfb` are always "interpolated".

## Value

**for `as.data.frame.tf`:** if `unnest` is `FALSE` (default), a
one-column `data.frame` with a `tf`-column containing `x`. if `unnest`
is `TRUE`, a 3-column data frame with columns `id` (containing (unique)
names of `x` or a numeric identifier if `x` is unnamed), `arg`, and
`value`, with each row containing one function evaluation at the
original `arg`-values.

**for `as.matrix.tf`:** a matrix with one row per function and one
column per `arg`.

**for `as.function.tf`:** an R function with argument `arg` that
evaluates `x` on `arg` and returns the list of function values

## Examples

``` r
f <- tfd(sin(seq(0, 2 * pi, length.out = 11)), arg = seq(0, 1, length.out = 11))
as.data.frame(f)
#>             f
#> 1 ▄▇██▇▅▂▁▁▂▄
as.data.frame(f, unnest = TRUE)
#>    id arg         value
#> 1   1 0.0  0.000000e+00
#> 2   1 0.1  5.877853e-01
#> 3   1 0.2  9.510565e-01
#> 4   1 0.3  9.510565e-01
#> 5   1 0.4  5.877853e-01
#> 6   1 0.5  1.224647e-16
#> 7   1 0.6 -5.877853e-01
#> 8   1 0.7 -9.510565e-01
#> 9   1 0.8 -9.510565e-01
#> 10  1 0.9 -5.877853e-01
#> 11  1 1.0 -2.449294e-16
as.matrix(f)
#>      0       0.1       0.2       0.3       0.4          0.5        0.6
#> [1,] 0 0.5877853 0.9510565 0.9510565 0.5877853 1.224647e-16 -0.5877853
#>             0.7        0.8        0.9             1
#> [1,] -0.9510565 -0.9510565 -0.5877853 -2.449294e-16
#> attr(,"arg")
#>  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
fun <- as.function(f)
fun(c(0, 0.5, 1))
#> [[1]]
#> [1]  0.000000e+00  1.224647e-16 -2.449294e-16
#> 
```
