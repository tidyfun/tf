# Differentiating functional data: approximating derivative functions

Derivatives of `tf`-objects use finite differences of the evaluations
for `tfd` and finite differences of the basis functions for `tfb`.

## Usage

``` r
tf_derive(f, arg, order = 1, ...)

# S3 method for class 'matrix'
tf_derive(f, arg, order = 1, ...)

# S3 method for class 'tfd'
tf_derive(f, arg = tf_arg(f), order = 1, ...)

# S3 method for class 'tfd_irreg'
tf_derive(f, arg, order = 1, ...)

# S3 method for class 'tfb_spline'
tf_derive(f, arg = tf_arg(f), order = 1, ...)

# S3 method for class 'tfb_fpc'
tf_derive(f, arg = tf_arg(f), order = 1, ...)
```

## Arguments

- f:

  a `tf`-object

- arg:

  grid to use for the finite differences.

- order:

  order of differentiation. Maximal value for `tfb_spline` is 2. For
  `tfb_spline`-objects, `order = -1` yields integrals (used internally).

- ...:

  not used

## Value

a `tf` (with the same `arg` for `tfd`-inputs, possibly different `basis`
for `tfb`-inputs, see details).

## Details

The derivatives of `tfd` objects use second-order accurate central
differences for interior points and second-order accurate one-sided
differences at boundaries, following the non-uniform grid formulas from
`numpy.gradient` with `edge_order=2` (Fornberg, 1988). Domain and grid
of the returned object are identical to the input. Unless the `tfd` has
a rather fine and regular grid, representing the data in a suitable
basis representation with
[`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md) and then
computing the derivatives (or integrals) of those is usually preferable.

Note that, for spline bases like `"cr"` or `"tp"` which are constrained
to begin/end linearly, computing *second* derivatives will produce
artefacts at the outer limits of the functions' domain due to these
boundary constraints. Basis `"bs"` does not have this problem for
sufficiently high orders (but tends to yield slightly less stable fits).

## Methods (by class)

- `tf_derive(matrix)`: row-wise finite differences

- `tf_derive(tfd)`: derivatives by finite differencing of function
  evaluations.

- `tf_derive(tfd_irreg)`: element-wise finite differencing for irregular
  grids. Falls back to `tf_derive.tfd` (interpolating to a common grid)
  if an explicit `arg` vector is supplied.

- `tf_derive(tfb_spline)`: derivatives by finite differencing of spline
  basis functions.

- `tf_derive(tfb_fpc)`: derivatives by finite differencing of FPC basis
  functions.

## References

Fornberg, Bengt (1988). “Generation of Finite Difference Formulas on
Arbitrarily Spaced Grids.” *Mathematics of Computation*, **51**(184),
699–706.

## See also

Other tidyfun calculus functions:
[`tf_integrate()`](https://tidyfun.github.io/tf/reference/tf_integrate.md)

## Examples

``` r
arg <- seq(0, 1, length.out = 31)
x <- tfd(rbind(arg^2, sin(2 * pi * arg)), arg = arg)
dx <- tf_derive(x)
x
#> tfd[2]: [0,1] -> [-0.9945219,1] based on 31 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▄▅▅▅▅▅▅▅▅▅▅▅▅▆▆▆▆▆▇▇▇▇████
#> [2]: ▅▆▆▇██████▆▆▅▄▃▂▁▁▁▁▁▁▂▂▃▄
dx
#> tfd[2]: [0,1] -> [-6.237351,6.373652] based on 31 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▄▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▆▆▆▆
#> [2]: ███▇▆▅▄▄▃▂▁▁▁▁▁▁▂▃▄▄▅▆▇███
tf_arg(dx)
#>  [1] 0.00000000 0.03333333 0.06666667 0.10000000 0.13333333 0.16666667
#>  [7] 0.20000000 0.23333333 0.26666667 0.30000000 0.33333333 0.36666667
#> [13] 0.40000000 0.43333333 0.46666667 0.50000000 0.53333333 0.56666667
#> [19] 0.60000000 0.63333333 0.66666667 0.70000000 0.73333333 0.76666667
#> [25] 0.80000000 0.83333333 0.86666667 0.90000000 0.93333333 0.96666667
#> [31] 1.00000000
```
