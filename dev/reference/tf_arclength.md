# Arc length of vector-valued functional data

For a vector-valued curve `f: [a, b] -> R^d`, the arc length is
\\\int_a^b \lVert f'(t) \rVert\\ dt\\ – the length traced out by `f` in
`R^d`.

## Usage

``` r
tf_arclength(f, ...)

# Default S3 method
tf_arclength(f, ...)

# S3 method for class 'tf_mv'
tf_arclength(
  f,
  arg = NULL,
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  method = c("polyline", "derive"),
  ...
)
```

## Arguments

- f:

  a `tf_mv` object.

- ...:

  forwarded to
  [`tf_integrate()`](https://tidyfun.github.io/tf/dev/reference/tf_integrate.md)
  when `method = "derive"`.

- arg, lower, upper:

  optional evaluation/integration grid and limits.

- definite:

  `TRUE` (default) returns a numeric vector of total arc lengths per
  curve; `FALSE` returns the cumulative arc length \\s(t) = \int_a^t
  \lVert f'(u) \rVert\\ du\\ as a univariate `tfd`.

- method:

  `"polyline"` (default) or `"derive"`.

## Value

a numeric vector (definite) or a univariate `tfd` (indefinite).

## Details

Two methods are supported:

- **`"polyline"`** (default): sum of the Euclidean lengths of the line
  segments between consecutive sample points (in `R^d`). Each curve is
  evaluated on the union of its components' argument grids (or a
  supplied `arg`) and the segment-sum is computed in closed form. For
  raw `tfd_mv` data this is more accurate than `"derive"` because it
  avoids the compounding error of numerical differentiation followed by
  quadrature.

- **`"derive"`**: composes the existing verbs – per-component
  differentiation
  ([`tf_derive()`](https://tidyfun.github.io/tf/dev/reference/tf_derive.md)),
  pointwise speed
  [`tf_speed()`](https://tidyfun.github.io/tf/dev/reference/tf_geom.md),
  then
  [`tf_integrate()`](https://tidyfun.github.io/tf/dev/reference/tf_integrate.md).
  Best for `tfb_mv` (analytical derivatives) or when a custom
  `tf_integrate(...)` argument is needed.

## See also

Other tf_mv-class:
[`plot.tf_mv()`](https://tidyfun.github.io/tf/dev/reference/plot.tf_mv.md),
[`tf_geom`](https://tidyfun.github.io/tf/dev/reference/tf_geom.md),
[`tf_mv_methods`](https://tidyfun.github.io/tf/dev/reference/tf_mv_methods.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/dev/reference/tfb_mfpc.md),
[`tfb_mv()`](https://tidyfun.github.io/tf/dev/reference/tfb_mv.md),
[`tfd_mv()`](https://tidyfun.github.io/tf/dev/reference/tfd_mv.md)

## Examples

``` r
# unit circle parameterised on [0, 1] -- arc length is 2*pi
t <- seq(0, 1, length.out = 401)
circ <- tfd_mv(list(
  x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
  y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
))
tf_arclength(circ)
#> [1] 6.283121
tf_arclength(circ, lower = 0, upper = 0.25) # quarter -> pi/2
#> [1] 1.57078
tf_arclength(circ, definite = FALSE)        # cumulative s(t)
#> tfd[1]: [0,1] -> [0,6.283121] based on 401 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▁▁▁▂▂▂▂▃▃▃▄▄▄▅▅▅▆▆▆▇▇▇▇███
```
