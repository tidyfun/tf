# Vector-valued functional data in basis representation (`f: R -> R^d`)

`tfb_mv` is the basis-representation analogue of
[`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md): it
bundles `d` univariate
[`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md) vectors (one
per output dimension / component) into a single vctrs vector of
vector-valued functions \\f: \mathbb{R} \to \mathbb{R}^d\\. Each
component is fitted independently with the usual univariate
[`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md) machinery
(spline or FPC basis), so all of its arguments (`k`, `bs`, `penalized`,
`basis`, ...) apply per component.

## Usage

``` r
tfb_mv(data, ...)

# S3 method for class 'tf_mv'
tfb_mv(data, basis = c("spline", "fpc"), ...)

# S3 method for class 'list'
tfb_mv(data, basis = c("spline", "fpc"), arg = NULL, domain = NULL, ...)

# Default S3 method
tfb_mv(data, basis = c("spline", "fpc"), arg = NULL, domain = NULL, ...)

as.tfb_mv(data, ...)

# Default S3 method
as.tfb_mv(data, ...)
```

## Arguments

- data:

  a [`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md) /
  `tfb_mv` object, a (named) `list` of univariate `tf` vectors, or
  anything
  [`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md) accepts
  (it is converted to `tfd_mv` first and then each component is expanded
  into a basis).

- ...:

  forwarded to the univariate
  [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md) constructor.

- basis:

  spline (default) or fpc basis, see
  [`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md).

- arg:

  evaluation grid for raw (list/matrix/array) inputs, forwarded to
  [`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md).

- domain:

  range of `arg`, forwarded to
  [`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md).

## Value

a `tfb_mv` object.

## Details

By default a single `...` is shared across all components (every
component gets the same `k`, `bs`, `sp`, etc.). To pass *different*
basis arguments to different components, give the argument as a list
named by component names – e.g. `tfb_mv(f, k = list(x = 5, y = 12))`
fits component `x` with `k = 5` and component `y` with `k = 12`. Any
list-valued `...` whose names do **not** match the component names is
treated as a shared argument value. (Already-`tfb` components passed via
`tfb_mv.list()` are kept as-is, which is the most permissive way to mix
entirely different basis kinds across components.)

## See also

Other tf_mv-class:
[`plot.tf_mv()`](https://tidyfun.github.io/tf/reference/plot.tf_mv.md),
[`tf_arclength()`](https://tidyfun.github.io/tf/reference/tf_arclength.md),
[`tf_geom`](https://tidyfun.github.io/tf/reference/tf_geom.md),
[`tf_mv_methods`](https://tidyfun.github.io/tf/reference/tf_mv_methods.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/reference/tfb_mfpc.md),
[`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md)

## Examples

``` r
traj <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
tb <- tfb_mv(traj, k = 7, verbose = FALSE)
tb
#> tfb_mv<d=2>[5] (x, y): [0, 1] -> [-2.252416, 1.870235] x [-1.211479, 1.995287]
#> components in basis representation: s(arg, bs = "cr", k = 7, sp = -1)
#> [1]: ███▇▇▆▆▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▆▆▆▆ | ▃▃▂▃▃▄▄▅▆▆▆▆▆▅▅▄▄▄▄▅▆▆▇███
#> [2]: ▇▇███▇▇▆▆▅▄▃▃▃▂▂▃▃▄▅▆▇▇███ | ▅▅▄▃▂▂▁▁▂▂▃▃▄▄▄▅▅▅▄▄▄▄▄▅▅▅
#> [3]: ▄▄▄▄▄▄▄▄▄▅▅▆▆▆▇████▇▇▆▅▄▄▃ | ▃▄▅▅▆▆▇▇▇▆▆▅▅▄▄▄▃▃▃▃▃▄▄▄▄▅
#> [4]: ▁▁▁▁▁▁▁▁▁▂▂▂▂▂▂▂▂▃▃▃▃▄▄▄▄▅ | ▃▂▂▁▁▁▁▁▁▂▃▄▅▆▆▇▇▇▆▆▅▄▃▃▂▂
#> [5]: ▅▅▄▄▄▃▃▃▃▃▃▃▃▃▄▄▄▄▄▅▅▅▅▆▆▇ | ▅▅▅▅▅▆▆▆▆▆▆▆▆▆▅▅▄▄▄▅▅▆▆▆▆▆
#> 
tf_ncomp(tb)
#> [1] 2
```
