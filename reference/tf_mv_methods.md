# Accessors and methods for vector-valued functional data

Utilities for `tf_mv` objects (see
[`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md) /
[`tfb_mv()`](https://tidyfun.github.io/tf/reference/tfb_mv.md)).
`tf_ncomp()` returns the number of output dimensions \\d\\,
`tf_components()` the list of the `d` underlying univariate `tf`
vectors, and `tf_component()` extracts or replaces a single one (also
available via the `$` operator, e.g. `f$x`).

## Usage

``` r
tf_ncomp(f)

tf_components(f)

tf_component(f, which)

tf_component(f, which) <- value
```

## Arguments

- f:

  a `tf_mv` object.

- which:

  a component name or index.

- value:

  a univariate `tf` vector (replacement) of matching length and domain.

## Value

`tf_ncomp()`: an integer. `tf_components()`: a named list of `tf`
vectors. `tf_component()`: a single univariate `tf` vector.

## Details

Most univariate `tf` verbs also work on `tf_mv` objects by acting on
each component:
[`tf_rebase()`](https://tidyfun.github.io/tf/reference/tf_rebase.md)
(and hence `tfd_mv`/`tfb_mv` conversion),
[`tf_derive()`](https://tidyfun.github.io/tf/reference/tf_derive.md),
[`tf_integrate()`](https://tidyfun.github.io/tf/reference/tf_integrate.md)
(definite integrals return an `n x d` matrix),
[`tf_smooth()`](https://tidyfun.github.io/tf/reference/tf_smooth.md) and
[`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.md).
Registration
([`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md)
/
[`tf_estimate_warps()`](https://tidyfun.github.io/tf/reference/tf_estimate_warps.md)
/ [`tf_warp()`](https://tidyfun.github.io/tf/reference/tf_warp.md) /
[`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.md))
estimates a *single, shared* time-warp per curve and applies it jointly
to every component. The registration signal is, by default, the first
component; use `ref_component` to pick another component (by
name/index), `"norm"` for the pointwise Euclidean norm, or a function
mapping the `tf_mv` to a univariate `tf` vector.

[`is.na()`](https://rdrr.io/r/base/NA.html) flags a curve as missing if
**any** of its components is missing (the union, not the intersection),
which also drives the `na.rm` behaviour of
[`mean()`](https://rdrr.io/r/base/mean.html) /
[`median()`](https://rdrr.io/r/stats/median.html) etc.

## See also

Other tf_mv-class:
[`plot.tf_mv()`](https://tidyfun.github.io/tf/reference/plot.tf_mv.md),
[`tf_arclength()`](https://tidyfun.github.io/tf/reference/tf_arclength.md),
[`tf_geom`](https://tidyfun.github.io/tf/reference/tf_geom.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/reference/tfb_mfpc.md),
[`tfb_mv()`](https://tidyfun.github.io/tf/reference/tfb_mv.md),
[`tfd_mv()`](https://tidyfun.github.io/tf/reference/tfd_mv.md)

## Examples

``` r
f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
tf_ncomp(f)
#> [1] 2
tf_components(f)
#> $x
#> tfd[3]: [0,1] -> [-2.614292,1.656685] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▂▂▃▃▃▄▄▄▅▅▅▆▆▆▆▆▆▇▇▇▇▇▇▇▇▇
#> 2: ▅▆▆▅▅▅▄▄▄▃▃▃▃▃▃▃▃▃▂▁▁▁▁▁▁▁
#> 3: █████▇▇▇▇▇▇▇▇▇▇▇▆▆▆▆▅▅▄▄▄▃
#> 
#> $y
#> tfd[3]: [0,1] -> [-2.029591,1.066542] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▃▄▄▄▅▅▅▆▇▇▇████▇▇▆▅▄▄▃▃▂▂▂
#> 2: ▇▇▇▇████████▇▇▆▆▆▅▅▅▆▆▆▆▆▆
#> 3: ▅▅▅▅▅▅▄▄▃▃▂▂▁▁▁▁▁▁▁▁▁▂▂▃▃▃
#> 
tf_component(f, "y")
#> tfd[3]: [0,1] -> [-2.029591,1.066542] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▃▄▄▄▅▅▅▆▇▇▇████▇▇▆▅▄▄▃▃▂▂▂
#> 2: ▇▇▇▇████████▇▇▆▆▆▅▅▅▆▆▆▆▆▆
#> 3: ▅▅▅▅▅▅▄▄▃▃▂▂▁▁▁▁▁▁▁▁▁▂▂▃▃▃
f$y
#> tfd[3]: [0,1] -> [-2.029591,1.066542] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▃▄▄▄▅▅▅▆▇▇▇████▇▇▆▅▄▄▃▃▂▂▂
#> 2: ▇▇▇▇████████▇▇▆▆▆▅▅▅▆▆▆▆▆▆
#> 3: ▅▅▅▅▅▅▄▄▃▃▂▂▁▁▁▁▁▁▁▁▁▂▂▃▃▃
```
