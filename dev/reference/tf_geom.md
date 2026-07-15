# Pointwise norm and inner product for functional data

Small geometric helpers defined by component-wise composition of the
existing univariate `Ops` / `Math` machinery:

- `tf_norm(f)` – pointwise Euclidean norm \\\lVert f(t) \rVert\\;

- `tf_speed(f)` – pointwise speed \\\lVert f'(t) \rVert\\;

- `tf_inner(f, g)` – pointwise inner product \\\langle f(t), g(t)
  \rangle\\;

- `tf_distance(f, g)` – pointwise Euclidean distance \\\lVert f(t) -
  g(t) \rVert\\;

- `tf_tangent(f)` – unit tangent \\f'(t) / \lVert f'(t) \rVert\\
  (undefined where the speed is zero – callers get `NaN`s there);

- `tf_reparam_arclength(f)` – re-parametrize the curve at constant speed
  (i.e. by its normalized cumulative arc length).

## Usage

``` r
tf_norm(f)

# Default S3 method
tf_norm(f)

# S3 method for class 'tf'
tf_norm(f)

# S3 method for class 'tf_mv'
tf_norm(f)

tf_speed(f)

tf_inner(f, g)

# Default S3 method
tf_inner(f, g)

# S3 method for class 'tf'
tf_inner(f, g)

# S3 method for class 'tf_mv'
tf_inner(f, g)

tf_distance(f, g)

tf_tangent(f)

# Default S3 method
tf_tangent(f)

# S3 method for class 'tf'
tf_tangent(f)

# S3 method for class 'tf_mv'
tf_tangent(f)

tf_reparam_arclength(f)
```

## Arguments

- f, g:

  `tf_mv` objects, or univariate `tf` (`tfd`/`tfb`) objects (with
  identical `d` and component names where two `tf_mv` arguments are
  required).

## Value

a univariate `tfd` for `tf_norm`/`tf_speed`/`tf_inner`/`tf_distance`;
`tf_tangent` returns a `tf_mv` (or a univariate `tf` for univariate
input) and `tf_reparam_arclength` a `tf_mv`.

## Details

These also apply to *univariate* `tfd`/`tfb` (treated as scalar-valued
curves \\f: T \to \mathbb{R}\\), where they reduce to their
one-dimensional specializations: \\\lVert f(t) \rVert = \|f(t)\|\\,
\\\langle f(t), g(t) \rangle = f(t)\\g(t)\\, and the unit tangent
\\f'(t) / \|f'(t)\| = \mathrm{sign}(f'(t))\\.

## See also

Other tf_mv-class:
[`plot.tf_mv()`](https://tidyfun.github.io/tf/dev/reference/plot.tf_mv.md),
[`tf_arclength()`](https://tidyfun.github.io/tf/dev/reference/tf_arclength.md),
[`tf_mv_methods`](https://tidyfun.github.io/tf/dev/reference/tf_mv_methods.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/dev/reference/tfb_mfpc.md),
[`tfb_mv()`](https://tidyfun.github.io/tf/dev/reference/tfb_mv.md),
[`tfd_mv()`](https://tidyfun.github.io/tf/dev/reference/tfd_mv.md)

## Examples

``` r
set.seed(1)
f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
tf_norm(f)
#> tfd[2]: [0,1] -> [0.4845396,2.208042] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▂▃▄▄▄▄▃▂▂▁▁▁▂▃▄▆▇█████▇▇▆▅
#> 2: ▃▃▃▄▅▅▆▆▇▇▆▆▅▅▃▂▁▁▁▂▂▁▁▁▂▃
tf_speed(f)
#> tfd[2]: [0,1] -> [0.7946952,12.11029] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▅▄▂▂▂▃▃▃▄▄▅▅▆▆▅▅▄▃▂▃▂▃▄▃▄▃
#> 2: ▇█▇▇▆▅▄▄▄▄▃▃▄▅▅▆▅▅▃▁▁▄▆▇▇▄
tf_distance(f, tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2))))
#> tfd[2]: [0,1] -> [0.2677482,2.923029] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▁▂▂▂▃▃▄▅▆▆▆▆▅▅▅▅▅▆▆▇█████▇
#> 2: ▃▄▅▆▆▆▇▆▆▆▅▅▄▄▂▁▁▁▂▂▃▃▄▅▇█
# univariate: tf_norm reduces to the pointwise absolute value
u <- tf_rgp(2)
tf_norm(u)
#> tfd[2]: [0,1] -> [0.0002797784,2.825257] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▂▂▂▁▁▁▂▂▃▄▄▄▄▄▄▄▄▅▅▆▇█████
#> 2: ▅▅▅▅▄▄▃▃▃▂▂▂▂▃▂▂▂▂▂▁▁▁▁▁▁▁
tf_inner(u, tf_rgp(2))
#> tfd[2]: [0,1] -> [-1.494741,4.227105] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▁▁▂▂▂▃▃▃▂▂▂▁▁▁▁▂▂▂▃▄▅▆▇███
#> 2: ▃▃▄▄▄▄▄▄▄▄▃▃▃▃▃▃▃▃▂▃▃▃▃▃▃▃
```
