# Constructors for vector-valued functional data (`f: R -> R^d`)

`tfd_mv` represents *vector-valued* functional data – vectors of
functions \\f: \mathcal{T} \subset \mathbb{R} \to \mathbb{R}^d\\, such
as movement trajectories \\(x(t), y(t))\\ or other multivariate-output
curves.

## Usage

``` r
tfd_mv(data, ...)

# S3 method for class 'list'
tfd_mv(data, arg = NULL, domain = NULL, evaluator = tf_approx_linear, ...)

# S3 method for class 'array'
tfd_mv(data, arg = NULL, domain = NULL, evaluator = tf_approx_linear, ...)

# S3 method for class 'data.frame'
tfd_mv(
  data,
  id = 1,
  arg = 2,
  value = 3,
  domain = NULL,
  evaluator = tf_approx_linear,
  ...
)

# S3 method for class 'tf_mv'
tfd_mv(data, arg = NULL, domain = NULL, evaluator = NULL, ...)

# Default S3 method
tfd_mv(data, arg = NULL, domain = NULL, ...)

as.tfd_mv(data, ...)

# Default S3 method
as.tfd_mv(data, ...)

# S3 method for class 'tf_mv'
as.tfd_mv(data, ...)
```

## Arguments

- data:

  one of: a (named) `list` of univariate `tf` vectors (used directly,
  one per component); a (named) `list` of numeric matrices / data.frames
  (one *per component*, each turned into a
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md)); a 3-d
  numeric `array` with dimensions `[curve, arg, component]`; or a long
  `data.frame` with an `id` column, an `arg` column and one or more
  `value` columns (one component per `value` column).

- ...:

  forwarded to the univariate
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md)
  constructor.

- arg:

  evaluation grid, see
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md).

- domain:

  range of `arg`, see
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md).

- evaluator:

  inter-/extrapolation function, see
  [`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md).

- id, value:

  for the `data.frame` method: the column defining function `id`, the
  column defining the `arg` grid, and the (possibly several) columns
  containing component evaluations (one component per `value` column).

## Value

a `tfd_mv` object (a vctrs vector of length `n`).

## Details

A `tfd_mv` object of length `n` bundles `d` *univariate*
[`tfd()`](https://tidyfun.github.io/tf/dev/reference/tfd.md) vectors
(one per output dimension / component), each of length `n`. All numeric
work (evaluation, arithmetic, smoothing, ...) is delegated to these
components, so regular and irregular sampling, the choice of
`evaluator`, etc. all behave exactly as in the univariate case – and
components may even live on different argument grids. Use
[`tfb_mv()`](https://tidyfun.github.io/tf/dev/reference/tfb_mv.md) for a
basis representation.

## Inheritance contract

`tf_mv` classes inherit from `"tf"` *only* for the purpose of
[`tf_domain()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
type predicates
([`is_tf()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
[`is_tf_mv()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md),
...) and S4 generic reuse. **Behaviour** on `tf_mv` comes *only* from
explicitly registered `.tf_mv` methods: any generic without one aborts
with a classed `tf_mv_method_unimplemented` condition. The earlier
promise of automatic "right thing component-wise" dispatch via
inheritance was incorrect – silent fall-through produced wrong-shape
results or deep internal errors, so it has been replaced with fail-fast
stubs. The stubbed (i.e., *not* implemented) verbs are listed in
[tf_mv_unimplemented](https://tidyfun.github.io/tf/dev/reference/tf_mv_unimplemented.md);
design of real component-wise semantics is tracked at
<https://github.com/tidyfun/tf/issues/255>. When you need to
*distinguish* univariate-only from any-`tf` inside a helper, use
[`is_tf_1d()`](https://tidyfun.github.io/tf/dev/reference/tfmethods.md):
it returns `TRUE` for `tfd` / `tfb` and `FALSE` for `tfd_mv` / `tfb_mv`.

## See also

[`tfb_mv()`](https://tidyfun.github.io/tf/dev/reference/tfb_mv.md) for
basis representation;
[`tf_components()`](https://tidyfun.github.io/tf/dev/reference/tf_mv_methods.md),
[`tf_ncomp()`](https://tidyfun.github.io/tf/dev/reference/tf_mv_methods.md)
and the `$` operator to access components.

Other tf_mv-class:
[`plot.tf_mv()`](https://tidyfun.github.io/tf/dev/reference/plot.tf_mv.md),
[`tf_arclength()`](https://tidyfun.github.io/tf/dev/reference/tf_arclength.md),
[`tf_geom`](https://tidyfun.github.io/tf/dev/reference/tf_geom.md),
[`tf_mv_methods`](https://tidyfun.github.io/tf/dev/reference/tf_mv_methods.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/dev/reference/tfb_mfpc.md),
[`tfb_mv()`](https://tidyfun.github.io/tf/dev/reference/tfb_mv.md)

## Examples

``` r
# (a) from a (named) list of univariate tfd vectors -- one per component:
traj <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
traj
#> tfd_mv<d=2>[3] (x, y): [0, 1] -> [-2.384502, 1.093416] x [-1.715262, 2.546049]
#> components based on 51 evaluations each, interpolation by tf_approx_linear
#> [1]: ▆▆▆▆▆▆▆▇▇▇█████████▇▇▇▆▆▆▅ | ▃▄▄▅▆▆▆▆▆▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅▅
#> [2]: ▄▄▄▃▃▂▁▁▁▁▁▁▂▂▂▂▂▂▂▂▂▂▂▂▃▃ | ▆▆▆▅▅▅▄▄▃▃▂▂▂▂▁▁▁▁▁▁▁▂▂▃▃▄
#> [3]: ▅▄▄▅▅▆▆▇▇▇▆▆▅▄▃▂▂▁▁▁▁▁▁▁▂▃ | ▆▆▆▆▇▇▇████████▇▇▆▅▅▄▄▃▃▂▂
#> 
tf_ncomp(traj)
#> [1] 2
traj$x
#> tfd[3]: [0,1] -> [-2.384502,1.093416] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▆▆▆▆▆▆▆▇▇▇█████████▇▇▇▆▆▆▅
#> 2: ▄▄▄▃▃▂▁▁▁▁▁▁▂▂▂▂▂▂▂▂▂▂▂▂▃▃
#> 3: ▅▄▄▅▅▆▆▇▇▇▆▆▅▄▃▂▂▁▁▁▁▁▁▁▂▃

# (b) from a list of matrices (one [curve, arg] matrix per component):
t <- seq(0, 1, length.out = 50)
mx <- matrix(sin(2 * pi * outer(1:3, t)), nrow = 3)
my <- matrix(cos(2 * pi * outer(1:3, t)), nrow = 3)
tfd_mv(list(x = mx, y = my), arg = t)
#> tfd_mv<d=2>[3] (x, y): [0, 1] -> [-0.9994862, 0.9994862] x [-0.9979454, 1]
#> components based on 50 evaluations each, interpolation by tf_approx_linear
#> [1]: ▅▆▇▇██████▇▆▅▄▃▂▁▁▁▁▁▁▂▂▃▄ | ███▇▆▅▄▄▃▂▁▁▁▁▁▁▂▃▄▄▅▆▇███
#> [2]: ▅▇███▆▄▃▂▁▁▂▃▆▇██▇▆▅▃▁▁▁▂▄ | ██▆▄▂▁▁▁▂▄▆▇██▇▆▄▂▁▁▁▂▄▆██
#> [3]: ▅██▇▄▁▁▁▃▆██▆▃▁▁▃▆███▅▂▁▁▄ | █▇▄▁▁▂▅▆██▆▃▁▁▃▆██▆▅▂▁▁▄▇█
#> 

# (c) from a 3-d array with dimensions [curve, arg, component]:
arr <- array(c(mx, my), dim = c(3, 50, 2),
             dimnames = list(NULL, NULL, c("x", "y")))
tfd_mv(arr, arg = t)
#> tfd_mv<d=2>[3] (x, y): [0, 1] -> [-0.9994862, 0.9994862] x [-0.9979454, 1]
#> components based on 50 evaluations each, interpolation by tf_approx_linear
#> [1]: ▅▆▇▇██████▇▆▅▄▃▂▁▁▁▁▁▁▂▂▃▄ | ███▇▆▅▄▄▃▂▁▁▁▁▁▁▂▃▄▄▅▆▇███
#> [2]: ▅▇███▆▄▃▂▁▁▂▃▆▇██▇▆▅▃▁▁▁▂▄ | ██▆▄▂▁▁▁▂▄▆▇██▇▆▄▂▁▁▁▂▄▆██
#> [3]: ▅██▇▄▁▁▁▃▆██▆▃▁▁▃▆███▅▂▁▁▄ | █▇▄▁▁▂▅▆██▆▃▁▁▃▆██▆▅▂▁▁▄▇█
#> 

# (d) from a long data.frame (id, arg, one value column per component):
df <- data.frame(
  id = rep(1:3, each = 50),
  arg = rep(t, times = 3),
  x = as.vector(t(mx)),
  y = as.vector(t(my))
)
tfd_mv(df, id = "id", arg = "arg", value = c("x", "y"))
#> tfd_mv<d=2>[3] (x, y): [0, 1] -> [-0.9994862, 0.9994862] x [-0.9979454, 1]
#> components based on 50 evaluations each, interpolation by tf_approx_linear
#> [1]: ▅▆▇▇██████▇▆▅▄▃▂▁▁▁▁▁▁▂▂▃▄ | ███▇▆▅▄▄▃▂▁▁▁▁▁▁▂▃▄▄▅▆▇███
#> [2]: ▅▇███▆▄▃▂▁▁▂▃▆▇██▇▆▅▃▁▁▁▂▄ | ██▆▄▂▁▁▁▂▄▆▇██▇▆▄▂▁▁▁▂▄▆██
#> [3]: ▅██▇▄▁▁▁▃▆██▆▃▁▁▃▆███▅▂▁▁▄ | █▇▄▁▁▂▅▆██▆▃▁▁▃▆██▆▅▂▁▁▄▇█
#> 
```
