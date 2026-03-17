# Gaussian Process random generator

Generates `n` realizations of a zero-mean Gaussian process. The function
also accepts user-defined covariance functions (without "nugget" effect,
see `cov`), The implemented defaults with `scale` parameter \\\phi\\,
`order` \\o\\ and `nugget` effect variance \\\sigma^2\\ are:

- *squared exponential*: \\Cov(x(t), x(t')) = \exp(-(t-t')^2)/\phi) +
  \sigma^2 \delta\_{t}(t')\\.

- *Wiener* process: \\Cov(x(t), x(t')) = \min(t',t)/\phi + \sigma^2
  \delta\_{t}(t')\\,

- [*Matèrn*
  process](https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function#Definition):
  \\Cov(x(t), x(t')) = \tfrac{2^{1-o}}{\Gamma(o)}
  (\tfrac{\sqrt{2o}\|t-t'\|}{\phi})^o
  \text{Bessel}\_o(\tfrac{\sqrt{2o}\|t-t'\|}{s}) + \sigma^2
  \delta\_{t}(t')\\

- [*Brownian Bridge*
  process](https://en.wikipedia.org/wiki/Brownian_bridge) for \\t, t'
  \in \[a, b\]\\: \\Cov(x(t), x(t')) = \frac{(b -
  \max(s,t))(\min(s, t) - a)}{\phi (b - a)} + \sigma^2 \delta\_{t}(t')\\

## Usage

``` r
tf_rgp(
  n,
  arg = 51L,
  cov = c("squareexp", "wiener", "matern", "brown_bridge"),
  scale = diff(domain)/10,
  nugget = scale/200,
  order = 1.5,
  domain = NULL
)
```

## Arguments

- n:

  how many realizations to draw.

- arg:

  vector of evaluation points (`arg` of the return object). Defaults to
  (0, 0.02, 0.04, ..., 1). If given as a single **integer** (don't
  forget the **`L`**...), creates a regular grid of that length over
  (0,1). If given as a `n`-long list of vectors, irregular functional
  data are created.

- cov:

  type of covariance function to use. Implemented defaults are
  `"squareexp"`, `"wiener"`, `"matern"`, see description. Can also be
  any vectorized function returning \\Cov(x(t), x(t'))\\ *without nugget
  effect* for pairs of inputs t and t'.

- scale:

  scale parameter (see description). Defaults to the width of the domain
  divided by 10.

- nugget:

  nugget effect for additional white noise / unstructured variability.
  Defaults to `scale/200` (so: very little white noise).

- order:

  order of the Matèrn covariance (if used, must be \>0), defaults to
  1.5. The higher, the smoother the process. Evaluation of the
  covariance function becomes numerically unstable for large (\>20)
  `order`, use "squareexp".

- domain:

  of the generated functions. If not provided, the range of the supplied
  `arg` values.

## Value

an `tfd`-vector of length `n`.

## See also

Other tidyfun RNG functions:
[`tf_jiggle()`](https://tidyfun.github.io/tf/reference/tf_jiggle.md)

## Examples

``` r
(x1 <- tf_rgp(10, cov = "squareexp", nugget = 0))
#> tfd[10]: [0,1] -> [-2.394052,3.134049] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▇▆▆▅▅▅▅▅▅▅▅▆▆▆▆▆▅▅▄▄▃▃▂▂▂▂
#> 2: ▃▃▃▄▄▄▄▄▄▄▄▄▃▃▃▂▂▂▁▁▁▁▁▁▂▂
#> 3: ▄▄▄▄▄▄▅▅▅▆▆▆▆▆▆▆▆▅▅▄▃▃▂▂▂▂
#> 4: ▄▄▄▄▄▅▅▅▅▅▅▅▅▅▅▄▄▄▄▃▃▃▃▂▂▂
#> 5: ▂▂▂▂▂▂▃▃▃▃▃▃▂▂▂▂▁▁▁▁▁▁▂▂▂▃
#> 6: ▆▆▆▆▆▆▅▅▄▄▃▃▃▃▃▃▄▄▅▆▆▆▆▆▆▆
#>     [....]   (4 not shown)
tf_rgp(2, arg = list(sort(runif(25)), sort(runif(34))))
#> irregular tfd[2]: [0.0005705222,0.9909104] -> [-1.634664,1.179332] based on 25 to 34 (mean: 30) evaluations each
#> interpolation by tf_approx_linear 
#> 1: (0.00057,0.27);(0.05877,0.50);(0.12418,0.77); ...
#> 2: ( 0.0026,-1.6);( 0.0857,-1.6);( 0.0858,-1.6); ...
```
