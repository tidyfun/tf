# Functional Data Depth

Data depths for functional data. All depths are scaled so that 1 means
most central and 0 means most extreme. Available methods:

## Usage

``` r
tf_depth(x, arg, depth = "MBD", na.rm = TRUE, ...)

# S3 method for class 'matrix'
tf_depth(
  x,
  arg,
  depth = c("MBD", "MHI", "FM", "FSD", "RPD"),
  na.rm = TRUE,
  ...
)

# S3 method for class 'tf'
tf_depth(x, arg, depth = "MBD", na.rm = TRUE, ...)
```

## Arguments

- x:

  `tf` (or a matrix of evaluations).

- arg:

  grid of evaluation points.

- depth:

  one of `"MBD"`, `"MHI"`, `"FM"`, `"FSD"`, or `"RPD"`.

- na.rm:

  remove missing observations? Defaults to `TRUE`.

- ...:

  for `"RPD"`: `u` (regularization quantile, default 0.01),
  `n_projections` (M, default 5000), `n_projections_beta` (L, default
  500).

## Value

vector of depth values

## Details

- `"MBD"`: Modified Band-2 Depth (default). Scale of 0 (most extreme) to
  1 (most central).

- `"MHI"`: Modified Hypograph Index. **Ranks functions from lowest (0)
  to highest (1)** instead of most extreme to most central! For
  functions that never cross: \\MBD = -2(MHI - 0.5)^2 + .5\\.

- `"FM"`: Fraiman-Muniz depth. Integrates pointwise univariate halfspace
  depths over the domain. Scale of 0 (most extreme) to 1 (most central).

- `"FSD"`: Functional Spatial Depth. Based on spatial signs; robust to
  outliers. Scale of 0 (most extreme) to 1 (most central).

- `"RPD"`: Regularized Projection Depth. Projects curves onto random
  directions and computes outlyingness. Especially useful for detecting
  shape outliers. Scale of 0 (most extreme) to 1 (most central).
  **Note:** results depend on the RNG state; set a seed (e.g.
  `set.seed(...)`) before calling for reproducibility. Accepts
  additional arguments via `...`: `u` (quantile level for
  regularization, default 0.01), `n_projections` (M, number of
  projection directions, default 5000), `n_projections_beta` (L,
  directions for estimating regularization parameter, default 500).

## References

Sun, Ying, Genton, G M, Nychka, W D (2012). “Exact fast computation of
band depth for large functional datasets: How quickly can one million
curves be ranked?” *Stat*, **1**(1), 68–74.

López-Pintado, Sara, Romo, Juan (2009). “On the concept of depth for
functional data.” *Journal of the American Statistical Association*,
**104**(486), 718–734.

López-Pintado, Sara, Romo, Juan (2011). “A half-region depth for
functional data.” *Computational Statistics & Data Analysis*, **55**(4),
1679–1695.

Fraiman, Ricardo, Muniz, Graciela (2001). “Trimmed means for functional
data.” *Test*, **10**(2), 419–440.

Chakraborty, Anirvan, Chaudhuri, Probal (2014). “The spatial
distribution in infinite dimensional spaces and related quantiles and
depths.” *The Annals of Statistics*, **42**(3), 1203–1231.

Bočinec, Filip, Nagy, Stanislav, Yeon, Hyemin (2026). “Projection depth
for functional data: Practical issues, computation and applications.”
*arXiv preprint arXiv:2602.22877*.

## See also

Other tidyfun ordering and ranking functions:
[`tf_minmax`](https://tidyfun.github.io/tf/reference/tf_minmax.md),
[`tf_order`](https://tidyfun.github.io/tf/reference/tf_order.md)

## Examples

``` r
x <- tf_rgp(3)/3 + 1:3
tf_depth(x, depth = "MBD")
#> [1] 1.333333 2.000000 1.333333
tf_depth(x, depth = "MHI")
#> [1] 0.3333333 0.6666667 1.0000000
tf_depth(x, depth = "FM")
#> [1] 0.6666667 0.6666667 0.0000000
tf_depth(x, depth = "FSD")
#> [1] 0.3335162 0.9426997 0.3346406
```
