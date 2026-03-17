# Estimate warping functions for registration

`tf_estimate_warps()` is the low-level workhorse for functional data
registration. It estimates warping functions that align a set of
functions to a template, but does *not* apply them. For a one-shot
interface that also aligns the data, see
[`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md).

## Usage

``` r
tf_estimate_warps(
  x,
  ...,
  template = NULL,
  method = c("srvf", "cc", "affine", "landmark"),
  max_iter = 3L,
  tol = 0.01
)
```

## Arguments

- x:

  a `tf` vector of functions to register.

- ...:

  additional method-specific arguments passed to backend routines (for
  example `crit` for `method = "cc"`).

- template:

  an optional `tf` vector of length 1 to use as the template. If `NULL`,
  a default template is computed (method-dependent). Not used for
  `method = "landmark"`.

- method:

  the registration method to use:

  - `"srvf"`: Square Root Velocity Framework (elastic registration). For
    details, see
    [`fdasrvf::time_warping()`](https://rdrr.io/pkg/fdasrvf/man/time_warping.html).
    Default template is the Karcher mean.

  - `"cc"`: continuous-criterion registration via a tf-native dense-grid
    optimizer with monotone spline warps. Default template is the
    arithmetic mean.

  - `"affine"`: affine (linear) registration with warps of the form
    \\h(t) = a \cdot t + b\\. Simpler than elastic registration,
    appropriate when phase variability consists only of shifts and/or
    uniform speed-up/slow-down. Default template is the arithmetic mean.

  - `"landmark"`: piecewise-linear warps that align user-specified
    landmark features. Requires `landmarks` argument.

- max_iter:

  integer: maximum number of Procrustes-style template refinement
  iterations when `template = NULL`. The iteration cycle is: (1)
  estimate template as mean of (aligned) curves, (2) register all curves
  to current template, (3) update template as mean of newly aligned
  curves, (4) repeat until convergence or `max_iter` reached. Ignored
  when `template` is provided (no refinement needed) or for
  `method = "landmark"` (template not used). For `method = "srvf"` with
  `template = NULL`, the outer Procrustes loop is skipped regardless of
  `max_iter` because
  [`fdasrvf::time_warping()`](https://rdrr.io/pkg/fdasrvf/man/time_warping.html)
  already computes the Karcher mean internally. Default is `3L`.

- tol:

  numeric: convergence tolerance for template refinement. For
  `method = "cc"`, iteration stops when the relative improvement in the
  registration criterion becomes negligible; for the other iterative
  methods, iteration stops when the relative change in the template (L2
  norm) falls below `tol`. Default is `1e-2`.

## Value

`tfd` vector of (forward) warping functions \\h_i(s) = t\\ with the same
length as `x`. Apply with
[`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.md) to
obtain registered functions, or use
[`tf_invert()`](https://tidyfun.github.io/tf/reference/tf_invert.md) to
obtain inverse warps \\h_i^{-1}(t) = s\\. The returned warps carry an
`attr(, "template")` with the template used (`NULL` for landmark
registration, which has no template).

## Details

For `method = "cc"`, `tf` uses a tf-native dense-grid optimizer with
monotone spline warps. Each warp is represented as the normalized
cumulative integral of `exp(eta(t))`, where `eta(t)` is a spline with
`nbasis` coefficients. Registration is then carried out curve-by-curve
by minimizing either an integrated squared-error criterion (`crit = 1`)
or the first-eigenfunction variance criterion (`crit = 2`) plus an
optional spline roughness penalty (`lambda`). The outer `max_iter` loop,
when `template = NULL`, still performs the same Procrustes-style
template refinement as the other methods.

## Important method-specific arguments (passed via `...`)

**For `method = "srvf"`:**

- `lambda`:

  non-negative number: penalty controlling the flexibility of warpings
  (default is `0` for unrestricted warps).

- `penalty_method`:

  cost function used to penalize warping functions. Defaults to
  `"roughness"` (norm of their second derivative), `"geodesic"` uses the
  geodesic distance to the identity and `"norm"` uses Euclidean distance
  to the identity.

**For `method = "cc"`:**

- `nbasis`:

  integer: number of B-spline basis functions for the monotone warp
  basis (default `6L`, minimum 2).

- `lambda`:

  non-negative number: roughness penalty for the warp basis (default `0`
  for unpenalized warping).

- `crit`:

  registration criterion. Defaults to `2` for the first-eigenfunction
  variance criterion; alternative is `1` for integrated squared error.

- `conv`:

  non-negative convergence tolerance for the inner optimizer. Default is
  `1e-4`.

- `iterlim`:

  maximum number of inner optimization iterations per curve. Default is
  `20L`.

**For `method = "affine"`:**

- `type`:

  character: `"shift"` (translation only), `"scale"` (scaling only), or
  `"shift_scale"` (both). Default is `"shift"`.

- `shift_range`:

  numeric(2): bounds for shift parameter. Default is
  `c(-range/2, range/2)` where range is the domain width. Larger bounds
  allow greater shifts but may result in more `NA` values.

- `scale_range`:

  numeric(2): bounds for scale parameter. Default is `c(0.5, 2)`. Must
  have `lower > 0`.

**For `method = "landmark"`:**

- `landmarks`:

  **(required)** numeric matrix of landmark positions with one row per
  function and one column per landmark. Use
  [`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/reference/landmarks.md)
  to find peaks/valleys automatically.

- `template_landmarks`:

  numeric vector of target landmark positions. Default is column-wise
  mean of `landmarks`.

## See also

Other registration functions:
[`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.md),
[`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/reference/landmarks.md),
[`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md),
[`tf_registration`](https://tidyfun.github.io/tf/reference/tf_registration.md),
[`tf_warp()`](https://tidyfun.github.io/tf/reference/tf_warp.md)

## Author

Maximilian Muecke, Fabian Scheipl, Claude Opus 4.6

## Examples

``` r
# \donttest{
# see tf_register() for full registration examples
set.seed(1)
f <- tf_rgp(5)
warps <- tf_estimate_warps(f, method = "srvf")
plot(warps)

# }
```
