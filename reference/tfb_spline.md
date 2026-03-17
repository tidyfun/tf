# Spline-based representation of functional data

Represent curves as a weighted sum of spline basis functions.

## Usage

``` r
tfb_spline(data, ...)

# S3 method for class 'data.frame'
tfb_spline(
  data,
  id = 1,
  arg = 2,
  value = 3,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'matrix'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'numeric'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'list'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'fd'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = FALSE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'fdSmooth'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = FALSE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'tfd'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'tfb'
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)

# Default S3 method
tfb_spline(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  a `matrix`, `data.frame` or `list` of suitable shape, or another
  `tf`-object containing functional data.

- ...:

  arguments to the calls to
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html) setting up the
  basis (and to
  [`mgcv::magic()`](https://rdrr.io/pkg/mgcv/man/magic.html) or
  [`mgcv::gam.fit()`](https://rdrr.io/pkg/mgcv/man/gam.fit.html) if
  `penalized = TRUE`). Uses `k = 25` cubic regression spline basis
  functions (`bs = "cr"`) by default, but should be set appropriately by
  the user. See details and examples in the vignettes.

- id:

  The name or number of the column defining which data belong to which
  function.

- arg:

  For the `list`- and `matrix`-methods: `numeric`, or list of
  `numeric`s. The evaluation grid. For the `data.frame`-method: the
  name/number of the column defining the evaluation grid. The `matrix`
  method will try to guess suitable `arg`-values from the column names
  of `data` if `arg` is not supplied. Other methods fall back on integer
  sequences (`1:<length of data>`) as the default if not provided.

- value:

  The name or number of the column containing the function evaluations.

- domain:

  range of the `arg`.

- penalized:

  `TRUE` (default) estimates regularized/penalized basis coefficients
  via [`mgcv::magic()`](https://rdrr.io/pkg/mgcv/man/magic.html) or
  [`mgcv::gam.fit()`](https://rdrr.io/pkg/mgcv/man/gam.fit.html),
  `FALSE` yields ordinary least squares / ML estimates for basis
  coefficients. `FALSE` is much faster but will overfit for noisy data
  if `k` is (too) large.

- global:

  Defaults to `FALSE`. If `TRUE` and `penalized = TRUE`, all functions
  share the same smoothing parameter (see details).

- verbose:

  `TRUE` (default) outputs statistics about the fit achieved by the
  basis and other diagnostic messages.

## Value

a `tfb`-object

## Details

The basis to be used is set up via a call to
[`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html) and all the spline
bases discussed in
[`mgcv::smooth.terms()`](https://rdrr.io/pkg/mgcv/man/smooth.terms.html)
are available, in principle. Depending on the value of the `penalized`-
and `global`-flags, the coefficient vectors for each observation are
then estimated via fitting a GAM (separately for each observation, if
`!global`) via
[`mgcv::magic()`](https://rdrr.io/pkg/mgcv/man/magic.html) (least square
error, the default) or
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) (if a `family`
argument was supplied) or unpenalized least squares / maximum
likelihood.

After the "smoothed" representation is computed, the amount of smoothing
that was performed is reported in terms of the "percentage of
variability preserved", which is the variance (or the explained
deviance, in the general case if `family` was specified) of the smoothed
function values divided by the variance of the original values (the null
deviance, in the general case). Reporting can be switched off with
`verbose = FALSE`.

The `...` arguments supplies arguments to both the spline basis (via
[`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html)) and the estimation
(via [`mgcv::magic()`](https://rdrr.io/pkg/mgcv/man/magic.html) or
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html)), the most
important arguments are:

- **`k`**: how many basis functions should the spline basis use, default
  is 25.

- **`bs`**: which type of spline basis should be used, the default is
  cubic regression splines (`bs = "cr"`)

- **`family`** argument: use this if minimizing squared errors is not a
  reasonable criterion for the representation accuracy (see
  [`mgcv::family.mgcv()`](https://rdrr.io/pkg/mgcv/man/family.mgcv.html)
  for what's available) and/or if function values are restricted to be
  e.g. positive (`family = Gamma()/tw()/...`), in \\\[0,1\]\\
  (`family = betar()`), etc.

- **`sp`**: numeric value for the smoothness penalty weight, for
  manually setting the amount of smoothing for all curves, see
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html). This (drastically)
  reduces computation time. Defaults to `-1`, i.e., automatic
  optimization of `sp` using
  [`mgcv::magic()`](https://rdrr.io/pkg/mgcv/man/magic.html) (LS fits)
  or [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) (GLM),
  source code in `R/tfb-spline-utils.R`.

If **`global == TRUE`**, this uses a small subset of curves (10`%` of
curves, at least 5, at most 100; non-random sample using every j-th
curve in the data) on which smoothing parameters per curve are estimated
and then takes the mean of the log smoothing parameter of those as `sp`
for all curves. This is much faster than optimizing for each curve on
large data sets. For very sparse or noisy curves, estimating a common
smoothing parameter based on the data for all curves simultaneously is
likely to yield better results, this is *not* what's implemented here.

## Methods (by class)

- `tfb_spline(data.frame)`: convert data frames

- `tfb_spline(matrix)`: convert matrices

- `tfb_spline(numeric)`: convert matrices

- `tfb_spline(list)`: convert lists

- `tfb_spline(fd)`: convert `fd` objects. Almost exact re-representation
  for objects using Fourier- or B-spline bases, other `fda`-style bases
  are not implemented here.

- `tfb_spline(fdSmooth)`: convert `fdSmooth` objects. Almost exact
  re-representation for objects using Fourier- or B-spline bases, other
  `fda`-style bases are not implemented here.

- `tfb_spline(tfd)`: convert `tfd` (raw functional data)

- `tfb_spline(tfb)`: convert `tfb`: modify basis representation,
  smoothing.

- `tfb_spline(default)`: convert `tfb`: default method, returning
  prototype when data is missing

## See also

[`mgcv::smooth.terms()`](https://rdrr.io/pkg/mgcv/man/smooth.terms.html)
for spline basis options.

Other tfb-class:
[`fpc_wsvd()`](https://tidyfun.github.io/tf/reference/fpc_wsvd.md),
[`tfb`](https://tidyfun.github.io/tf/reference/tfb.md),
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md)

## Examples

``` r
arg <- seq(0, 1, length.out = 21)
mat <- rbind(sin(2 * pi * arg), cos(2 * pi * arg))
fit <- tfb_spline(mat, arg = arg, k = 8, penalized = FALSE, verbose = FALSE)
fit
#> tfb[2]: [0,1] -> [-0.9999508,1.019048] in basis representation:
#>  using  s(arg, bs = "cr", k = 8, sp = NA)  
#> 1: ▄▆▇█████▇▆▄▃▂▁▁▁▁▁▂▃▄
#> 2: ███▇▆▄▃▂▁▁▁▁▁▂▃▄▆▇███
```
