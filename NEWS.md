# tf 0.4.2

## Bug fixes

* `tf_evaluate()` no longer returns values at the wrong positions when the
  requested arg contains duplicates (#236).
* `tf_integrate(f, definite = FALSE)` for irregular `tfd` with n > 1 no longer
  crashes; the antiderivative's per-curve grids are kept as a list (#237).
* `Math.tfd()` / `Math.tfb()` now forward `...` to the underlying op so
  `round(x, digits)`, `log(x, base)`, `signif(x, digits)`, etc. honor their
  extra arguments instead of silently dropping them (#246).
* `tf_integrate()` on irregular `tfd` no longer silently returns `NA` under
  default limits; for irregular input the defaults are now each curve's own
  observed arg range (i.e., the range of its `tf_arg()` values). Pass explicit
  `lower` / `upper` (or an extrapolating evaluator) to override (#253).

## Vector-valued functional data

This release introduces first-class support for vector-valued (multivariate)
functional data -- functions whose codomain is `R^d` -- alongside the existing
univariate `tfd`/`tfb` classes.

* `tfd_mv()` and `tfb_mv()`: new `vctrs`-based S3 classes for vector-valued
  functional data, holding several component functions per observation on a
  shared domain. Constructors accept named lists of `tfd`/`tfb` vectors or
  list-columns of matrices.
* `tfb_mfpc()` implements multivariate functional principal component analysis
  (Happ & Greven, 2018) for `tf_mv` data: a single set of scalar scores per
  curve shared across all components, with vector-valued eigenfunctions.
  Component weighting is configurable (`"inverse_variance"` default, `"snr"`,
  `"equal"`, or user-supplied). New data can be projected onto a fitted basis
  via `tf_rebase()` / `vec_cast()`. Accessors `tf_mfpc_scores()`,
  `tf_mfpc_efunctions()` and the predicate `is_tfb_mfpc()`.
* Multivariate registration: `tf_register()` gains `method = "srvf_mv"` for
  jointly aligning the components of `tf_mv` curves via the multivariate SRVF
  framework, and `tf_register_shape()` provides elastic shape registration
  (rotation/translation/scale-invariant) via `fdasrvf`.
* New geometry verbs for `tf_mv` (and where meaningful for univariate `tf`):
  `tf_norm()`, `tf_inner()`, `tf_tangent()`, `tf_arclength()`.
* `tf_mv_*` accessors, `tf_split()` / `tf_combine()` extensions and `[`/`[[`
  methods for extracting, replacing and recombining components.

### Contract change

* `is_tf()` now returns `TRUE` for `tf_mv` as well as univariate `tfd`/`tfb`.
  Code that branched on `is_tf()` to mean "univariate `tf`" should switch to
  the new predicate `is_tf_1d()`.

# tf 0.4.1

## Bug fixes

* replace blanket `checkmate` namespace imports with selective imports to avoid
  new R-devel warnings from upstream `rlang` exports (`check_string()` and
  `check_data_frame()`)

# tf 0.4.0

## New features

* `tf_register()` & co implement registration methods (SRVF, affine, landmark,
  continuous-criterion) for aligning functions and analyzing phase variability.
  Returns a new `tf_registration` class with accessors `tf_aligned()`,
  `tf_inv_warps()`, `tf_template()`.
* `tf_depth()` now implements five functional depth methods: MBD (default), MHI,
  FM (Fraiman-Muniz), FSD (Functional Spatial Depth), and RPD (Regularized
  Projection Depth). MHI ranks functions from lowest (0) to highest (1); all
  other depths are scaled from 0 (most extreme) to 1 (most central).
  MBD is now scaled to [0, 1] instead of [0, 0.5].
* `tf_split()` / `tf_combine()` for splitting functions into sub-domain
  fragments and re-combining them
* `tf_rgp()` gains a `"brownian_bridge"` covariance option and can generate
  irregular data via the new `irreg` argument
* `tfb_spline()` now converts `fda::fd` and `fda::fdSmooth` objects directly
* `mgcv`-style Fourier basis constructor for use in `tfb_spline()`
* sparklines in `print()` / `format()` for all `tf` subtypes
* `tf_where()` now defaults to `arg = tf_arg(x)`, consistent with other functions
* `as.matrix()` for irregular `tfd` now always interpolates to a common grid

## Bug fixes

* `tf_derive()` for `tfd` objects now uses second-order accurate finite
  differences, preserving the original domain and grid (#59)
* major overhaul of NA handling: NA entries are now consistently represented as
  `NULL` internally; fixes for `tf_arg()`, `tf_evaluate()`, `tf_smooth()`,
  `tfd()` conversion, `is.na()`, printing, and arithmetic with NA entries
* fix `summary.tf()` returning wrong central range
* fix `tfd` domain enforcement for length-1 data (#139)
* fix printing for empty `tfb` objects and all-NA vectors

## Other changes

* `tf_derive` and `tf_integrate` can now handle `tfb` with non-identity link functions
  by falling back to `tfd`-calculus methods (and returning `tfd` objects).
* `tfd_irreg` arithmetic now operates on intersection of arg-values instead
  of failing when arg-values are not identical
* `tfb` arithmetic fix
* dropped `glue` dependency in favour of `cli` / `sprintf`; 
  replaced `stopifnot()` with informative `cli::cli_abort()` throughout
* use `vctrs::vec_arith()` for group generics, `vctrs` utils in `[.tf`
* various internal performance improvements
* use `air` formatter for all R code

# tf 0.3.4

* bug fix: normalize `tf_crosscov` correctly

# tf 0.3.3

* `tf_rebase` behaves more consistently on (irregular) data with different lengths
* avoid CRAN issues by streamlining `tfb_fpc` example and skipping `tf_rebase` tests
