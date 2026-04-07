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
