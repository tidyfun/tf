# tf 0.4.0

## New features

* new `tf_split()` / `tf_combine()` for splitting functions into sub-domain
  fragments and re-combining them
* `tf_rgp()` gains a `"brownian_bridge"` covariance option and can generate
  irregular data via the new `irreg` argument
* `tfb_spline()` now converts `fda::fd` and `fda::fdSmooth` objects directly
* mgcv-style Fourier basis constructor for use in `tfb_spline()`
* sparklines in `print()` / `format()` for all `tf` subtypes
* `tf_where()` now defaults to `arg = tf_arg(x)`, consistent with other functions
* `as.matrix()` for irregular `tfd` now always interpolates to a common grid

## Bug fixes

* major overhaul of NA handling: NA entries are now consistently represented as
  `NULL` internally; fixes for `tf_arg()`, `tf_evaluate()`, `tf_smooth()`,
  `tfd()` conversion, `is.na()`, printing, and arithmetic with NA entries
* fix `summary.tf()` returning wrong central range
* fix `tfd` domain enforcement for length-1 data (#139)
* fix printing for empty `tfb` objects and all-NA vectors

## Other changes

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
