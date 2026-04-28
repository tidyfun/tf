# Changelog

## tf 0.4.1

CRAN release: 2026-04-07

### Bug fixes

- replace blanket `checkmate` namespace imports with selective imports
  to avoid new R-devel warnings from upstream `rlang` exports
  (`check_string()` and `check_data_frame()`)

## tf 0.4.0

CRAN release: 2026-03-17

### New features

- [`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md)
  & co implement registration methods (SRVF, affine, landmark,
  continuous-criterion) for aligning functions and analyzing phase
  variability. Returns a new `tf_registration` class with accessors
  [`tf_aligned()`](https://tidyfun.github.io/tf/reference/tf_registration.md),
  [`tf_inv_warps()`](https://tidyfun.github.io/tf/reference/tf_registration.md),
  [`tf_template()`](https://tidyfun.github.io/tf/reference/tf_registration.md).
- [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md) now
  implements five functional depth methods: MBD (default), MHI, FM
  (Fraiman-Muniz), FSD (Functional Spatial Depth), and RPD (Regularized
  Projection Depth). MHI ranks functions from lowest (0) to highest (1);
  all other depths are scaled from 0 (most extreme) to 1 (most central).
  MBD is now scaled to \[0, 1\] instead of \[0, 0.5\].
- [`tf_split()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.md)
  /
  [`tf_combine()`](https://tidyfun.github.io/tf/reference/tf_splitcombine.md)
  for splitting functions into sub-domain fragments and re-combining
  them
- [`tf_rgp()`](https://tidyfun.github.io/tf/reference/tf_rgp.md) gains a
  `"brownian_bridge"` covariance option and can generate irregular data
  via the new `irreg` argument
- [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)
  now converts [`fda::fd`](https://rdrr.io/pkg/fda/man/fd.html) and
  `fda::fdSmooth` objects directly
- `mgcv`-style Fourier basis constructor for use in
  [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)
- sparklines in [`print()`](https://rdrr.io/r/base/print.html) /
  [`format()`](https://rdrr.io/r/base/format.html) for all `tf` subtypes
- [`tf_where()`](https://tidyfun.github.io/tf/reference/tf_where.md) now
  defaults to `arg = tf_arg(x)`, consistent with other functions
- [`as.matrix()`](https://rdrr.io/r/base/matrix.html) for irregular
  `tfd` now always interpolates to a common grid

### Bug fixes

- [`tf_derive()`](https://tidyfun.github.io/tf/reference/tf_derive.md)
  for `tfd` objects now uses second-order accurate finite differences,
  preserving the original domain and grid
  ([\#59](https://github.com/tidyfun/tf/issues/59))
- major overhaul of NA handling: NA entries are now consistently
  represented as `NULL` internally; fixes for
  [`tf_arg()`](https://tidyfun.github.io/tf/reference/tfmethods.md),
  [`tf_evaluate()`](https://tidyfun.github.io/tf/reference/tf_evaluate.md),
  [`tf_smooth()`](https://tidyfun.github.io/tf/reference/tf_smooth.md),
  [`tfd()`](https://tidyfun.github.io/tf/reference/tfd.md) conversion,
  [`is.na()`](https://rdrr.io/r/base/NA.html), printing, and arithmetic
  with NA entries
- fix
  [`summary.tf()`](https://tidyfun.github.io/tf/reference/tfsummaries.md)
  returning wrong central range
- fix `tfd` domain enforcement for length-1 data
  ([\#139](https://github.com/tidyfun/tf/issues/139))
- fix printing for empty `tfb` objects and all-NA vectors

### Other changes

- `tf_derive` and `tf_integrate` can now handle `tfb` with non-identity
  link functions by falling back to `tfd`-calculus methods (and
  returning `tfd` objects).
- `tfd_irreg` arithmetic now operates on intersection of arg-values
  instead of failing when arg-values are not identical
- `tfb` arithmetic fix
- dropped `glue` dependency in favour of `cli` / `sprintf`; replaced
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) with
  informative
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  throughout
- use
  [`vctrs::vec_arith()`](https://vctrs.r-lib.org/reference/vec_arith.html)
  for group generics, `vctrs` utils in `[.tf`
- various internal performance improvements
- use `air` formatter for all R code

## tf 0.3.4

CRAN release: 2024-05-22

- bug fix: normalize `tf_crosscov` correctly

## tf 0.3.3

CRAN release: 2024-03-28

- `tf_rebase` behaves more consistently on (irregular) data with
  different lengths
- avoid CRAN issues by streamlining `tfb_fpc` example and skipping
  `tf_rebase` tests
