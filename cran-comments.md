# tf 0.5.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this version

Feature release (v0.4.1 -> v0.5.0): adds classes and methods for
vector-valued functional data (`tfd_mv` / `tfb_mv`), multivariate functional
principal component analysis (`tfb_mfpc`), multivariate/elastic-shape
registration, and geometry verbs, plus assorted bug fixes. See NEWS.md.

## Reverse dependencies

Checked all CRAN reverse dependencies (ehymet, mlr3fda, tidyfun). One
finding:

* mlr3fda: one test asserted the previous (buggy) behavior that all-NA
  functional input silently collapses to a length-0 vector; tf 0.5.0 instead
  returns a length-n vector of NA functions with a warning (tidyfun/tf#241).
  The mlr3fda maintainers were notified and have already adapted the test in
  their development version (mlr-org/mlr3fda@5e6204d, 2026-07-09), ready to
  submit once this version is on CRAN.
