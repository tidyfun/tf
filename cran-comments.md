## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes in this version

Patch update (v0.4.0 -> v0.4.1): no user-facing feature changes.

This resubmission follows shortly after the previous release because R-devel
now warns when packages import both `rlang` and `checkmate` wholesale:
`rlang` newly exports `check_string()` and `check_data_frame()`, which conflict
with `checkmate`. This update switches `tf` from `import(checkmate)` to
selective `importFrom(checkmate, ...)` directives to remove that warning and
keep the package clean on current R-devel / CRAN pretest infrastructure.

## Reverse dependencies

Checked all reverse dependencies. No issues found.
