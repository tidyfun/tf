# tf 0.3.5
* use sparklines for `print()/format()` for regular functional data
* `tfd_irreg`-arithmetic now operates on intersection of arg-values instead of failing
  if arg-values are not identical
* added `fda::fdSmooth` converter to `tfb`, implemented mgcv-style Fourier basis
* fix broken `tfb`-arithmetic
* more `vctrs`-compliance: use `vec_arith` for group generics,
   use `vctrs`-utils in `[.tf`. 
* use `cli` for user communication & export `format_glimpse` dynamically
* use `air` formatter for all R code

# tf 0.3.4
* bug fix: normalize `tf_crosscov` correctly

# tf 0.3.3
* `tf_rebase` behaves more consistently on (irregular) data with different lengths
* avoid CRAN issues by streamlining tfb_fpc example and skipping `tf_rebase` tests
