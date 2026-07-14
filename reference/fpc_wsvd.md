# Eigenfunctions via weighted, regularized SVD

Compute (truncated) orthonormal eigenfunctions and scores for (partially
missing) data on a common (potentially non-equidistant) grid.

## Usage

``` r
fpc_wsvd(data, arg, pve = 0.995)

# S3 method for class 'matrix'
fpc_wsvd(data, arg, pve = 0.995)

# S3 method for class 'data.frame'
fpc_wsvd(data, arg, pve = 0.995)
```

## Arguments

- data:

  numeric matrix of function evaluations (each row is one curve, no
  NAs).

- arg:

  numeric vector of argument values.

- pve:

  percentage of variance explained.

## Value

a list with entries

- `mu` estimated mean function (numeric vector)

- `efunctions` estimated FPCs (numeric matrix, columns represent FPCs)

- `scores` estimated FPC scores (one row per observed curve)

- `npc` how many FPCs were returned for the given `pve` (integer)

- `scoring_function` a function that returns FPC scores for new data and
  given eigenfunctions, see `tf:::.fpc_wsvd_scores` for an example.

## Details

Performs a weighted SVD with trapezoidal quadrature weights s.t.
returned vectors represent (evaluations of) orthonormal eigen*functions*
\\\phi_j(t)\\, not eigen*vectors* \\\phi_j = (\phi_j(t_1), \dots,
\phi_j(t_n))\\, specifically:  
\\\int_T \phi_j(t)^2 dt \approx \sum_i \Delta_i \phi_j(t_i)^2 = 1\\
given quadrature weights \\\Delta_i\\, not \\\phi_j'\phi_j = \sum_i
\phi_j(t_i)^2 = 1\\;  
\\\int_T \phi_j(t) \phi_k(t) dt = 0\\ not \\\phi_j'\phi_k = \sum_i
\phi_j(t_i)\phi_k(t_i) = 0\\.  
For incomplete data, this uses a soft-impute iterative-SVD scheme (see
references). Note that this will not work well for data on a common grid
if more than a few percent of data points are missing, and it breaks
down completely for truly irregular data with no/few common timepoints,
even if observed very densely. For such data, either re-evaluate on a
common grid first or use more advanced FPCA approaches like
`refund::fpca_sc()`, see last example for
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md)

## References

the soft-impute SVD algorithm for incomplete data is described in
Mazumder, Rahul, Hastie, Trevor, Tibshirani, Robert (2010). “Spectral
Regularization Algorithms for Learning Large Incomplete Matrices.” *The
Journal of Machine Learning Research*, **11**, 2287–2322.

## See also

Other tfb-class:
[`tfb()`](https://tidyfun.github.io/tf/reference/tfb.md),
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md),
[`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)

Other tfb_fpc-class:
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md),
[`tfb_mfpc()`](https://tidyfun.github.io/tf/reference/tfb_mfpc.md)

## Author

Fabian Scheipl

## Examples

``` r
arg <- seq(0, 1, length.out = 41)
data <- t(sapply(seq(0, 2 * pi, length.out = 10),
                 function(p) sin(2 * pi * arg + p)))
fpc <- fpc_wsvd(data, arg = arg, pve = 0.99)
fpc$npc
#> [1] 2
```
