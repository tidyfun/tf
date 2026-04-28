# Utility functions for `tf`-objects

A bunch of methods & utilities that do what they say: get or set the
respective attributes of a `tf`-object.

## Usage

``` r
tf_arg(f)

tf_evaluations(f)

tf_count(f)

tf_domain(f)

tf_domain(x) <- value

tf_evaluator(f)

tf_evaluator(x) <- value

tf_basis(f, as_tfd = FALSE)

tf_arg(x) <- value

# S3 method for class 'tfd_irreg'
tf_arg(x) <- value

# S3 method for class 'tfd_reg'
tf_arg(x) <- value

# S3 method for class 'tfb'
tf_arg(x) <- value

# S3 method for class 'tfb'
coef(object, ...)

# S3 method for class 'tf'
rev(x)

# S3 method for class 'tf'
is.na(x)

# S3 method for class 'tfd_irreg'
is.na(x)

is_tf(x)

is_tfd(x)

is_reg(x)

is_tfd_reg(x)

is_irreg(x)

is_tfd_irreg(x)

is_tfb(x)

is_tfb_spline(x)

is_tfb_fpc(x)
```

## Arguments

- f:

  an `tf` object.

- x:

  an `tf` object.

- value:

  **for `tf_evaluator<-`:** (bare or quoted) name of a function that can
  be used to interpolate an `tfd`. Needs to accept vector arguments `x`,
  `arg`, `evaluations` and return evaluations of the function defined by
  `arg`, `evaluations` at `x`.  
  **for `tf_arg<-`:** (list of) new `arg`-values.  
  **for `tf_domain<-`:** sorted numeric vector with the 2 new endpoints
  of the domain.  

- as_tfd:

  should the basis be returned as a `tfd`-vector evaluated on
  `tf_arg(f)`? Defaults to `FALSE`, which returns the matrix of basis
  functions (columns) evaluated on `tf_arg(f)` (rows).

- object:

  as usual

- ...:

  dots

## Value

either the respective attribute or, for setters (assignment functions),
the input object with modified properties.

## See also

Other tidyfun utility functions:
[`in_range()`](https://tidyfun.github.io/tf/reference/in_range.md),
[`tf_zoom()`](https://tidyfun.github.io/tf/reference/tf_zoom.md)

## Examples

``` r
x <- tf_rgp(3)
tf_arg(x)
#>  [1] 0.00 0.02 0.04 0.06 0.08 0.10 0.12 0.14 0.16 0.18 0.20 0.22 0.24 0.26 0.28
#> [16] 0.30 0.32 0.34 0.36 0.38 0.40 0.42 0.44 0.46 0.48 0.50 0.52 0.54 0.56 0.58
#> [31] 0.60 0.62 0.64 0.66 0.68 0.70 0.72 0.74 0.76 0.78 0.80 0.82 0.84 0.86 0.88
#> [46] 0.90 0.92 0.94 0.96 0.98 1.00
tf_evaluations(x)
#> $`1`
#>  [1] 1.5135408 1.5465073 1.5369858 1.5697465 1.5714709 1.6353928 1.5856153
#>  [8] 1.6094901 1.6662783 1.6314549 1.6336818 1.6367116 1.6020364 1.6594355
#> [15] 1.6166639 1.5656236 1.5335329 1.4620264 1.4026727 1.3300888 1.2009638
#> [22] 1.0988050 1.0189181 0.8900833 0.8087505 0.7486157 0.6528027 0.6329539
#> [29] 0.5611478 0.5332983 0.5330818 0.5270663 0.5633167 0.6239479 0.6427727
#> [36] 0.6866764 0.6817430 0.7708453 0.8179473 0.8246502 0.9028411 0.9460575
#> [43] 0.9629997 1.0684018 1.0659101 1.1363187 1.1895048 1.1904337 1.2702224
#> [50] 1.3571844 1.4087177
#> 
#> $`2`
#>  [1] 0.2460432 0.2323935 0.1399692 0.1154920 0.1175801 0.1329389 0.1320968
#>  [8] 0.1587012 0.1729748 0.2901721 0.3747257 0.5225041 0.6283878 0.7327980
#> [15] 0.9377491 1.2112754 1.3482925 1.5220939 1.6580685 1.8164518 2.0045371
#> [22] 2.0933358 2.1465323 2.2523857 2.2900022 2.3150377 2.3020997 2.2706018
#> [29] 2.2904270 2.2112746 2.1441833 2.1267001 2.1022045 2.0561951 2.0289924
#> [36] 1.9719107 1.9896746 1.9883448 2.0120667 1.9932549 2.0345371 2.0324733
#> [43] 2.0865893 2.1161068 2.1931689 2.2061105 2.1754907 2.2082579 2.2096478
#> [50] 2.1557483 2.1170930
#> 
#> $`3`
#>  [1] -0.75374233 -0.77775792 -0.76041419 -0.81515851 -0.79056274 -0.77815907
#>  [7] -0.76566509 -0.72777650 -0.74994751 -0.73561565 -0.67613458 -0.65074704
#> [13] -0.59298930 -0.61799721 -0.55300108 -0.50651394 -0.45639877 -0.38024743
#> [19] -0.30507751 -0.27440137 -0.19265989 -0.11675818 -0.02096315  0.03127692
#> [25]  0.11841039  0.23665467  0.29647397  0.36968536  0.43486651  0.52443404
#> [31]  0.57151509  0.66628899  0.68893369  0.77520118  0.80359573  0.86428402
#> [37]  0.86900635  0.91164892  0.87635657  0.83792940  0.78790633  0.70212418
#> [43]  0.62222874  0.49693137  0.40665924  0.25253816  0.07406338 -0.07633862
#> [49] -0.17161658 -0.34688935 -0.48290160
#> 
tf_count(x)
#> [1] 51
tf_domain(x)
#> [1] 0 1
tf_evaluator(x)
#> function (x, arg, evaluations) 
#> {
#>     x_arg <- sort_unique(c(x, arg))
#>     x_arg_match <- match(x_arg, arg, nomatch = length(arg) + 
#>         1)
#>     requested <- x_arg %in% x
#>     dots[[length(dots) + 1]] <- zoo(evaluations[x_arg_match], 
#>         x_arg)
#>     ret <- do.call(f, dots)
#>     coredata(ret)[requested]
#> }
#> <bytecode: 0x55f768c13658>
#> <environment: 0x55f768c0ea18>
tf_evaluate(x, 0.25)
#> $`1`
#> [1] 1.630736
#> 
#> $`2`
#> [1] 0.6805929
#> 
#> $`3`
#> [1] -0.6054933
#> 
tf_evaluator(x) <- tf_approx_none
tf_evaluate(x, 0.25)
#> $`1`
#> [1] NA
#> 
#> $`2`
#> [1] NA
#> 
#> $`3`
#> [1] NA
#> 
c(is_tf(x), is_tfd(x), is_reg(x), is_irreg(x))
#> [1]  TRUE  TRUE  TRUE FALSE

xb <- tfb(x, k = 4, penalized = FALSE, verbose = FALSE)
tf_basis(xb)
#> function (arg) 
#> {
#>     Predict.matrix(object = spec, data = data_frame0(arg = arg))
#> }
#> <bytecode: 0x55f766b93e20>
#> <environment: 0x55f7654a9990>
tf_basis(xb)(c(0, .1, .2))
#>        [,1]   [,2]    [,3]   [,4]
#> [1,] 1.0000 0.0000  0.0000 0.0000
#> [2,] 0.6272 0.4638 -0.1092 0.0182
#> [3,] 0.2976 0.8304 -0.1536 0.0256
c(is_tfb(xb), is_tfb_spline(xb), is_tfb_fpc(xb))
#> [1]  TRUE  TRUE FALSE
```
