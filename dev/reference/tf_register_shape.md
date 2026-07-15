# Register vector-valued curves in elastic shape space

`tf_register_shape()` aligns vector-valued `tf_mv` curves with SRVF
curve alignment, optionally allowing rotations and rescaling in addition
to time warping. Unlike
[`tf_register()`](https://tidyfun.github.io/tf/dev/reference/tf_register.md),
this is a shape-registration interface: the aligned curves live in
centered shape space and the result stores rotations and scale factors.

## Usage

``` r
tf_register_shape(
  x,
  ...,
  template = NULL,
  max_iter = 3L,
  tol = 0.01,
  rotation = TRUE,
  scale = TRUE,
  mode = c("O", "C"),
  lambda = 0,
  store_x = TRUE
)
```

## Arguments

- x:

  a regular shared-grid `tfd_mv` object.

- ...:

  additional arguments passed to fdasrvf alignment routines, such as
  `ncores` or `verbose`.

- template:

  optional length-one `tf_mv` template. If `NULL`, a template is refined
  from the first curve by iterative group alignment.

- max_iter:

  integer: maximum template refinement iterations when
  `template = NULL`. Default `3L`.

- tol:

  numeric: relative template convergence tolerance.

- rotation:

  logical: allow rotations? Default `TRUE`.

- scale:

  logical: allow scale changes? Default `TRUE`.

- mode:

  character: curve mode for fdasrvf. Only open curves (`"O"`, the
  default) are currently supported; `"C"` (closed curves) is reserved
  for a future release (see Details).

- lambda:

  numeric: non-negative elastic penalty passed to the fdasrvf alignment
  routines. Larger values penalise warping. Default `0`.

- store_x:

  logical: store original data in the result object?

## Value

A `tf_shape_registration` object. Access aligned curves with
[`tf_aligned()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
inverse warps with
[`tf_inv_warps()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
the template with
[`tf_template()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
rotations with
[`tf_rotations()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
and scales with
[`tf_scales()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md).

## Details

When `scale = TRUE` the aligned curves returned by
[`tf_aligned()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md)
are renormalised to a common (mean) arc length so that congruent shapes
overlay. The per-curve factors returned by
[`tf_scales()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md)
are the sizes that were removed: multiplying an aligned curve by its
scale factor rescales it back to the corresponding input curve's arc
length, so a value `> 1` means the input curve was larger than the
shared aligned size and `< 1` means it was smaller. With `scale = FALSE`
warping and rotation preserve arc length, so all factors are `1`. With
`template = NULL` the returned
[`tf_template()`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md)
is the empirical mean of the aligned shape-space curves rather than any
single input curve.

Only open curves (`mode = "O"`) are supported. Closed curves
(`mode = "C"`) additionally optimise over a circular seed shift that the
returned warping functions do not represent, which would make the stored
warps inconsistent with the aligned curves; `mode = "C"` is therefore
rejected for now.

## See also

Other registration functions:
[`tf_align()`](https://tidyfun.github.io/tf/dev/reference/tf_align.md),
[`tf_estimate_warps()`](https://tidyfun.github.io/tf/dev/reference/tf_estimate_warps.md),
[`tf_landmarks_extrema()`](https://tidyfun.github.io/tf/dev/reference/tf_landmarks_extrema.md),
[`tf_register()`](https://tidyfun.github.io/tf/dev/reference/tf_register.md),
[`tf_registration`](https://tidyfun.github.io/tf/dev/reference/tf_registration.md),
[`tf_warp()`](https://tidyfun.github.io/tf/dev/reference/tf_warp.md)

## Examples

``` r
t <- seq(0, 1, length.out = 51)
base <- rbind(t, t^2)
beta <- array(NA_real_, dim = c(3, length(t), 2))
for (i in 1:3) {
  beta[i,, 1] <- base[1, ]
  beta[i,, 2] <- base[2, ]
}
curves <- tfd_mv(beta, arg = t)
reg <- tf_register_shape(curves, max_iter = 1)
tf_rotations(reg)
#> , , 1
#> 
#>               v1            v2
#> v1  1.000000e+00 -5.219196e-17
#> v2 -5.458887e-17  1.000000e+00
#> 
#> , , 2
#> 
#>               v1            v2
#> v1  1.000000e+00 -5.219196e-17
#> v2 -5.458887e-17  1.000000e+00
#> 
#> , , 3
#> 
#>               v1            v2
#> v1  1.000000e+00 -5.219196e-17
#> v2 -5.458887e-17  1.000000e+00
#> 
tf_scales(reg)
#> [1] 1.511187 1.511187 1.511187
```
