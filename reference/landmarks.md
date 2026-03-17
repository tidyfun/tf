# Find Extrema Locations in Functional Data

Find landmark locations for registration

## Usage

``` r
tf_landmarks_extrema(x, which = "all", threshold = 0.5, boundary_tol = NULL)

detect_landmarks(x, arg_list, which)

cluster_landmarks(features, n, bandwidth, threshold)

build_landmark_matrix(features, clusters, n, bandwidth)
```

## Arguments

- x:

  tf object (already smoothed if needed)

- which:

  character vector: subset of c("max", "min", "zero")

- threshold:

  minimum proportion of curves for a cluster to be retained

- boundary_tol:

  numeric: features within this distance of the domain boundary are
  dropped (they are redundant with the boundary anchors in landmark
  registration). Defaults to 2x the grid spacing. Set to `0` to keep all
  features.

- arg_list:

  list of numeric vectors: per-curve evaluation grids

- features:

  list of per-curve data.frames

- n:

  number of curves

- bandwidth:

  matching distance

- clusters:

  data.frame from cluster_landmarks()

## Value

A numeric matrix with one row per function and one column per landmark,
sorted left-to-right on the domain. Has attribute `"feature_types"`
(character vector of `"max"`, `"min"`, or `"zero"` for each column).
Contains `NA` where a curve is missing a landmark.

`detect_landmarks`: list of n data.frames with columns (position, type)

`cluster_landmarks`: data.frame with columns: center, type, count,
proportion

`build_landmark_matrix`: n x k matrix with feature_types attribute

## Details

Detects local maxima, minima, and/or zero crossings in each function and
returns a landmark matrix suitable for
[`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md)
with `method = "landmark"`. Uses position-based clustering across curves
to establish feature correspondence and majority-count filtering to
discard unstable landmarks.

- `detect_landmarks` detects local extrema and zero crossings per curve.

&nbsp;

- `cluster_landmarks` clusters within each feature type separately (max
  with max, min with min, etc.) to avoid merging adjacent features of
  different types. Then combines and sorts by position.

&nbsp;

- `build_landmark_matrix` creates a landmark matrix by matching
  per-curve features to clusters.

## See also

[`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md)
with `method = "landmark"`

Other registration functions:
[`tf_align()`](https://tidyfun.github.io/tf/reference/tf_align.md),
[`tf_estimate_warps()`](https://tidyfun.github.io/tf/reference/tf_estimate_warps.md),
[`tf_register()`](https://tidyfun.github.io/tf/reference/tf_register.md),
[`tf_registration`](https://tidyfun.github.io/tf/reference/tf_registration.md),
[`tf_warp()`](https://tidyfun.github.io/tf/reference/tf_warp.md)

## Examples

``` r
t <- seq(0, 1, length.out = 101)
x <- tfd(t(sapply(c(0.3, 0.5, 0.7), function(s) dnorm(t, s, 0.1))), arg = t)
tf_landmarks_extrema(x, "max")
#> Warning: No stable landmarks detected across curves.
#> ℹ Pre-smoothing with `tf_smooth()` can help suppress spurious features.
#>     
#> [1,]
#> [2,]
#> [3,]
#> attr(,"feature_types")
#> character(0)
tf_landmarks_extrema(x, "both")
#> Warning: No stable landmarks detected across curves.
#> ℹ Pre-smoothing with `tf_smooth()` can help suppress spurious features.
#>     
#> [1,]
#> [2,]
#> [3,]
#> attr(,"feature_types")
#> character(0)
```
