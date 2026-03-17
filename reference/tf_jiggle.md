# Make a `tf` (more) irregular

Randomly create some irregular functional data from regular ones.

- **jiggle** it by randomly moving around its `arg`-values inside the
  intervals defined by its grid neighbors on the original argument grid.

- **sparsify** it by removing (100\*`dropout`)% of the function values

## Usage

``` r
tf_jiggle(f, amount = 0.4, ...)

tf_sparsify(f, dropout = 0.5)
```

## Arguments

- f:

  a `tfd` object.

- amount:

  how far away from original grid points can the jiggled grid points
  lie, at most (relative to original distance to neighboring grid
  points). Defaults to at most 40% (0.4) of the original grid distances.
  Must be lower than 0.5.

- ...:

  additional args for the returned `tfd` in `tf_jiggle`.

- dropout:

  what proportion of values of `f` to drop, on average. Defaults to
  half.

## Value

an (irregular) `tfd` object.

## See also

Other tidyfun RNG functions:
[`tf_rgp()`](https://tidyfun.github.io/tf/reference/tf_rgp.md)

## Examples

``` r
set.seed(1)
(x <- tf_rgp(2, arg = 21L))
#> tfd[2]: [0,1] -> [-0.7703284,1.490023] based on 21 evaluations each
#> interpolation by tf_approx_linear 
#> 1: ▃▃▄▅▆▆▆▇▆▆▅▅▄▄▄▅▆▇███
#> 2: ▃▂▁▁▁▁▁▂▂▃▃▃▂▂▂▃▃▄▄▄▄
(x_jig <- tf_jiggle(x, amount = 0.2))
#> irregular tfd[2]: [0,1] -> [-0.7703284,1.490023] based on 21 to 21 (mean: 21) evaluations each
#> interpolation by tf_approx_linear 
#> 1: (0.019,-0.176);(0.040, 0.045);(0.106, 0.233); ...
#> 2: (0.023, -0.16);(0.053, -0.42);(0.093, -0.66); ...
(x_sp <- tf_sparsify(x, dropout = 0.3))
#> irregular tfd[2]: [0,1] -> [-0.7703284,1.490023] based on 13 to 18 (mean: 16) evaluations each
#> interpolation by tf_approx_linear 
#> 1: (0.00,-0.176);(0.05, 0.045);(0.15, 0.490); ...
#> 2: (0.05, -0.42);(0.10, -0.66);(0.15, -0.73); ...
c(is_irreg(x_jig), is_irreg(x_sp))
#> [1] TRUE TRUE
```
