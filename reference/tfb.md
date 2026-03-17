# Constructors for functional data in basis representation

Various constructors for `tfb`-vectors from different kinds of inputs.

## Usage

``` r
tfb(data = data_frame0(), basis = c("spline", "fpc", "wavelet"), ...)

tfb_wavelet(data, ...)

as.tfb(data, basis = c("spline", "fpc"), ...)
```

## Arguments

- data:

  a `matrix`, `data.frame` or `list` of suitable shape, or another
  `tf`-object containing functional data.

- basis:

  either "`spline`" (see
  [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md),
  the default) or "`fpc`" (see
  [`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md)).
  (`wavelet` not implemented yet)

- ...:

  further arguments for
  [`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)
  or [`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md).

## Value

a `tfb`-object (or a `data.frame`/`matrix` for the conversion functions,
obviously).

## Details

`tfb` is a wrapper for functions that set up spline-, principal
component- or wavelet-based representations of functional data. For all
three, the input data \\x_i(t)\\ are represented as weighted sums of a
set of common basis functions \\B_k(t); k = 1,\dots, K\\ identical for
all observations and weight or coefficient vectors \\b_i = (b\_{i1},
\dots, b\_{iK})\\ estimated for each observation: \\x_i(t) \approx
\sum_k B_k(t) b\_{ik}\\. Depending on the value of `basis`, the basis
functions \\B(t)\\ will either be `spline` functions or the first few
estimated eigenfunctions of the covariance operator of the \\x(t)\\
(`fpc`) or wavelets (`wavelet`).

See
**[`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)**
for more details on spline basis representation (the default). See
**[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md)** for
using an functional principal component representation with an
orthonormal basis estimated from the data instead.

## See also

Other tfb-class:
[`fpc_wsvd()`](https://tidyfun.github.io/tf/reference/fpc_wsvd.md),
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md),
[`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)

Other tfb-class:
[`fpc_wsvd()`](https://tidyfun.github.io/tf/reference/fpc_wsvd.md),
[`tfb_fpc()`](https://tidyfun.github.io/tf/reference/tfb_fpc.md),
[`tfb_spline()`](https://tidyfun.github.io/tf/reference/tfb_spline.md)

## Examples

``` r
arg <- seq(0, 1, length.out = 21)
x <- tfd(rbind(sin(2 * pi * arg), cos(2 * pi * arg)), arg = arg)
xb <- tfb(x, k = 8, penalized = FALSE)
#> Percentage of input data variability preserved in basis representation
#> (per functional observation, approximate):
#> Min. 1st Qu.  Median Mean 3rd Qu.  Max.
#> 100 100 100 100 100 100
xb
#> tfb[2]: [0,1] -> [-0.9999508,1.019048] in basis representation:
#>  using  s(arg, bs = "cr", k = 8, sp = NA)  
#> 1: ▄▆▇█████▇▆▄▃▂▁▁▁▁▁▂▃▄
#> 2: ███▇▆▄▃▂▁▁▁▁▁▂▃▄▆▇███

as.tfb(x, basis = "spline", k = 8)
#> Percentage of input data variability preserved in basis representation
#> (per functional observation, approximate):
#> Min. 1st Qu.  Median Mean 3rd Qu.  Max.
#> 100 100 100 100 100 100
#> tfb[2]: [0,1] -> [-0.9999447,1.021025] in basis representation:
#>  using  s(arg, bs = "cr", k = 8, sp = -1)  
#> 1: ▄▆▇█████▇▆▄▃▂▁▁▁▁▁▂▃▄
#> 2: ███▇▆▄▃▂▁▁▁▁▁▂▃▄▆▇███
```
