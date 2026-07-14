# Savitzky-Golay smoothing filter

Local polynomial least-squares smoother. Re-implementation of
[`pracma::savgol()`](https://rdrr.io/pkg/pracma/man/savgol.html) to
avoid the extra dependency; numerically equivalent (Savitzky & Golay
1964).

## Usage

``` r
savgol(T, fl, forder = 4, dorder = 0)
```

## Arguments

- T:

  a numeric vector to smooth.

- fl:

  filter window length (odd integer \> 1, must be greater than
  `forder`).

- forder:

  polynomial order of the local fit (non-negative integer, default 4).

- dorder:

  derivative order (non-negative integer not greater than `forder`,
  default 0).

## Value

a smoothed numeric vector of the same length as `T`.
