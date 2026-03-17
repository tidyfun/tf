# `base` plots for `tf`s

Some `base` functions for displaying functional data in spaghetti-
(i.e., line plots) and lasagna- (i.e., heat map) flavors.

## Usage

``` r
# S3 method for class 'tf'
plot(
  x,
  y,
  n_grid = 50,
  points = is_irreg(x),
  type = c("spaghetti", "lasagna"),
  alpha = min(1, max(0.05, 2/length(x))),
  ...
)

# S3 method for class 'tf'
lines(x, arg, n_grid = 50, alpha = min(1, max(0.05, 2/length(x))), ...)

# S3 method for class 'tf'
points(
  x,
  arg,
  n_grid = NA,
  alpha = min(1, max(0.05, 2/length(x))),
  interpolate = FALSE,
  ...
)
```

## Arguments

- x:

  an `tf` object.

- y:

  (optional) numeric vector to be used as `arg` (i.e., for the
  **x**-axis...!).

- n_grid:

  minimal size of equidistant grid used for plotting, defaults to `50`.
  See details.

- points:

  should the original evaluation points be marked by points? Defaults to
  `TRUE` for irregular `tfd` and `FALSE` for all others.

- type:

  `"spaghetti"`: line plots, `"lasagna"`: heat maps.

- alpha:

  alpha-value (see
  [`grDevices::rgb()`](https://rdrr.io/r/grDevices/rgb.html)) for noodle
  transparency. Defaults to 2/(no. of observations). Lower is more
  transparent.

- ...:

  additional arguments for
  [`graphics::matplot()`](https://rdrr.io/r/graphics/matplot.html)
  ("spaghetti") or [`image()`](https://rdrr.io/r/graphics/image.html)
  ("lasagna").

- arg:

  evaluation grid (vector).

- interpolate:

  should functions be evaluated (i.e., inter-/extrapolated) for arg for
  which no original data is available? Only relevant for tfd, defaults
  to `FALSE`.

## Value

the plotted `tf`-object, invisibly.

## Details

If no second argument `y` is given, evaluation points (`arg`) for the
functions are given by the union of the `tf`'s `arg` and an equidistant
grid over its domain with `n_grid` points. If you want to only see the
original data for `tfd`-objects without inter-/extrapolation, use
`n_grid < 1` or `n_grid = NA`.

## References

Swihart, J B, Caffo, Brian, James, D B, Strand, Matthew, Schwartz, S B,
Punjabi, M N (2010). “Lasagna plots: a saucy alternative to spaghetti
plots.” *Epidemiology (Cambridge, Mass.)*, **21**(5), 621–625.

## Examples

``` r
f <- tfd(sin(seq(0, 2 * pi, length.out = 51)), arg = seq(0, 1, length.out = 51))
plot(f)

plot(c(f, 2 * f), type = "lasagna")
```
