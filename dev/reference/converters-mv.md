# Coerce a `tf_mv` to a matrix or data.frame

`as.matrix.tf_mv` returns a **3-d** array `[curve, arg, component]` –
the natural shape for a vector-valued evaluation. This is deliberately
different from
[`as.matrix.tf`](https://tidyfun.github.io/tf/dev/reference/converters.md)
(2-d, `[curve, arg]`); see `@seealso`.

## Usage

``` r
# S3 method for class 'tf_mv'
as.matrix(x, arg, interpolate = FALSE, ...)

# S3 method for class 'tf_mv'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  unnest = FALSE,
  long = TRUE,
  arg = NULL,
  interpolate = TRUE,
  grids = c("union", "component"),
  ...
)
```

## Arguments

- x:

  a `tf_mv` object.

- arg:

  optional evaluation grid (numeric vector or per-curve list). When
  `NULL` (default for `as.data.frame.tf_mv`; equivalent to "missing" for
  `as.matrix.tf_mv`), the per-curve union of all components' native
  argument grids is used.

- interpolate:

  forwarded to the underlying `tf` evaluation. `tfb` components are
  always interpolated.

- ...:

  passed through.

- row.names, optional:

  standard `as.data.frame` plumbing.

- unnest:

  if `TRUE`, return an evaluated data.frame (see `long`); if `FALSE`
  (default), a one-column data.frame wrapping `x`.

- long:

  when `unnest = TRUE`, controls the schema. `long = TRUE` (default)
  returns a 4-column data.frame `(id, arg, component, value)` – the
  multivariate analogue of the univariate `(id, arg, value)` contract,
  with `component` a `factor` over `attr(x, "comp_names")`.
  `long = FALSE` returns the wide `(id, arg, comp1, ..., compd)` schema.

- grids:

  when `unnest = TRUE`, controls *where* components are evaluated when
  they live on different argument grids (for shared grids both settings
  agree). `"union"` (default) evaluates every component on each curve's
  union grid, so components get (interpolated) values at the *other*
  components' arg values inside their observed range. `"component"`
  evaluates each component strictly on its **own** grid (or on `arg`, if
  supplied): no values are fabricated at args a component was not
  observed at – in the long schema such rows are simply absent, in the
  wide schema the other components' columns are `NA` there. Use
  `"union"` for paired evaluations (e.g. trajectory plots),
  `"component"` for faithful tabular exports of the observed data.

## Value

a 3-d array (`as.matrix.tf_mv`) or a data.frame (`as.data.frame.tf_mv`).

## Details

`as.data.frame.tf_mv` returns either a single-column wrapping data.frame
(`unnest = FALSE`, for storing a `tf_mv` in a tibble column) or an
evaluated long/wide data.frame (`unnest = TRUE`).

## See also

[`as.matrix.tf()`](https://tidyfun.github.io/tf/dev/reference/converters.md)
(2-d sibling),
[`as.data.frame.tf()`](https://tidyfun.github.io/tf/dev/reference/converters.md)
(univariate contract),
[`tf_evaluate()`](https://tidyfun.github.io/tf/dev/reference/tf_evaluate.md).

Other tidyfun converters:
[`as.data.frame.tf()`](https://tidyfun.github.io/tf/dev/reference/converters.md)

## Examples

``` r
arg <- seq(0, 1, length.out = 11)
xf <- tfd(t(sapply(1:3, function(i) sin(2 * pi * arg + i))), arg = arg)
yf <- tfd(t(sapply(1:3, function(i) cos(2 * pi * arg + i))), arg = arg)
mv <- tfd_mv(list(x = xf, y = yf))
dim(as.matrix(mv))
#> [1]  3 11  2
head(as.data.frame(mv, unnest = TRUE))
#>   id arg component       value
#> 1  1 0.0         x  0.84147098
#> 2  1 0.0         y  0.54030231
#> 3  1 0.1         x  0.99834605
#> 4  1 0.1         y -0.05749049
#> 5  1 0.2         x  0.77388686
#> 6  1 0.2         y -0.63332387
```
