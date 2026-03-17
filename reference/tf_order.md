# Rank, order and sort `tf` vectors

These methods use
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md) to
rank, order, and sort functional data. By default they use the modified
hypograph index (`"MHI"`) which provides an up-down ordering (lowest to
highest). You can also use any of the other depth methods available via
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md), or
supply a custom depth function.

## Usage

``` r
rank(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  ...
)

# Default S3 method
rank(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  ...
)

# S3 method for class 'tf'
rank(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  depth = "MHI",
  ...
)

# S3 method for class 'tf'
xtfrm(x)

# S3 method for class 'tf'
sort(x, decreasing = FALSE, na.last = NA, depth = "MHI", ...)
```

## Arguments

- x:

  a `tf` vector.

- na.last:

  for handling of `NA`s; see
  [`base::rank()`](https://rdrr.io/r/base/rank.html) and
  [`base::sort()`](https://rdrr.io/r/base/sort.html).

- ties.method:

  a character string for handling ties; see
  [`base::rank()`](https://rdrr.io/r/base/rank.html).

- ...:

  passed to
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md)
  (e.g. `arg`).

- depth:

  the depth function to use for ranking. One of the depths available via
  [`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md)
  (default: `"MHI"`) or a function that takes a `tf` vector and returns
  a numeric vector of depth values.

- decreasing:

  logical. Should the sort be decreasing?

## Value

`rank`: a numeric vector of ranks.  
`order`: an integer vector of indices.  
`sort.tf`: a sorted `tf` vector.  
`xtfrm.tf`: a numeric vector of depth values.

## Details

`rank` assigns ranks based on depth values: lower depth values get lower
ranks. For `"MHI"` this gives an ordering from lowest to highest
function. For centrality-based depths (`"MBD"`, `"FM"`, `"FSD"`,
`"RPD"`), the most extreme function gets rank 1 and the most central
gets the highest rank.

`order` returns the permutation which rearranges `x` into ascending
order according to depth.

`sort.tf` returns the sorted `tf` vector.

`xtfrm.tf` returns a numeric vector of MHI depth values, enabling
[`base::order`](https://rdrr.io/r/base/order.html) and
[`base::rank`](https://rdrr.io/r/base/rank.html) to work on `tf`
vectors.

## See also

[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md),
[`min.tf()`](https://tidyfun.github.io/tf/reference/tf_minmax.md),
[`max.tf()`](https://tidyfun.github.io/tf/reference/tf_minmax.md)

Other tidyfun ordering and ranking functions:
[`tf_depth()`](https://tidyfun.github.io/tf/reference/tf_depth.md),
[`tf_minmax`](https://tidyfun.github.io/tf/reference/tf_minmax.md)

## Examples

``` r
x <- tf_rgp(5) + 1:5
rank(x)
#> [1] 2 1 3 4 5
order(x)
#> [1] 2 1 3 4 5
sort(x)
#> tfd[5]: [0,1] -> [-0.8587719,6.066542] based on 51 evaluations each
#> interpolation by tf_approx_linear 
#> [1]: ▄▄▄▄▄▃▃▃▃▂▂▂▂▂▃▂▂▂▂▁▁▁▁▁▁▁
#> [2]: ▁▁▁▁▁▂▂▂▂▂▃▃▃▃▃▃▃▃▃▄▄▄▄▄▃▃
#> [3]: ▇▇▇▆▆▆▆▆▆▆▆▆▆▆▆▆▆▅▅▅▅▅▄▄▄▃
#> [4]: ▅▅▅▅▆▆▆▆▆▇▇▇▇▇▇▇▆▆▆▅▅▅▅▅▄▄
#> [5]: ███████████████▇▇▇▇▇▇▇▇▇██
# use a centrality-based depth instead:
rank(x, depth = "MBD")
#> [1] 3 2 5 4 1
```
