# Change (basis) representation of a `tf`-object

Apply the representation of one `tf`-object to another; i.e. re-express
it in the other's basis, on its grid, etc.  
Useful for making different functional data objects compatible so they
can be combined, compared or computed with.

## Usage

``` r
tf_rebase(object, basis_from, arg = tf_arg(basis_from), ...)

# S3 method for class 'tfd'
tf_rebase(object, basis_from, arg = tf_arg(basis_from), ...)

# S3 method for class 'tfb'
tf_rebase(object, basis_from, arg = tf_arg(basis_from), ...)
```

## Arguments

- object:

  a `tf` object whose representation should be changed.

- basis_from:

  the `tf` object with the desired basis, `arg`, `evaluator`, etc.

- arg:

  optional new `arg` values, defaults to those of `basis_from`.

- ...:

  forwarded to the `tfb` or `tfd` constructors.

## Value

a `tf`-vector containing the data of `object` in the same representation
as `basis_from` (potentially modified by the arguments given in `...`).

## Details

This uses double dispatch (S3) internally, so the methods defined below
are themselves generics for methods `tf_rebase.tfd.tfd`,
`tf_rebase.tfd.tfb_spline`, `tf_rebase.tfd.tfb_fpc`,
`tf_rebase.tfb.tfd`, `tf_rebase.tfb.tfb` that dispatch on `object_from`.

## Methods (by class)

- `tf_rebase(tfd)`: re-express a `tfd`-vector in the same representation
  as some other `tf`-vector

- `tf_rebase(tfb)`: re-express a `tfb`-vector in the same representation
  as some other `tf`-vector.

## Examples

``` r
x <- tf_rgp(3)
xb <- tfb(x, k = 8, penalized = FALSE, verbose = FALSE)
tf_rebase(tf_rgp(3), xb)
#> tfb[3]: [0,1] -> [-1.307387,1.384965] in basis representation:
#>  using  s(arg, bs = "cr", k = 8, sp = NA)  
#> 1: ▆▆▆▆▆▆▆▆▅▅▄▄▃▂▂▁▁▁▁▁▂▃▄▄▅▆
#> 2: ▂▃▄▆▆▇█████▇▆▆▅▃▂▁▁▁▁▁▁▂▂▃
#> 3: ▆▆▆▇▇▇▇▇▇▇▆▅▄▄▃▂▂▂▂▂▂▃▃▄▄▅
```
