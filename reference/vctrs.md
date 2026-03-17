# `vctrs` methods for `tf` objects

These functions are the extensions that allow `tf` vectors to work with
`vctrs`.

## Usage

``` r
# S3 method for class 'tfd_reg.tfd_reg'
vec_cast(x, to, ...)

# S3 method for class 'tfd_reg.tfd_irreg'
vec_cast(x, to, ...)

# S3 method for class 'tfd_reg.tfb_spline'
vec_cast(x, to, ...)

# S3 method for class 'tfd_reg.tfb_fpc'
vec_cast(x, to, ...)

# S3 method for class 'tfd_irreg.tfd_reg'
vec_cast(x, to, ...)

# S3 method for class 'tfd_irreg.tfd_irreg'
vec_cast(x, to, ...)

# S3 method for class 'tfd_irreg.tfb_spline'
vec_cast(x, to, ...)

# S3 method for class 'tfd_irreg.tfb_fpc'
vec_cast(x, to, ...)

# S3 method for class 'tfb_spline.tfb_spline'
vec_cast(x, to, ...)

# S3 method for class 'tfb_spline.tfb_fpc'
vec_cast(x, to, ...)

# S3 method for class 'tfb_fpc.tfb_spline'
vec_cast(x, to, ...)

# S3 method for class 'tfb_fpc.tfb_fpc'
vec_cast(x, to, ...)

# S3 method for class 'tfb_spline.tfd_reg'
vec_cast(x, to, ...)

# S3 method for class 'tfb_spline.tfd_irreg'
vec_cast(x, to, ...)

# S3 method for class 'tfb_fpc.tfd_reg'
vec_cast(x, to, ...)

# S3 method for class 'tfb_fpc.tfd_irreg'
vec_cast(x, to, ...)

# S3 method for class 'tfd_reg.tfd_reg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_reg.tfd_irreg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_reg.tfb_spline'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_reg.tfb_fpc'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_irreg.tfd_reg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_irreg.tfd_irreg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_irreg.tfb_spline'
vec_ptype2(x, y, ...)

# S3 method for class 'tfd_irreg.tfb_fpc'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_spline.tfb_spline'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_spline.tfb_fpc'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_spline.tfd_reg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_spline.tfd_irreg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_fpc.tfb_spline'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_fpc.tfb_fpc'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_fpc.tfd_reg'
vec_ptype2(x, y, ...)

# S3 method for class 'tfb_fpc.tfd_irreg'
vec_ptype2(x, y, ...)
```

## Arguments

- x:

  Vectors to cast.

- to:

  Type to cast to. If `NULL`, `x` will be returned as is.

- ...:

  For `vec_cast_common()`, vectors to cast. For `vec_cast()`,
  `vec_cast_default()`, and `vec_restore()`, these dots are only for
  future extensions and should be empty.

- y:

  vectors to cast.

## Value

for `vec_cast`: the casted `tf`-vector, for `vec_ptype2`: the common
prototype

## Details

**Notes on `vec_cast`:** Use
[`tf_rebase()`](https://tidyfun.github.io/tf/reference/tf_rebase.md) to
change the representations of `tf`-vectors, these methods are only for
internal use – automatic/implicit casting of `tf` objects is tricky
because it's hard to determine automatically whether such an operation
would lose precision (different bases with different expressivity?
different argument grids?), and it's not generally clear which instances
of which `tf`-subclasses should be considered the "richer" objects.
Rules for casting:

- If the casted object's `domain` would not contain the entire original
  `domain`, no casting is possible (would lose data).

- Every cast that evaluates (basis) functions on different `arg` values
  is a *lossy* cast, since it might lose precision
  ([`vctrs::maybe_lossy_cast`](https://vctrs.r-lib.org/reference/maybe_lossy_cast.html)).

- As long as the casted object's `domain` contains the entire original
  `domain`:

  - every `tfd_reg`, `tfd_irreg` or `tfb` can always be cast into an
    equivalent `tfd_irreg` (which may also change its `evaluator` and
    `domain`).

  - every `tfd_reg` can always be cast to `tfd_reg` (which may change
    its `evaluator` and `domain`)

  - every `tfb` can be cast *losslessly* to `tfd` (regular or irregular,
    note it's lossless only on the *original* `arg`-grid)

- Any cast of a `tfd` into `tfb` is potentially *lossy* (because we
  don't know how expressive the chosen basis is)

- Only `tfb` with identical bases and domains can be cast into one
  another *losslessly*

## See also

[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html),
[`vctrs::vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.html)
