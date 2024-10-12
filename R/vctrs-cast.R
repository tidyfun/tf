assert_domain_x_in_to <- function(x, to) {
  dom_x <- tf_domain(x)
  dom_to <- tf_domain(to)
  # can (try to) cast losslessly if domain of 'to' contains domain of 'x'
  if ((dom_to[1] <= dom_x[1]) && (dom_to[2] >= dom_x[2])) {
    return(TRUE)
  }
  stop_incompatible_cast(x = x, to = to, x_arg = "", to_arg = "",
                         details = "domains not compatible")
}

assert_same_domains <- function(x, to) {
  if (all(tf_domain(x) == tf_domain(to))) return(TRUE)
  stop_incompatible_cast(x = x, to = to, x_arg = "", to_arg = "",
                         details = "domains not identical")
}

same_args <- function(x, to) {
  arg_x <- tf_arg(x)
  arg_to <- tf_arg(to)
  min_resolution <- c(get_resolution(arg_x), get_resolution(arg_to)) |> min()
  all.equal(tf_arg(to),  tf_arg(x),
            check.attributes = FALSE, tolerance = min_resolution) |>
    isTRUE()
}

#----------------- s3 generics for tfd casting -----------------#

#' `vctrs` methods for `tf` objects
#'
#' These functions are the extensions that allow `tf` vectors
#' to work with `vctrs`.
#'
#' **Notes on `vec_cast`:**
#' Use [tf_rebase()] to change the representations of `tf`-vectors,
#' these methods are only for internal use --
#' automatic/implicit casting of `tf` objects is tricky
#' because it's hard to determine automatically whether such an operation would
#' lose precision (different bases with different expressivity? different
#' argument grids?), and it's not generally clear which instances of which
#' `tf`-subclasses should be considered the "richer" objects.
#' Rules for casting:
#'
#' - If the casted object's `domain` would not contain the entire original `domain`,
#'   no casting is possible (would lose data).
#' - Every cast that evaluates (basis) functions on different `arg` values is a *lossy* cast,
#'   since it might lose precision (`vctrs::maybe_lossy_cast`).
#' - As long as the casted object's `domain` contains the entire original `domain`:
#'    - every `tfd_reg`, `tfd_irreg` or `tfb` can always be cast into an equivalent
#'   `tfd_irreg` (which may also change its `evaluator` and `domain`).
#'   - every `tfd_reg` can always be cast to `tfd_reg` (which may change its `evaluator` and `domain`)
#'   - every `tfb` can be cast *losslessly* to `tfd` (regular or irregular,
#'     note it's lossless only on the *original* `arg`-grid)
#' - Any cast of a `tfd` into `tfb` is potentially *lossy* (because we don't know how expressive the chosen basis is)
#' - Only `tfb` with identical bases and domains can be cast into one another *losslessly*
#'
#' @name vctrs
#' @family tidyfun vctrs
#' @inheritParams vctrs::vec_cast
#' @param y Vectors to cast.
#' @returns for `vec_cast`: the casted `tf`-vector, for `vec_ptype2`: the common prototype
#' @seealso [vctrs::vec_cast()], [vctrs::vec_ptype2()]
NULL

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_reg.tfd_reg <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x))
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_reg.tfd_irreg <- function(x, to, ...) {
  regular_x <- all(duplicated(tf_arg(x))[-1])
  if (!regular_x) {
    stop_incompatible_cast(x = x, to = to, x_arg = "", to_arg = "")
  }
  tf_rebase(x, to, arg = tf_arg(x)[[1]])
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_reg.tfb_spline <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x))
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_reg.tfb_fpc <- vec_cast.tfd_reg.tfb_spline

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_irreg.tfd_reg <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x)) |> as.tfd_irreg()
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_irreg.tfd_irreg <- vec_cast.tfd_irreg.tfd_reg

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_irreg.tfb_spline <- vec_cast.tfd_irreg.tfd_reg

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfd_irreg.tfb_fpc <- vec_cast.tfd_irreg.tfd_reg

#-------------------------------------------------------------------------------

vec_cast_tfb_tfb <- function(x, to, ...) {
  assert_same_domains(x, to)
  same_basis <- isTRUE(all.equal(tf_basis(to)(tf_arg(x)),
                                 attr(x, "basis_matrix"),
                                 check.attributes = FALSE))
  if (same_basis) return(x)
  maybe_lossy_cast(tf_rebase(x, to, arg = tf_arg(x)),
                   x, to, lossy = TRUE, locations = 1:vec_size(x),
                   x_arg = "", to_arg = "",
                   loss_type = "precision",
                   message = "result represented in new basis")
}

vec_cast_tfb_tfd <- function(x, to, ...) {
  maybe_lossy_cast(tf_rebase(x, to, arg = tf_arg(x)),
                   x, to, lossy = TRUE, locations = 1:vec_size(x),
                   x_arg = "", to_arg = "",
                   loss_type = "precision",
                   message = "result represented in new basis")
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_spline.tfb_spline <- vec_cast_tfb_tfb

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_spline.tfb_fpc <- vec_cast_tfb_tfb

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_fpc.tfb_spline <- vec_cast_tfb_tfb

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_fpc.tfb_fpc <- vec_cast_tfb_tfb

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_spline.tfd_reg <- vec_cast_tfb_tfd

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_spline.tfd_irreg <- vec_cast_tfb_tfd

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_fpc.tfd_reg <- vec_cast_tfb_tfd

#' @rdname vctrs
#' @family tidyfun vctrs
#' @export
vec_cast.tfb_fpc.tfd_irreg <- vec_cast_tfb_tfd
