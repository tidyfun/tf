c_names <- function(funs) {
  fnames <- as.list(names(funs) %||% rep("", length(funs)))
  elnames <- map(funs, \(x) names(x) %||% rep("", length(x)))
  # always use argnames
  # argnames replace elementnames if elments have length 1
  # else paste with "."
  names <- map2(fnames, elnames, \(x, y) {
    if (nzchar(x, keepNA = TRUE)) {
      return(y)
    }
    if (all(nzchar(y, keepNA = TRUE)) || length(y) == 1) {
      return(rep(x, length(y)))
    }
    paste(x, y, sep = ".")
  }) |>
    unlist()
  if (all(nzchar(names, keepNA = TRUE))) NULL else names
}

assert_domain_x_in_to <- function(x, to) {
  dom_x <- tf_domain(x)
  dom_to <- tf_domain(to)
  # can (try to) cast losslessly if domain of 'to' contains domain of 'x'
  if ((dom_to[1] <= dom_x[1]) & (dom_to[2] >= dom_x[2])) {
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


#----------------- s3 generics for tfd casting -----------------#

#' \code{vctrs} methods for \code{tf} objects
#'
#' These functions are the extensions that allow \code{tf} vectors
#' to work with \code{vctrs}.
#'
#' **Notes on `vec_cast`:** automatic/implicit casting of `tf` objects is tricky
#' because it's hard to determine automatically whether such an operation would
#' lose precision (different bases with different expressivity? different
#' argument grids?), and it's not generally clear which instances of which
#' `tf`-subclasses should be considered the "richer" objects.
#' Rules:
#'
#' - If the casted object's `domain` would not contain the entire original `domain`,
#'   no casting is possible.
#' - As long as the casted object's `domain` contains the entire original `domain`:
#'    - every `tfd_reg`, `tfd_irreg` or `tfb` can always be cast into an equivalent
#'   `tfd_irreg` (which may change `evaluator` and `domain`).
#'   - every `tfd_reg` can always be cast to `tfd_reg` (which may change `evaluator` and `domain`)
#'   - every `tfb` can be cast *losslessly* to `tfd` (regular or irregular,
#'     note it's lossless on *original* `arg`-grid)
#' - Any cast of a `tfd` into `tfb` is potentially *lossy* (`vctrs::maybe_lossy_cast`)
#' - Only `tfb` with identical bases and domains can be cast to one another *losslessly*
#'
#' @rdname vctrs
#' @family tidyfun vctrs
#' @importFrom vctrs vec_ptype2 vec_cast stop_incompatible_cast maybe_lossy_cast
#' @method vec_cast tfd_reg
#' @export
#' @export vec_cast.tfd_reg
#' @inheritParams vctrs::vec_cast
vec_cast.tfd_reg <- function(x, to, ...) UseMethod("vec_cast.tfd_reg")

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast tfd_irreg
#' @export
#' @export vec_cast.tfd_irreg
vec_cast.tfd_irreg <- function(x, to, ...) UseMethod("vec_cast.tfd_irreg")


#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_reg tfd_reg
#' @export
vec_cast.tfd_reg.tfd_reg <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x))
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_reg tfd_irreg
#' @export
vec_cast.tfd_reg.tfd_irreg <- function(x, to, ...) {
  akshually_regular_x <- all(duplicated(tf_arg(x))[-1])
  if (!akshually_regular_x) {
    stop_incompatible_cast(x = x, to = to, x_arg = "", to_arg = "")
  }
  tf_rebase(x, to, arg = tf_arg(x)[[1]])
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_reg tfb_spline
#' @export
vec_cast.tfd_reg.tfb_spline <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x))
}
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_reg tfb_fpc
#' @export
vec_cast.tfd_reg.tfb_fpc <- vec_cast.tfd_reg.tfb_spline

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_irreg tfd_reg
#' @export
vec_cast.tfd_irreg.tfd_reg <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x)) |> as.tfd_irreg()
}
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_irreg tfd_irreg
#' @export
vec_cast.tfd_irreg.tfd_irreg <- vec_cast.tfd_irreg.tfd_reg
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_irreg tfb_spline
#' @export
vec_cast.tfd_irreg.tfb_spline <- vec_cast.tfd_irreg.tfd_reg
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfd_irreg tfb_fpc
#' @export
vec_cast.tfd_irreg.tfb_fpc <- vec_cast.tfd_irreg.tfd_reg

#-------------------------------------------------------------------------------

#' @rdname vctrs
#' @family tidyfun vctrs
#' @import vctrs
#' @method vec_cast tfb_spline
#' @export
#' @export vec_cast.tfb_spline
#' @inheritParams vctrs::vec_cast
vec_cast.tfb_spline <- function(x, to, ...) UseMethod("vec_cast.tfb_spline")

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast tfb_fpc
#' @export
#' @export vec_cast.tfd_irreg
vec_cast.tfb_fpc <- function(x, to, ...) UseMethod("vec_cast.tfb_fpc")

vec_cast_tfb_tfb <- function(x, to, ...) {
  assert_same_domains(x, to)
  same_basis <- isTRUE(all.equal(tf_basis(to)(tf_arg(x)),
                                 attr(x, "basis_matrix")))
  if (same_basis) return(x)
  maybe_lossy_cast(tf_rebase(x, to, arg = tf_arg(x)),
                   x, to, lossy = TRUE, locations = 1:vec_size(x),
                   x_arg = "", to_arg = "",
                   loss_type = "precision",
                   message = "result represented in new basis")
}

vec_cast_tfb_tfd <- function(x, to, ...) {
  tf_rebase(x, to, arg = tf_arg(x))
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_spline tfb_spline
#' @export
vec_cast.tfb_spline.tfb_spline <- vec_cast_tfb_tfb
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_spline tfb_fpc
#' @export
vec_cast.tfb_spline.tfb_fpc <- vec_cast_tfb_tfb
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_fpc tfb_spline
#' @export
vec_cast.tfb_fpc.tfb_spline <- vec_cast_tfb_tfb
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_fpc tfb_fpc
#' @export
vec_cast.tfb_fpc.tfb_fpc <- vec_cast_tfb_tfb


#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_spline tfd_reg
#' @export
vec_cast.tfb_spline.tfd_reg <- vec_cast_tfb_tfd
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_spline tfd_irreg
#' @export
vec_cast.tfb_spline.tfd_irreg <- vec_cast_tfb_tfd
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_fpc tfd_reg
#' @export
vec_cast.tfb_fpc.tfd_reg <- vec_cast_tfb_tfd
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_cast.tfb_fpc tfd_irreg
#' @export
vec_cast.tfb_fpc.tfd_irreg <- vec_cast_tfb_tfd

