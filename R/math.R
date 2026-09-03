# utility function for linear operations that can be done on coefs or
#   evaluations directly.
fun_math <- function(x, op, ...) {
  ret <- map(tf_evaluations(x), \(evals) op_or_null(op, evals, ...))
  if (is_irreg(x)) {
    ret <- irreg_pairs(tf_arg(x), ret)
  }
  attributes(ret) <- attributes(x)
  ret
}
#------------------------------------------------------------------------------

#' @include ops.R
NULL

#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute functions
Math.tfd <- function(x, ...) {
  fun_math(x, .Generic, ...)
}

#' @rdname tfgroupgenerics
#' @export
Math.tfb <- function(x, ...) {
  genericname <- .Generic
  cli::cli_warn(
    "Potentially lossy cast to {.cls tfd} and back in {.fn {genericname}}({.cls {vec_ptype_full(x)}})."
  )
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(tfd(x), .Generic, ...)
  na_entries <- is.na(eval)
  if (all(na_entries)) {
    return(restore_na_entries(
      eval[!na_entries],
      na_entries,
      names(eval),
      ref_tfb = x
    ))
  }
  non_na <- do.call(
    "tfb",
    c(list(eval[!na_entries]), basis_args, penalized = FALSE, verbose = FALSE)
  )
  restore_na_entries(non_na, na_entries, names(eval))
}

#-------------------------------------------------------------------------------
# TODO:
# inner product?
# `%*%.default` = .Primitive("%*%") # assign default as current definition
# `%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
# }
# `%*%.tf(x, y) = [int x_i(t)*y_i(t) dt]
