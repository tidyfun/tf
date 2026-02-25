# utility function for linear operations that can be done on coefs or
#   evaluations directly.
fun_math <- function(x, op) {
  attr_ret <- attributes(x)
  ret <- map(tf_evaluations(x), \(x) {
    if (is.null(x)) return(NULL)
    result <- do.call(op, list(x = x))
    if (allMissing(result)) NULL else result
  })
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, \(x, y) {
      if (is.null(y)) return(NULL)
      list(arg = x, value = y)
    })
  }
  attributes(ret) <- attr_ret
  ret
}
#------------------------------------------------------------------------------

#' @include ops.R
NULL

#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute functions
Math.tfd <- function(x, ...) {
  fun_math(x, .Generic)
}

#' @rdname tfgroupgenerics
#' @export
Math.tfb <- function(x, ...) {
  genericname <- .Generic
  cli::cli_warn(
    "Potentially lossy cast to {.cls tfd} and back in {.fn {genericname}}({.cls {vec_ptype_full(x)}})."
  )
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(tfd(x), .Generic)
  na_entries <- is.na(eval)
  if (all(na_entries)) {
    # all entries became NA -- return tfb with NULL entries
    result <- vector("list", length(eval))
    result[] <- list(NULL)
    result_names <- names(eval)
    attributes(result) <- attributes(x)
    names(result) <- result_names
    return(result)
  }
  if (any(na_entries)) {
    # refit only non-NA entries, then insert NULLs at NA positions
    non_na <- do.call(
      "tfb",
      c(list(eval[!na_entries]), basis_args, penalized = FALSE, verbose = FALSE)
    )
    result <- vector("list", length(eval))
    result[!na_entries] <- unclass(non_na)
    result[na_entries] <- list(NULL)
    result_names <- names(eval)
    attributes(result) <- attributes(non_na)
    names(result) <- result_names
    return(result)
  }
  do.call(
    "tfb",
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
  )
}

#-------------------------------------------------------------------------------
# TODO:
# inner product?
# `%*%.default` = .Primitive("%*%") # assign default as current definition
# `%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
# }
# `%*%.tf(x, y) = [int x_i(t)*y_i(t) dt]
