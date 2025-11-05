# utility function for linear operations that can be done on coefs or
#   evaluations directly.
fun_math <- function(x, op) {
  attr_ret <- attributes(x)
  ret <- map(tf_evaluations(x), \(x) {
    if (is.null(x)) return(NULL)  # Preserve NULL for NA entries
    result <- do.call(op, list(x = x))
    # Convert all-NA/NaN results to NULL (NA.tf should be NULL entry)
    if (all(is.na(result))) NULL else result
  })
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, \(x, y) {
      if (is.null(y)) NULL else list(arg = x, value = y)
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
    "Potentially lossy cast to {.cls tfd} and back in {genericname}({.cls {vec_ptype_full(x)}})"
  )
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(tfd(x), .Generic)
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
