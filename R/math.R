# utility function for linear operations that can be done on coefs or
#   evaluations directly.
fun_math <- function(x, op) {
  attr_ret <- attributes(x)
  ret <- map(tf_evaluations(x), \(x) do.call(op, list(x = x)))
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, \(x, y) list(arg = x, value = y))
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
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(tfd(x), .Generic)
  do.call(
    "tfb",
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
  )
}

<<<<<<< HEAD
=======
#' @rdname tfgroupgenerics
#' @export
cummax.tfd <- function(...) {
  summarize_tf(..., op = "cummax", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cummin.tfd <- function(...) {
  summarize_tf(..., op = "cummin", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cumsum.tfd <- function(...) {
  summarize_tf(..., op = "cumsum", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute
cumprod.tfd <- function(...) {
  summarize_tf(..., op = "cumprod", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cummax.tfb <- function(...) {
  summarize_tf(..., op = "cummax", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cummin.tfb <- function(...) {
  summarize_tf(..., op = "cummin", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cumsum.tfb <- function(...) {
  summarize_tf(..., op = "cumsum", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cumprod.tfb <- function(...) {
  summarize_tf(..., op = "cumprod", eval = FALSE)
}

#-------------------------------------------------------------------------------

#' @rdname tfgroupgenerics
#' @export
Summary.tf <- function(...) {
  not_defined <- switch(.Generic,
    `all` = , `any` = TRUE, FALSE
  )
  if (not_defined) {
    stop(sprintf("%s not defined for \"tf\" objects", .Generic), call. = FALSE)
  }
  summarize_tf(..., op = .Generic, eval = is_tfd(list(...)[[1]]))
}

>>>>>>> 46049eccabb040bc902d5522d219706fe4d4cb5a
#-------------------------------------------------------------------------------
# TODO:
# inner product?
# `%*%.default` = .Primitive("%*%") # assign default as current definition
# `%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
# }
# `%*%.tf(x, y) = [int x_i(t)*y_i(t) dt]
