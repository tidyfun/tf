# utility function for linear operations that can be done on coefs or evaluations directly.
fun_math <- function(x, op) {
  attr_ret <- attributes(x)
  ret <- map(tf_evaluations(x), ~do.call(op, list(x = .x)))
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, ~list(arg = .x, value = .y))
  }
  attributes(ret) <- attr_ret
  ret
}
#-------------------------------------------------------------------------------
# used for Summary grup generics and stats-methods...
# op has to be a string!
summarize_tf <- function(..., op = NULL, eval = FALSE) {
  dots <- list(...)
  funs <- map_lgl(dots, is_tf)
  op_args <- dots[!funs]
  funs <- dots[funs]
  op_call <- function(x) do.call(op, c(list(x), op_args))
  funs <- do.call(c, funs)
  # FIXME: should this set interpolate = TRUE so irregular data returns
  #  more useful results?
  m <- suppressWarnings(as.matrix(funs))
  ret <- apply(m, 2, op_call)
  arg <- as.numeric(colnames(m))
  args <- c(list(ret),
    arg = list(arg),
    domain = list(tf_domain(funs)),
    resolution = attr(funs, "resolution")
  )
  if (eval) {
    ret <- do.call(tfd, c(args, evaluator = attr(funs, "evaluator_name")))
    if (is_irreg(funs) && !is_irreg(ret)) ret <- as.tfd_irreg(ret)
    if (!is_irreg(funs) && is_irreg(ret)) ret <- as.tfd(ret)
    return(ret)
  } else {
    return(do.call(tfb, c(args,
      penalized = FALSE, verbose = FALSE,
      attr(funs, "basis_args")
    )))
  }
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
    stop(sprintf("%s not defined for \"tf\" objects", .Generic))
  }
  summarize_tf(..., op = .Generic, eval = is_tfd(list(...)[[1]]))
}

#-------------------------------------------------------------------------------
# TODO:
# inner product?
# `%*%.default` = .Primitive("%*%") # assign default as current definition
# `%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
# }
# `%*%.tf(x, y) = [int x_i(t)*y_i(t) dt]
