#' @rdname tfd
#' @export
as.tfd <- function(data, ...) UseMethod("as.tfd")
#' @export
as.tfd.default <- function(data, ...) {
  tfd(data, ...)
}

# TODO: this ignores arg, domain for now, only needed internally in c.tfd
#' @rdname tfd
#' @export
as.tfd_irreg <- function(data, ...) UseMethod("as.tfd_irreg")
#' @import purrr
#' @export
as.tfd_irreg.tfd_reg <- function(data, ...) {
  arg <- ensure_list(tf_arg(data))
  ret <- map2(tf_evaluations(data), arg, ~list(arg = .y, value = .x))
  attributes(ret) <- attributes(data)
  attr(ret, "arg") <- numeric(0)
  class(ret)[1] <- "tfd_irreg"
  ret
}
#' @export
as.tfd_irreg.tfd_irreg <- function(data, ...) {
  data
}

#' @rdname tfd
#' @inheritParams base::as.data.frame
#' @param optional not used!
#' @param unnest if `TRUE`, the function will return a data.frame with the evaluated functions.
#' @param x a `tf` object
#' @return if `unnest` is `FALSE` (default), a one-column `data.frame` with a `tf`-column containing `x`.
#'    if `unnest` is `TRUE`, a 3-column data frame with columns `id` 
#'    for the (unique) names of `x` or a numeric identifier, `arg` and `value`,
#'   with each row containing one function evaluation at the original `arg`-values.
#' @export
as.data.frame.tf <- function(x, row.names = NULL, optional = FALSE, unnest = FALSE, ...) {
  if (unnest) return(tf_2_df(x))
  colname <- deparse(substitute(x))
  ret <- data.frame(tmp = 1:length(x), row.names = row.names)
  ret[[colname]] <- x
  ret[, colname, drop = FALSE]
}



#' @rdname tfd
#' @inheritParams [.tf
#' @export
as.matrix.tfd <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg)) {
    arg <- sort(unique(unlist(tf_arg(x))))
  }
  x[, arg, interpolate = interpolate, matrix = TRUE]
}

#-------------------------------------------------------------------------------

#' @rdname tfb
#' @export
as.tfb <- function(data, basis = c("spline", "fpc"), ...) tfb(data, basis, ...)

#' @rdname tfb
#' @param x a [tfb] object to be converted
#' @param arg a grid of argument values to evaluate on.
#' @export
as.matrix.tfb <- function(x, arg = tf_arg(x), ...) {
  assert_arg_vector(arg, x)
  x[, arg, matrix = TRUE]
}

#-------------------------------------------------------------------------------
#' @export
as.function.tf <- function(x, ...) {
  function(arg) unlist(tf_evaluate(object = x, arg = arg))
}
