#' Summarize each `tf` in a vector
#'
#' These functions extract (user-specified) **function-wise** summary statistics
#' from each entry in a  `tf`-vector. To summarize a vector of functions at each
#' argument value, see `?tfsummaries`. Note that these will tend to yield lots
#' of `NA`s for irregular `tfd` unless you set a [tf_evaluator()]-function
#' that does inter- and extrapolation for them beforehand.
#'
#' @param x  a `tf` object
#' @param y  a `tf` object
#' @param arg defaults to standard argument values of `x`
#' @param ... additional arguments for [purrr::as_mapper()]
#' @name functionwise
#' @family tidyfun summary functions
#' @return a list (or vector) of the same length as `x` with the respective
#'   summaries
#' @importFrom purrr as_mapper list_c
NULL

#' @export
#' @describeIn functionwise User-specified function-wise summary statistics
#' @param .f a function or formula that is applied to each entry of `x`, see
#'   [purrr::as_mapper()] and Details.
#' @details `tf_fwise` turns `x` into a list of data.frames with columns `arg`
#' and `values` internally, so the function/formula in `.f` gets a data.frame
#' `.x` with these columns, see examples below or source code for [tf_fmin()],
#' [tf_fmax()], etc
#' @examples
#' x <- tf_rgp(3)
#' layout(t(1:3))
#' plot(x, col = 1:3)
#' #  each function's values to [0,1]:
#' x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
#' plot(x_clamp, col = 1:3)
#' # standardize each function to have mean / integral 0 and sd 1:
#' x_std <- (x - tf_fmean(x)) / tf_fsd(x)
#' tf_fvar(x_std) == c(1, 1, 1)
#' plot(x_std, col = 1:3)
#' # Custom functions:
#' # 80%tiles of each function's values:
#' tf_fwise(x, ~ quantile(.x$value, .8)) |> unlist()
#' # minimal value of each function for t >.5
#' tf_fwise(x, ~ min(.x$value[.x$arg > .5])) |> unlist()
#'
#' tf_crosscor(x, -x)
#' tf_crosscov(x, x) == tf_fvar(x)
tf_fwise <- function(x, .f, arg = tf_arg(x), ...) {
  checkmate::assert_class(x, "tf")
  assert_arg(arg = arg, x = x)
  x_ <- x[, arg, matrix = FALSE]
  f_map <- purrr::as_mapper(.f, ...)
  ret <- map(x_, f_map)
  setNames(ret, names(x))
}
#' @export
#' @describeIn functionwise maximal value of each function
#' @inheritParams base::min
tf_fmax <- function(x, arg = tf_arg(x), na.rm = FALSE) {
  ret <- tf_fwise(x, ~ max(.x$value, na.rm = na.rm), arg = arg) |> list_c()
  setNames(ret, names(x))
}
#' @export
#' @describeIn functionwise minimal value of each function
#' @inheritParams base::min
tf_fmin <- function(x, arg = tf_arg(x), na.rm = FALSE) {
  ret  <- tf_fwise(x, ~ min(.x$value, na.rm = na.rm), arg = arg) |> list_c()
  setNames(ret, names(x))
}
#' @export
#' @describeIn functionwise median value of each function
#' @inheritParams base::min
tf_fmedian <- function(x, arg = tf_arg(x), na.rm = FALSE) {
  ret  <- tf_fwise(x, ~ median(.x$value, na.rm = na.rm), arg = arg) |> list_c()
  setNames(ret, names(x))
}
#' @export
#' @describeIn functionwise range of values of each function
#' @inheritParams base::range
tf_frange <- function(x, arg = tf_arg(x), na.rm = FALSE, finite = FALSE) {
  tf_fwise(x, ~ range(.x$value, na.rm = na.rm, finite = finite), arg = arg)
}

#' @export
#' @describeIn functionwise mean of each function:
#'   \eqn{\tfrac{1}{|T|}\int_T x_i(t) dt}
tf_fmean <- function(x, arg = tf_arg(x)) {
  checkmate::assert_class(x, "tf")
  assert_arg(arg = arg, x = x)

  x_ <- tf_interpolate(x, arg = arg)
  arg <- ensure_list(arg)
  length <- map_dbl(arg, max) - map_dbl(arg, min)
  tf_integrate(x_)/length
}
#' @export
#' @describeIn functionwise variance of each function:
#'   \eqn{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t))^2 dt}
tf_fvar <- function(x, arg = tf_arg(x)) {
  checkmate::assert_class(x, "tf")
  assert_arg(arg = arg, x = x)

  arg <- ensure_list(arg)
  length <- map_dbl(arg, max) - map_dbl(arg, min)
  x_ <- tf_interpolate(x, arg = arg)
  x_mean <- tf_integrate(x_)/length
  x_c <- x_ - x_mean
  tf_integrate(x_c^2)/length
}
#' @export
#' @describeIn functionwise standard deviation of each function:
#'   \eqn{\sqrt{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t))^2 dt}}
tf_fsd <- function(x, arg = tf_arg(x)) {
  tf_fvar(x, arg) |> sqrt()
}

#' @export
#' @describeIn functionwise cross-covariances between two functional vectors:
#'   \eqn{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t)) (y_i(t)-\bar y(t)) dt}
tf_crosscov <- function(x, y, arg = tf_arg(x)) {
  # check same domain, arg
  checkmate::assert_class(x, "tf")
  checkmate::assert_class(y, "tf")
  if (!any(c(length(x) == length(y), length(x) == 1, length(y) == 1))) {
    stop("x or y must have length 1 or the same lengths.", call. = FALSE)
  }
  assert_arg(arg = arg, x = x)
  checkmate::assert_true(
    identical(tf_domain(x), tf_domain(y))
  )
  arg <- ensure_list(arg)
  length <- map_dbl(arg, max) - map_dbl(arg, min)
  # set up common args
  x_ <- tf_interpolate(x, arg = arg)
  y_ <- tf_interpolate(y, arg = arg)
  x_mean <- tf_integrate(x_)/length
  y_mean <- tf_integrate(y_)/length
  x_c <- x_ - x_mean
  y_c <- y_ - y_mean
  tf_integrate(x_c * y_c)
}
#' @export
#' @describeIn functionwise cross-correlation between two functional vectors:
#'   `tf_crosscov(x, y) / (tf_fsd(x) * tf_fsd(y))`
tf_crosscor <- function(x, y, arg = tf_arg(x)) {
  tf_crosscov(x, y, arg) / sqrt(tf_fvar(x, arg) * tf_fvar(y, arg))
}


