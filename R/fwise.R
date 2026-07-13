#' Summarize each `tf` in a vector (function-wise)
#'
#' These functions extract (user-specified) **function-wise** summary statistics
#' from every entry in a  `tf`-vector. To summarize a vector of functions at each
#' argument value, see `?tfsummaries`. Note that most of these will tend to yield lots
#' of `NA`s for irregular `tfd` unless you set a [tf_evaluator()]-function
#' that does inter- and extrapolation for them beforehand.
#'
#' @param x  a `tf` object.
#' @param y  a `tf` object.
#' @param arg defaults to standard argument values of `x`.
#' @param ... additional arguments for [purrr::as_mapper()].
#' @name functionwise
#' @family tidyfun summary functions
#' @return a list (or vector) of the same length as `x` with the respective
#'   summaries.
NULL

#' @export
#' @describeIn functionwise User-specified function-wise summary statistics
#' @param .f a function or formula that is applied to each entry of `x`, see
#'   [purrr::as_mapper()] and details.
#' @details `tf_fwise` turns `x` into a list of data.frames with columns `arg`
#' and `values` internally, so the function/formula in `.f` gets a data.frame
#' `.x` with these columns, see examples below or source code for [tf_fmin()],
#' [tf_fmax()], etc.
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
#' tf_fwise(x, \(.x) quantile(.x$value, 0.8)) |> unlist()
#' # minimal value of each function for t > 0.5
#' tf_fwise(x, \(.x) min(.x$value[.x$arg > 0.5])) |> unlist()
#'
#' tf_crosscor(x, -x)
#' tf_crosscov(x, x) == tf_fvar(x)
tf_fwise <- function(x, .f, arg = tf_arg(x), ...) {
  UseMethod("tf_fwise")
}

#' @export
tf_fwise.default <- function(x, .f, arg = tf_arg(x), ...) {
  assert_tf(x)
  assert_arg(arg = arg, x = x)
  x_ <- x[, arg, matrix = FALSE]
  f_map <- as_mapper(.f, ...)
  ret <- map(x_, f_map)
  setNames(ret, names(x))
}

#' @export
tf_fwise.tf_mv <- function(x, .f, arg = tf_arg(x), ...) {
  comp_names <- attr(x, "comp_names")
  comp_results <- imap(tf_components(x), function(comp, nm) {
    tf_fwise(comp, .f, arg = tf_mv_component_arg(arg, nm, comp_names), ...)
  })
  ret <- map(seq_along(x), function(i) {
    vals <- map(comp_results, \(res) res[[i]])
    names(vals) <- comp_names
    vals
  })
  setNames(ret, names(x))
}

# Factory for the function-wise scalar reductions tf_fmax / tf_fmin /
# tf_fmedian: reduce each function's values with `reduce_op`, unlist the
# per-function scalars and reattach names. For tf_mv inputs, return an
# n x d matrix (curves x components) like tf_fmean/tf_fvar/tf_fsd -- naive
# unlisting would interleave components into a misnamed length-n*d vector.
make_tf_freduce <- function(reduce_op) {
  freduce <- function(x, arg = tf_arg(x), na.rm = FALSE) {
    if (is_tf_mv(x)) {
      return(tf_mv_fsummary_matrix(
        x,
        \(comp, arg) freduce(comp, arg = arg, na.rm = na.rm),
        arg = arg
      ))
    }
    x |>
      tf_fwise(\(.x) reduce_op(.x$value, na.rm = na.rm), arg = arg) |>
      unlist(use.names = FALSE) |>
      setNames(names(x))
  }
  freduce
}

#' @export
#' @describeIn functionwise maximal value of each function
#' @inheritParams base::min
tf_fmax <- make_tf_freduce(max)

#' @export
#' @describeIn functionwise minimal value of each function
#' @inheritParams base::min
tf_fmin <- make_tf_freduce(min)

#' @export
#' @describeIn functionwise median value of each function
#' @inheritParams base::min
tf_fmedian <- make_tf_freduce(stats::median)

#' @export
#' @describeIn functionwise range of values of each function
#' @inheritParams base::range
tf_frange <- function(x, arg = tf_arg(x), na.rm = FALSE, finite = FALSE) {
  tf_fwise(x, \(.x) range(.x$value, na.rm = na.rm, finite = finite), arg = arg)
}

#' @export
#' @describeIn functionwise mean of each function:
#'   \eqn{\tfrac{1}{|T|}\int_T x_i(t) dt}
tf_fmean <- function(x, arg = tf_arg(x)) {
  UseMethod("tf_fmean")
}

#' @export
tf_fmean.default <- function(x, arg = tf_arg(x)) {
  assert_tf(x)
  assert_arg(arg = arg, x = x)
  x_ <- tf_interpolate(x, arg = arg)
  arg <- ensure_list(arg)
  len <- map_dbl(arg, \(x) max(x) - min(x))
  tf_integrate(x_) / len
}

#' @export
#' @describeIn functionwise component-wise means of each vector-valued function
tf_fmean.tf_mv <- function(x, arg = tf_arg(x)) {
  tf_mv_fsummary_matrix(x, tf_fmean, arg = arg)
}

#' @export
#' @describeIn functionwise variance of each function:
#'   \eqn{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t))^2 dt}
tf_fvar <- function(x, arg = tf_arg(x)) {
  UseMethod("tf_fvar")
}

#' @export
tf_fvar.default <- function(x, arg = tf_arg(x)) {
  assert_tf(x)
  assert_arg(arg = arg, x = x)
  arg <- ensure_list(arg)
  len <- map_dbl(arg, \(x) max(x) - min(x))
  x_ <- tfd(x, arg = arg) # cast to tfd to avoid repeatedly casting back to tfb
  x_mean <- tf_interpolate(x, arg = arg) |> tf_fmean()
  x_c <- x_ - x_mean
  tf_integrate(x_c^2) / len
}

#' @export
#' @describeIn functionwise component-wise variances of each vector-valued function
tf_fvar.tf_mv <- function(x, arg = tf_arg(x)) {
  tf_mv_fsummary_matrix(x, tf_fvar, arg = arg)
}

#' @export
#' @describeIn functionwise standard deviation of each function:
#'   \eqn{\sqrt{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t))^2 dt}}
tf_fsd <- function(x, arg = tf_arg(x)) {
  UseMethod("tf_fsd")
}

#' @export
tf_fsd.default <- function(x, arg = tf_arg(x)) {
  tf_fvar(x, arg) |> sqrt()
}

#' @export
#' @describeIn functionwise component-wise standard deviations of each vector-valued function
tf_fsd.tf_mv <- function(x, arg = tf_arg(x)) {
  tf_mv_fsummary_matrix(x, tf_fsd, arg = arg)
}

tf_mv_fsummary_matrix <- function(x, .f, arg = tf_arg(x)) {
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")
  if (!length(comps)) {
    return(matrix(
      numeric(0),
      nrow = vec_size(x),
      ncol = 0L,
      dimnames = list(names(x), comp_names)
    ))
  }
  vals <- imap(comps, function(comp, nm) {
    .f(comp, arg = tf_mv_component_arg(arg, nm, comp_names))
  })
  ret <- do.call(cbind, vals)
  dimnames(ret) <- list(names(x), comp_names)
  ret
}

tf_mv_component_arg <- function(arg, nm, comp_names) {
  if (
    is.list(arg) &&
      length(arg) == length(comp_names) &&
      !is.null(names(arg)) &&
      !anyDuplicated(names(arg)) &&
      setequal(names(arg), comp_names)
  ) {
    arg[[nm]]
  } else {
    arg
  }
}

#' @export
#' @describeIn functionwise cross-covariances between two functional vectors:
#'   \eqn{\tfrac{1}{|T|}\int_T (x_i(t) - \bar x(t)) (y_i(t)-\bar y(t)) dt}
tf_crosscov <- function(x, y, arg = tf_arg(x)) {
  UseMethod("tf_crosscov")
}

#' @export
tf_crosscov.default <- function(x, y, arg = tf_arg(x)) {
  # check same domain, arg
  assert_tf(x)
  assert_tf(y)
  if (length(x) != length(y) && length(x) != 1 && length(y) != 1) {
    cli::cli_abort(
      "{.arg x} or {.arg y} must have length 1 or the same lengths."
    )
  }
  assert_arg(arg = arg, x = x)
  assert_true(identical(tf_domain(x), tf_domain(y)))
  arg <- ensure_list(arg)
  len <- map_dbl(arg, \(x) max(x) - min(x))
  # set up common args &
  # cast to tfd to avoid repeatedly casting back to tfb
  x_ <- tfd(x, arg = arg)
  y_ <- tfd(y, arg = arg)
  x_mean <- tf_interpolate(x, arg = arg) |> tf_fmean()
  y_mean <- tf_interpolate(y, arg = arg) |> tf_fmean()
  x_c <- x_ - x_mean
  y_c <- y_ - y_mean
  tf_integrate(x_c * y_c) / len
}

#' @export
#' @describeIn functionwise cross-correlation between two functional vectors:
#'   `tf_crosscov(x, y) / (tf_fsd(x) * tf_fsd(y))`
tf_crosscor <- function(x, y, arg = tf_arg(x)) {
  UseMethod("tf_crosscor")
}

#' @export
tf_crosscor.default <- function(x, y, arg = tf_arg(x)) {
  tf_crosscov(x, y, arg) / sqrt(tf_fvar(x, arg) * tf_fvar(y, arg))
}
