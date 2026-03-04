zoo_wrapper <- function(f, ...) {
  #nocov start
  dots <- list(...)
  function(x, arg, evaluations) {
    x_arg <- sort_unique(c(x, arg))
    x_arg_match <- match(x_arg, arg, nomatch = length(arg) + 1)
    requested <- x_arg %in% x
    dots[[length(dots) + 1]] <- zoo(evaluations[x_arg_match], x_arg)
    ret <- do.call(f, dots)
    coredata(ret)[requested]
  }
  #nocov end
}

#-------------------------------------------------------------------------------

#' @title Inter- and extrapolation functions for `tfd`-objects
#'
#' @description
#' These are exported evaluator callbacks for `tfd` objects. They control how
#' function values are inter-/extrapolated to previously unseen `arg` values and
#' are used by [tf_evaluate()].
#'
#' In typical use, set an evaluator when constructing a `tfd`
#' (`tfd(..., evaluator = tf_approx_linear)`) or replace it later via
#' `tf_evaluator(x) <- tf_approx_none`.
#'
#' These helpers are wrappers around [zoo::na.fill()], [zoo::na.approx()], etc.
#' and all share the same signature (`x`, `arg`, `evaluations`), so they can
#' also be called directly.
#'
#' The list:
#'
#' - `tf_approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`)  -- this is the default,
#' - `tf_approx_spline` for cubic spline interpolation, (i.e., [zoo::na.spline()]
#' with `na.rm = FALSE`),
#' - `tf_approx_none` in order to not inter-/extrapolate ever (i.e., [zoo::na.fill()] with `fill = NA`)
#' - `tf_approx_fill_extend` for linear interpolation and constant extrapolation
#' (i.e., [zoo::na.fill()] with `fill = "extend"`)
#' - `tf_approx_locf` for "last observation carried forward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE`)
#' - `tf_approx_nocb` for "next observation carried backward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE, fromLast = TRUE`).
#'
#' For implementing your own, see source code of `tf:::zoo_wrapper`.
#'
#' @rdname tf_approx
#' @seealso [tfd()]
#' @export
#' @param x new `arg` values to approximate/interpolate/extrapolate the function for.
#' @param arg the `arg` values of the `evaluations`.
#' @param evaluations the function values at `arg`.
#' @returns a vector of values of the function defined by the given
#'   \eqn{(x_i, f(x_i))}=`(arg, evaluations)`-tuples at new argument values `x`.
#' @family tidyfun inter/extrapolation functions
#' @examples
#' x <- tfd(matrix(c(0, 1), nrow = 1), arg = c(0, 1))
#' tf_evaluate(x, c(0, 0.5, 1))
#' tf_evaluator(x) <- tf_approx_none
#' tf_evaluate(x, c(0, 0.5, 1))
#'
#' tf_approx_linear(
#'   x = c(0, 0.5, 1),
#'   arg = c(0, 1),
#'   evaluations = c(0, 1)
#' )
tf_approx_linear <- zoo_wrapper(na.approx, na.rm = FALSE)

#' @rdname tf_approx
#' @export
#' @family tidyfun inter/extrapolation functions
tf_approx_spline <- zoo_wrapper(na.spline, na.rm = FALSE)

#' @rdname tf_approx
#' @export
#' @family tidyfun inter/extrapolation functions
tf_approx_none <- zoo_wrapper(na.fill, fill = NA)

#' @rdname tf_approx
#' @export
#' @family tidyfun inter/extrapolation functions
tf_approx_fill_extend <- zoo_wrapper(na.fill, fill = "extend")

#' @rdname tf_approx
#' @export
#' @family tidyfun inter/extrapolation functions
tf_approx_locf <- zoo_wrapper(na.locf, na.rm = FALSE)

#' @rdname tf_approx
#' @export
#' @family tidyfun inter/extrapolation functions
tf_approx_nocb <- zoo_wrapper(na.locf, na.rm = FALSE, fromLast = TRUE)
