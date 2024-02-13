#' Convert functional data back to tabular data formats
#'
#' Various converters to turn `tfb`- or `tfd`-vectors into data.frames or
#' matrices, or even an actual R function.
#'
#' @rdname converters
#' @inheritParams base::as.data.frame
#' @param optional not used
#' @param unnest if `TRUE`, the function will return a data.frame with the
#'   evaluated functions.
#' @param x a `tf` object
#' @returns **for `as.data.frame.tf`:** if `unnest` is `FALSE` (default), a
#'   one-column `data.frame` with a `tf`-column containing `x`. if `unnest` is
#'   `TRUE`, a 3-column data frame with columns `id` for the (unique) names of
#'   `x` or a numeric identifier, `arg` and `value`, with each row containing
#'   one function evaluation at the original `arg`-values.
#' @export
#' @family tidyfun converters
as.data.frame.tf <- function(x, row.names = NULL, optional = FALSE,
                             unnest = FALSE, ...) {
  if (unnest) return(tf_2_df(x))
  colname <- deparse(substitute(x))
  ret <- data.frame(tmp = seq_along(x), row.names = row.names)
  ret[[colname]] <- x
  ret[, colname, drop = FALSE]
}

#' @rdname converters
#' @param arg a vector of argument values / evaluation points for `x`. Defaults
#'   to `tf_arg(x)`.
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for values in `arg` for which no original data is available? Only relevant
#'   for the raw data class `tfd`, for which it defaults to `FALSE`.
#'   Basis-represented functional data `tfb` are always "interpolated".
#' @returns **for `as.matrix.tf`:** a matrix with one row per function and one
#'   column per `arg`.
#' @export
#' @family tidyfun converters
as.matrix.tf <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg)) {
    arg <- tf_arg(x) |> unlist() |>  unique() |> sort()
  }
  if (is_tfb(x)) interpolate <- TRUE
  assert_arg_vector(arg, x, check_unique = FALSE)
  x[, arg, interpolate = interpolate, matrix = TRUE]
}

#-------------------------------------------------------------------------------
#' @rdname converters
#' @returns **for `as.function.tf`:** an R function with argument `arg` that
#'   evaluates `x` on `arg` and returns the list of function values
#' @export
#' @family tidyfun converters
as.function.tf <- function(x, ...) {
  function(arg) tf_evaluate(object = x, arg = arg)
}
