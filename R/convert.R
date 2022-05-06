#' Convert functional data back to tabular data formats
#'
#' Various converters to turn `tfb`- or `tfd`-vectors into data.frames or
#' matrices (or actual functions, even!).
#'
#' @rdname converters
#' @inheritParams base::as.data.frame
#' @param optional not used!
#' @param unnest if `TRUE`, the function will return a data.frame with the evaluated functions.
#' @param x a `tf` object
#' @return **for `as.data.frame.tf`:** if `unnest` is `FALSE` (default), a one-column `data.frame` with a `tf`-column containing `x`.
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

#' @rdname converters
#' @inheritParams [.tf
#' @export
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
#' @export
as.function.tf <- function(x, ...) {
  function(arg) unlist(tf_evaluate(object = x, arg = arg))
}
