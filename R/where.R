#' Find out where functional data fulfills certain conditions.
#'
#' `tf_where` allows to define a logical expression about the function values
#' and returns the argument values for which that condition is true.\cr
#' `tf_anywhere` is syntactic sugar for `tf_where` with `return = "any"` to
#' get a logical flag for each function if the condition is `TRUE` *anywhere*,
#' see below.
#'
#' Entries in `f` that do not fulfill `cond` anywhere yield `numeric(0)`.\cr
#' `cond`  is evaluated as a [base::subset()]-statement on a `data.frame`
#' containing a single entry in `f` with columns `arg` and `value`, so most
#' of the usual `dplyr` tricks are available as well, see examples.\cr
#' Any `cond`ition evaluates to `NA` on `NA`-entries in `f`.
#'
#' @param f a `tf` object
#' @param cond a logical expression about `value` (and/or `arg`) that defines a
#'   condition about the functions, see examples and details.
#' @param return for each entry in `f`, `tf_where` either returns *all* `arg`
#'   for which `cond` is true, the *first*, the *last* or their *range* or
#'   logical flags whether the functions fullfill the condition *any*where. For
#'   `"range"`, note that `cond` may not be true for all `arg` values in this
#'   range, though, this is not checked.
#' @param arg optional `arg`-values on which to evaluate `f` and check `cond`,
#'   defaults to `tf_arg(f)`.
#' @returns depends on  `return`:
#'  - `return = "any"`, i.e, `anywhere`:
#'   a logical vector of the same length as `f`.
#'  - `return = "all"`: a list of vectors of the same length as `f`, with
#'   empty vectors for the functions that  never fulfill the `cond`ition.
#'  - `return = "range"`: a data frame with columns "begin" and "end".
#'  - else, a numeric vector of the same length as `f` with `NA` for entries of
#'   `f` that nowhere fulfill the `cond`ition.
#' @examples
#' lin <- 1:4 * tfd(seq(-1, 1, length.out = 11), seq(-1, 1, length.out = 11))
#' tf_where(lin, value %inr% c(-1, 0.5))
#' tf_where(lin, value %inr% c(-1, 0.5), "range")
#' a <- 1
#' tf_where(lin, value > a, "first")
#' tf_where(lin, value < a, "last")
#' tf_where(lin, value > 2, "any")
#' tf_anywhere(lin, value > 2)
#'
#' set.seed(4353)
#' f <- tf_rgp(5, 11)
#' plot(f, pch = as.character(1:5), points = TRUE)
#' tf_where(f, value == max(value))
#' # where is the function increasing/decreasing?
#' tf_where(f, value > dplyr::lag(value, 1, value[1]))
#' tf_where(f, value < dplyr::lead(value, 1, tail(value, 1)))
#' # where are the (interior) extreme points (sign changes of `diff(value)`)?
#' tf_where(
#'   f,
#'   sign(c(diff(value)[1], diff(value))) !=
#'     sign(c(diff(value), tail(diff(value), 1)))
#' )
#' # where in its second half is the function positive?
#' tf_where(f, arg > 0.5 & value > 0)
#' # does the function ever exceed?
#' tf_anywhere(f, value > 1)
#' @importFrom stats setNames
#' @family tidyfun query-functions
#' @export
tf_where <- function(f, cond,
                     return = c("all", "first", "last", "range", "any"), arg) {
  if (missing(arg)) {
    arg <- tf_arg(f)
  }
  assert_arg(arg, f)
  return <- match.arg(return)
  cond_call <- substitute(cond)
  parent <- parent.frame()
  where_at <- map(
    f[, arg, matrix = FALSE],
    \(x) subset(x, eval(cond_call, envir = x, enclos = parent))[["arg"]]
  )
  where_at[is.na(f)] <- NA

  if (return == "all") {
    return(where_at)
  }

  where_at <- map_if(where_at, \(x) length(x) == 0, \(x) NA)
  if (return == "range") {
    where_at <- map(where_at, range)
    where_at <- do.call(what = rbind, args = where_at) |>
      as.data.frame() |>
      setNames(c("begin", "end"))
    return(where_at)
  }
  where_at <- switch(return,
    "any"   = map_lgl(where_at, \(x) !all(is.na(x))),
    "first" = map_dbl(where_at, min),
    "last"  = map_dbl(where_at, max)
  )
  where_at
}

#' @rdname tf_where
#' @export
tf_anywhere <- function(f, cond, arg) {
  call <- match.call()
  call[[1]] <- tf_where
  call$return <- "any"
  eval(call, parent.frame())
}
