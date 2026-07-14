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
#' For vector-valued (`tf_mv`) input, the `data.frame` on which `cond` is
#' evaluated has one column per component, named like the components, instead
#' of a single `value` column: conditions are *joint* conditions across
#' components, e.g. `tf_where(f, x > 0 & y < 1)` or
#' `tf_where(f, sqrt(x^2 + y^2) > 1)` for components `"x"` and `"y"`. To apply
#' a condition to a single component, extract it first:
#' `tf_where(f[, component = "x"], value > 0)`. All components have to be
#' observed on a common grid -- use [tf_interpolate()] to align them first, or
#' supply `arg` explicitly.
#'
#' @param f a `tf` object.
#' @param cond a logical expression about `value` (and/or `arg`) that defines a
#'   condition about the functions, see examples and details.
#' @param return for each entry in `f`, `tf_where` either returns *all* `arg`
#'   for which `cond` is true, the *first*, the *last* or their *range* or
#'   logical flags whether the functions fulfill the condition *any*where. For
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
#' @examplesIf rlang::is_installed("dplyr")
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
#'
#' # vector-valued input: conditions refer to components by name
#' fm <- tfd_mv(list(x = tf_rgp(3, 11L), y = tf_rgp(3, 11L)))
#' tf_where(fm, x > 0 & y < 0)
#' tf_where(fm, sqrt(x^2 + y^2) > 1, "first")
#' tf_anywhere(fm, x > y)
#' @family tidyfun query-functions
#' @export
tf_where <- function(
  f,
  cond,
  return = c("all", "first", "last", "range", "any"),
  arg = tf_arg(f)
) {
  UseMethod("tf_where")
}

#' @export
tf_where.default <- function(
  f,
  cond,
  return = c("all", "first", "last", "range", "any"),
  arg = tf_arg(f)
) {
  assert_arg(arg, f)
  return <- match.arg(return)
  tf_where_impl(f, enquo(cond), return, arg)
}

#' @export
tf_where.tf_mv <- function(
  f,
  cond,
  return = c("all", "first", "last", "range", "any"),
  arg = tf_arg(f)
) {
  return <- match.arg(return)
  cond_quo <- enquo(cond)
  comp_names <- attr(f, "comp_names")
  if (
    "value" %in% all.vars(quo_get_expr(cond_quo)) && !("value" %in% comp_names)
  ) {
    cli::cli_abort(
      c(
        "{.var value} is undefined for vector-valued {.cls tf_mv} input.",
        i = "Refer to the components by name instead: {.val {comp_names}}.",
        i = paste(
          "If you meant a variable {.var value} from the calling environment,",
          "inject it with {.code !!value}."
        )
      ),
      class = "tf_mv_where_value",
      call = NULL
    )
  }
  if (missing(arg) && !mv_args_shared(f)) {
    cli::cli_abort(
      c(
        "{.fn tf_where} needs all components of {.arg f} on a common grid.",
        i = paste(
          "Use {.fn tf_interpolate} to align the components first,",
          "or supply a single numeric {.arg arg} vector explicitly."
        )
      ),
      class = "tf_mv_incommensurate_args",
      call = NULL
    )
  }
  if (!missing(arg) && is.list(arg) && identical(names(arg), comp_names)) {
    # tf_arg(f) returns such a per-component list exactly when the component
    # grids differ; the evaluation machinery would misread it as per-curve
    # grids and silently evaluate each curve on a different component's grid.
    cli::cli_abort(
      c(
        paste(
          "{.arg arg} is a per-component list, as returned by",
          "{.code tf_arg(f)} for components on different grids."
        ),
        i = paste(
          "Supply a single numeric vector of evaluation points instead,",
          "or a per-curve list of such vectors."
        )
      ),
      class = "tf_mv_incommensurate_args",
      call = NULL
    )
  }
  assert_arg(arg, f)
  tf_where_impl(f, cond_quo, return, arg)
}

tf_where_impl <- function(f, cond_quo, return, arg) {
  # no subset(): subset.data.frame() evaluates its condition *inside* the
  # data.frame, so for tf_mv input a component named like any symbol used
  # here (e.g. "x") would capture it. Logical indexing with NAs set to FALSE
  # reproduces subset()'s semantics, including recycling of short conditions
  # (e.g. a scalar `mean(value) > 0` applies to all args).
  where_at <- map(
    f[, arg, matrix = FALSE],
    \(d) {
      keep <- eval_tidy(cond_quo, d)
      if (!is.logical(keep)) {
        cli::cli_abort("{.arg cond} must evaluate to a logical vector.")
      }
      d[["arg"]][keep & !is.na(keep)]
    }
  )
  where_at[is.na(f)] <- NA

  if (return == "all") {
    return(where_at)
  }

  where_at[lengths(where_at) == 0] <- NA
  if (return == "range") {
    if (!length(where_at)) {
      return(data.frame(begin = numeric(0), end = numeric(0)))
    }
    where_at <- map(where_at, range)
    where_at <- do.call(rbind, where_at) |>
      as.data.frame() |>
      setNames(c("begin", "end"))
    return(where_at)
  }
  where_at <- switch(
    return,
    any = map_lgl(where_at, \(x) !allMissing(x)),
    first = map_dbl(where_at, min),
    last = map_dbl(where_at, max)
  )
  where_at
}

#' @rdname tf_where
#' @export
tf_anywhere <- function(f, cond, arg = tf_arg(f)) {
  UseMethod("tf_anywhere")
}

#' @export
tf_anywhere.default <- function(f, cond, arg = tf_arg(f)) {
  tf_where(f = f, cond = {{ cond }}, return = "any", arg = arg)
}

#' @export
tf_anywhere.tf_mv <- function(f, cond, arg = tf_arg(f)) {
  # forward arg only if supplied, so tf_where.tf_mv can tell apart a
  # deliberate grid choice from the default (which requires shared grids)
  if (missing(arg)) {
    tf_where(f = f, cond = {{ cond }}, return = "any")
  } else {
    tf_where(f = f, cond = {{ cond }}, return = "any", arg = arg)
  }
}
