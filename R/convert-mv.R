# Conversion / interop ---------------------------------------------------------

#' Coerce a `tf_mv` to a matrix or data.frame
#'
#' `as.matrix.tf_mv` returns a **3-d** array `[curve, arg, component]` -- the
#'   natural shape for a vector-valued evaluation. This is deliberately
#'   different from [`as.matrix.tf`] (2-d, `[curve, arg]`); see `@seealso`.
#'
#' `as.data.frame.tf_mv` returns either a single-column wrapping data.frame
#'   (`unnest = FALSE`, for storing a `tf_mv` in a tibble column) or an
#'   evaluated long/wide data.frame (`unnest = TRUE`).
#'
#' @param x a `tf_mv` object.
#' @param arg optional evaluation grid (numeric vector or per-curve list).
#'   When `NULL` (default for `as.data.frame.tf_mv`; equivalent to "missing"
#'   for `as.matrix.tf_mv`), the per-curve union of all components' native
#'   argument grids is used.
#' @param interpolate forwarded to the underlying `tf` evaluation. `tfb`
#'   components are always interpolated.
#' @param unnest if `TRUE`, return an evaluated data.frame (see `long`); if
#'   `FALSE` (default), a one-column data.frame wrapping `x`.
#' @param long when `unnest = TRUE`, controls the schema. `long = TRUE`
#'   (default) returns a 4-column data.frame
#'   `(id, arg, component, value)` -- the multivariate analogue of the
#'   univariate `(id, arg, value)` contract, with `component` a `factor` over
#'   `attr(x, "comp_names")`. `long = FALSE` returns the wide
#'   `(id, arg, comp1, ..., compd)` schema.
#' @param row.names,optional standard `as.data.frame` plumbing.
#' @param ... passed through.
#' @returns a 3-d array (`as.matrix.tf_mv`) or a data.frame (`as.data.frame.tf_mv`).
#' @seealso [as.matrix.tf()] (2-d sibling), [as.data.frame.tf()] (univariate
#'   contract), [tf_evaluate()].
#' @family tidyfun converters
#' @name converters-mv
#' @examples
#' arg <- seq(0, 1, length.out = 11)
#' xf <- tfd(t(sapply(1:3, function(i) sin(2 * pi * arg + i))), arg = arg)
#' yf <- tfd(t(sapply(1:3, function(i) cos(2 * pi * arg + i))), arg = arg)
#' mv <- tfd_mv(list(x = xf, y = yf))
#' dim(as.matrix(mv))
#' head(as.data.frame(mv, unnest = TRUE))
#' @export
as.matrix.tf_mv <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg) || is.null(arg)) {
    x[,, interpolate = interpolate, matrix = TRUE]
  } else {
    x[, arg, interpolate = interpolate, matrix = TRUE]
  }
}

#' @rdname converters-mv
#' @export
as.data.frame.tf_mv <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  unnest = FALSE,
  long = TRUE,
  arg = NULL,
  interpolate = TRUE,
  ...
) {
  if (!unnest) {
    out <- vctrs::new_data_frame(list(x), n = vec_size(x))
    names(out) <- "data"
    return(out)
  }

  comp_names <- attr(x, "comp_names") %||% character(0)
  n <- vec_size(x)
  ids <- unique_id(names(x)) %||% seq_len(n)
  id_levels <- as.character(ids)

  empty_long <- function() {
    data_frame0(
      id = factor(character(0), levels = id_levels),
      arg = numeric(0),
      component = factor(character(0), levels = comp_names),
      value = numeric(0)
    )
  }
  empty_wide <- function() {
    do.call(
      data_frame0,
      c(
        list(id = factor(character(0), levels = id_levels), arg = numeric(0)),
        setNames(rep(list(numeric(0)), length(comp_names)), comp_names)
      )
    )
  }

  if (!length(comp_names) || !n) {
    return(if (long) empty_long() else empty_wide())
  }

  # `tf_evaluate.tf_mv` returns a length-n list of per-curve data.frames with
  # columns (arg, comp1, ..., compd), NA-filled where a component has no value
  # at that arg. This is exactly the union-grid + NA-fill coercion downstream
  # consumers (e.g. tidyfun's ggplot layer) need; long/wide is just a pivot.
  per_curve <- tf_evaluate(x, arg = arg, interpolate = interpolate, ...)

  id_factor <- factor(as.character(ids), levels = id_levels)

  if (long) {
    parts <- map(seq_len(n), function(i) {
      cdf <- per_curve[[i]]
      if (!nrow(cdf)) return(empty_long())
      nr <- nrow(cdf)
      do.call(
        rbind,
        lapply(comp_names, function(nm) {
          data_frame0(
            id = rep(id_factor[i], nr),
            arg = cdf$arg,
            component = factor(rep(nm, nr), levels = comp_names),
            value = cdf[[nm]]
          )
        })
      )
    })
    out <- do.call(rbind, parts)
    if (!nrow(out)) return(empty_long())
    out <- out[order(out$id, out$arg, out$component), , drop = FALSE]
  } else {
    parts <- map(seq_len(n), function(i) {
      cdf <- per_curve[[i]]
      if (!nrow(cdf)) return(empty_wide())
      cbind(
        data_frame0(id = rep(id_factor[i], nrow(cdf))),
        cdf
      )
    })
    out <- do.call(rbind, parts)
    if (!nrow(out)) return(empty_wide())
    out <- out[order(out$id, out$arg), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
