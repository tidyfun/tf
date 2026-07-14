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
#' @param grids when `unnest = TRUE`, controls *where* components are
#'   evaluated when they live on different argument grids (for shared grids
#'   both settings agree). `"union"` (default) evaluates every component on
#'   each curve's union grid, so components get (interpolated) values at the
#'   *other* components' arg values inside their observed range. `"component"`
#'   evaluates each component strictly on its **own** grid (or on `arg`, if
#'   supplied): no values are fabricated at args a component was not observed
#'   at -- in the long schema such rows are simply absent, in the wide schema
#'   the other components' columns are `NA` there. Use `"union"` for paired
#'   evaluations (e.g. trajectory plots), `"component"` for faithful tabular
#'   exports of the observed data.
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
  grids = c("union", "component"),
  ...
) {
  if (!unnest) {
    out <- vctrs::new_data_frame(list(x), n = vec_size(x))
    names(out) <- "data"
    return(out)
  }
  grids <- match.arg(grids)

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

  if (grids == "component") {
    return(mv_df_component_grids(
      x,
      long = long,
      arg = arg,
      interpolate = interpolate,
      id_levels = id_levels,
      comp_names = comp_names,
      empty_long = empty_long,
      empty_wide = empty_wide,
      ...
    ))
  }

  # `tf_evaluate.tf_mv` returns a length-n list of per-curve data.frames with
  # columns (arg, comp1, ..., compd), NA-filled where a component has no value
  # at that arg. This is exactly the union-grid + NA-fill coercion downstream
  # consumers (e.g. tidyfun's ggplot layer) need; long/wide is just a pivot.
  per_curve <- tf_evaluate(x, arg = arg, interpolate = interpolate, ...)

  id_factor <- factor(as.character(ids), levels = id_levels)

  # build the pivoted columns in one pass instead of rbind-ing n (or n * d)
  # per-curve data.frames; the final sort makes the construction order
  # irrelevant (ties from duplicated `arg` values keep their per-curve
  # evaluation order under both schemes).
  nr <- map_int(per_curve, nrow)
  if (!sum(nr)) {
    return(if (long) empty_long() else empty_wide())
  }
  id_all <- rep(id_factor, nr)
  arg_all <- unlist(map(per_curve, `[[`, "arg"), use.names = FALSE)
  comp_col <- function(nm) {
    unlist(map(per_curve, `[[`, nm), use.names = FALSE)
  }

  if (long) {
    out <- data_frame0(
      id = rep(id_all, times = length(comp_names)),
      arg = rep(arg_all, times = length(comp_names)),
      component = factor(rep(comp_names, each = sum(nr)), levels = comp_names),
      value = unlist(map(comp_names, comp_col), use.names = FALSE)
    )
    out <- out[order(out$id, out$arg, out$component), , drop = FALSE]
  } else {
    out <- do.call(
      data_frame0,
      c(
        list(id = id_all, arg = arg_all),
        map(setNames(comp_names, comp_names), comp_col)
      )
    )
    out <- out[order(out$id, out$arg), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# grids = "component" backend of as.data.frame.tf_mv: each component is
# evaluated strictly on its own grid (or `arg`) via the univariate tf_2_df(),
# so no values are fabricated at other components' args. Long = the stacked
# per-component rows; wide = outer alignment on the (id, arg) key set with NA
# where a component was not evaluated.
mv_df_component_grids <- function(
  x,
  long,
  arg,
  interpolate,
  id_levels,
  comp_names,
  empty_long,
  empty_wide,
  ...
) {
  comps <- tf_components(x)
  per_comp <- map(comp_names, function(nm) {
    comp <- comps[[nm]]
    df <- if (is.null(arg)) {
      tf_2_df(comp, interpolate = interpolate, ...)
    } else {
      tf_2_df(comp, arg = arg, interpolate = interpolate, ...)
    }
    df$component <- nm
    df
  })
  stacked <- do.call(rbind, per_comp)
  if (!nrow(stacked)) {
    return(if (long) empty_long() else empty_wide())
  }
  id_f <- factor(as.character(stacked$id), levels = id_levels)
  component_f <- factor(stacked$component, levels = comp_names)
  if (long) {
    out <- data_frame0(
      id = id_f,
      arg = stacked$arg,
      component = component_f,
      value = stacked$value
    )
    out <- out[order(out$id, out$arg, out$component), , drop = FALSE]
  } else {
    keys <- data_frame0(id = id_f, arg = stacked$arg)
    ukeys <- vec_unique(keys)
    ukeys <- ukeys[order(ukeys$id, ukeys$arg), , drop = FALSE]
    loc <- vec_match(keys, ukeys)
    cols <- map(comp_names, function(nm) {
      v <- rep(NA_real_, nrow(ukeys))
      sel <- component_f == nm
      v[loc[sel]] <- stacked$value[sel]
      v
    })
    out <- do.call(
      data_frame0,
      c(
        list(id = ukeys$id, arg = ukeys$arg),
        setNames(cols, comp_names)
      )
    )
  }
  rownames(out) <- NULL
  out
}
