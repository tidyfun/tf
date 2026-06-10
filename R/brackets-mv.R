# Evaluation and bracket-indexing ----------------------------------------------

#' @export
tf_evaluate.tf_mv <- function(object, arg, ...) {
  if (!vec_size(object)) return(list())
  comps <- tf_components(object)
  comp_names <- attr(object, "comp_names")
  n <- vec_size(object)
  has_arg <- !missing(arg) && !is.null(arg)
  # Build the per-curve evaluation grid: caller's `arg` (a numeric vector or
  # per-curve list) if supplied, otherwise the per-curve union of the
  # components' native grids. Then evaluate each component on that grid so
  # every per-curve data.frame has a single `arg` column with NA-fill where
  # a component has no value at that arg.
  if (has_arg) {
    grids <- if (is.list(arg)) arg else rep(list(arg), n)
  } else {
    grids <- tf_mv_curve_grids(object)
  }
  comp_evals <- map(comps, function(comp) {
    tf_evaluate(comp, arg = grids, ...)
  })
  assemble_mv_evals(comp_evals, grids, comp_names, n)
}

#' @rdname tfbrackets
#' @param component for `tf_mv` objects only: optionally restrict evaluation /
#'   extraction to a subset of the output dimensions, given by name or integer
#'   index. A single name/index drops to the univariate component (a `tfd` or
#'   `tfb`); a vector of length > 1 returns a sub-`tf_mv` containing just those
#'   components. `NULL` (default) keeps all `d` components.
#' @export
`[.tf_mv` <- function(
  x,
  i,
  j,
  component = NULL,
  interpolate = TRUE,
  matrix = TRUE
) {
  # multi-component subset: rewrap as a smaller tf_mv with the selected
  # components, then fall through to the rest of the bracket logic with
  # `component = NULL` (which now operates on the smaller tf_mv).
  if (!is.null(component) && length(component) > 1L) {
    comp_names <- attr(x, "comp_names")
    if (is.character(component)) {
      bad <- setdiff(component, comp_names)
      if (length(bad)) {
        cli::cli_abort("Unknown component{?s}: {.val {bad}}.")
      }
    } else {
      assert_integerish(
        component,
        any.missing = FALSE,
        lower = 1L,
        upper = length(comp_names)
      )
    }
    x <- new_tf_mv(tf_components(x)[component], domain = tf_domain(x))
    component <- NULL
  }
  if (!is.null(component)) {
    comp <- tf_component(x, component)
    if (missing(i)) i <- seq_along(comp)
    if (missing(j)) {
      if (missing(matrix)) {
        return(comp[i, interpolate = interpolate])
      }
      return(comp[i, interpolate = interpolate, matrix = matrix])
    }
    if (missing(matrix)) {
      return(comp[i, j, interpolate = interpolate])
    }
    return(comp[i, j, interpolate = interpolate, matrix = matrix])
  }
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")

  # If any component is basis-represented, the per-component `[.tf` would emit
  # the "interpolate ignored" inform once per component; emit it once here
  # and suppress the per-component calls below (#252).
  if (!interpolate && any(map_lgl(comps, is_tfb))) {
    cli::cli_inform(
      "{.arg interpolate} ignored for data in basis representation."
    )
    interpolate <- TRUE
  }

  # matrix-index i: (function, arg) pairs -> (nrow(i) x d) matrix
  if (!missing(i) && is.matrix(i)) {
    cols <- map(comps, \(comp) comp[i, interpolate = interpolate])
    ret <- do.call(cbind, cols)
    colnames(ret) <- comp_names
    return(ret)
  }

  # Validate `i` the same way univariate `[.tf` does (no NA, no missing names,
  # no out-of-bounds). TODO(#252): factor this and `j`-normalisation out of
  # `[.tf` into a shared helper to remove the bracket-code duplication.
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    i <- vec_as_location(
      i,
      n = vec_size(x),
      names = names(x),
      missing = "error"
    )
  }
  xi <- vec_slice(x, i)

  if (missing(j) && missing(matrix)) {
    return(xi)
  }
  if (missing(j) && !missing(matrix) && isFALSE(matrix)) {
    j <- tf_mv_curve_grids(xi)
  }

  comps_i <- tf_components(xi)
  n_i <- vec_size(xi)
  if (matrix) {
    if (missing(j)) j <- sort_unique(tf_mv_curve_grids(xi), simplify = TRUE)
    if (!length(comps_i) || n_i == 0L) {
      return(array(
        numeric(0),
        dim = c(n_i, length(j), length(comps_i)),
        dimnames = list(names(xi), as.character(j), comp_names)
      ))
    }
    mats <- map(
      comps_i,
      \(comp) comp[, j, interpolate = interpolate, matrix = TRUE]
    )
    arr <- array(
      unlist(mats, use.names = FALSE),
      dim = c(nrow(mats[[1]]), ncol(mats[[1]]), length(comps_i)),
      dimnames = list(rownames(mats[[1]]), colnames(mats[[1]]), comp_names)
    )
    return(arr)
  }
  # matrix = FALSE: list of per-curve data.frames with arg + one col per comp
  if (!length(comps_i) || n_i == 0L) {
    return(setNames(vector("list", n_i), names(xi)))
  }
  dfs <- map(
    comps_i,
    \(comp) comp[, j, interpolate = interpolate, matrix = FALSE]
  )
  map(seq_len(n_i), function(k) {
    base <- dfs[[1]][[k]]
    out <- data_frame0(arg = base$arg)
    for (cn in seq_along(comp_names)) {
      out[[comp_names[cn]]] <- dfs[[cn]][[k]]$value
    }
    out
  }) |>
    setNames(names(xi))
}

#' @rdname tfbrackets
#' @export
`[<-.tf_mv` <- function(x, i, value) {
  # Replace curves component-wise via the univariate `[<-.tf` (which handles
  # NA assignment, length recycling and lossy casts per component), then
  # rebuild. This is more robust than letting the default `[<-.tf` thread a
  # `vec_slice<-` through the data-frame-of-components proxy.
  if (missing(i)) i <- seq_along(x)
  comps <- tf_components(x)
  value_comps <- if (is_tf_mv(value)) {
    check_compatible_mv(x, value)
    tf_components(value)
  } else {
    # only NA (scalar logical/numeric, or all-NA atomic) is broadcast across
    # every component. Anything else -- including a univariate tf -- is a
    # type error: it would silently make every component identical.
    is_atomic_all_na <- is.atomic(value) && length(value) &&
      all(is.na(value))
    if (!is_atomic_all_na) {
      cli::cli_abort(
        "univariate tf cannot be combined with vector-valued tf_mv"
      )
    }
    rep(list(value), length(comps))
  }
  new_comps <- map2(comps, value_comps, function(comp, v) {
    comp[i] <- v
    comp
  })
  names(new_comps) <- attr(x, "comp_names")
  new_tf_mv(new_comps, domain = tf_domain(x))
}

#' @export
`names<-.tf_mv` <- function(x, value) {
  # curve names live on the underlying components (that is what `vec_restore()`
  # rebuilds from), so push them down to every component rather than only onto
  # the outer vctr -- otherwise they are lost on the next subset / concatenation.
  comps <- map(tf_components(x), function(comp) {
    names(comp) <- value
    comp
  })
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps, domain = tf_domain(x))
}
