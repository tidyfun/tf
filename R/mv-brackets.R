# Evaluation and bracket-indexing ----------------------------------------------

#' @export
tf_evaluate.tf_mv <- function(object, arg, ...) {
  has_arg <- !missing(arg)
  comp_evals <- map(tf_components(object), function(comp) {
    if (has_arg) tf_evaluate(comp, arg = arg, ...) else tf_evaluate(comp)
  })
  assemble_mv_evals(comp_evals, attr(object, "comp_names"), vec_size(object))
}

#' @rdname tfbrackets
#' @param component for `tf_mv` objects only: optionally restrict evaluation /
#'   extraction to a single output dimension (by name or index), returning the
#'   univariate result. If `NULL` (default) all `d` components are returned (as
#'   an `array` `[curve, arg, component]` when `matrix = TRUE`).
#' @export
`[.tf_mv` <- function(
  x,
  i,
  j,
  interpolate = TRUE,
  matrix = TRUE,
  component = NULL
) {
  if (!is.null(component)) {
    comp <- tf_component(x, component)
    if (missing(i)) i <- seq_along(comp)
    if (missing(j)) {
      return(comp[i, interpolate = interpolate, matrix = matrix])
    }
    return(comp[i, j, interpolate = interpolate, matrix = matrix])
  }
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")

  # matrix-index i: (function, arg) pairs -> (nrow(i) x d) matrix
  if (!missing(i) && is.matrix(i)) {
    cols <- map(comps, \(comp) comp[i, interpolate = interpolate])
    ret <- do.call(cbind, cols)
    colnames(ret) <- comp_names
    return(ret)
  }

  if (missing(i)) i <- seq_along(x)
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
    # a scalar (typically NA) is broadcast to every component
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
