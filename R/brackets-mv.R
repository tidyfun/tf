# Evaluation and bracket-indexing ----------------------------------------------

#' @export
tf_evaluate.tf_mv <- function(object, arg, ...) {
  if (!vec_size(object)) return(list())
  comps <- tf_components(object)
  comp_names <- attr(object, "comp_names")
  n <- vec_size(object)
  has_arg <- !missing(arg) && !is.null(arg)
  # Build the evaluation grid: caller's `arg` (a shared numeric vector or a
  # per-curve list, both handled by the univariate tf_evaluate and by
  # assemble_mv_evals) if supplied, otherwise the per-curve union of the
  # components' native grids. Then evaluate each component on that grid so
  # every per-curve data.frame has a single `arg` column with NA-fill where
  # a component has no value at that arg.
  grids <- if (has_arg) arg else tf_mv_curve_grids(object)
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
    x <- tf_mv_subset_components(x, component)
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

  # `interpolate = FALSE` is only meaningful for tfd components; tfb components
  # are always evaluated from their basis representation (and the constructor
  # guarantees all components are the same kind). Emit the "interpolate
  # ignored" inform only once, not per component (#252).
  if (!interpolate && length(comps) && is_tfb(comps[[1]])) {
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

  i <- tf_bracket_i(x, i)
  xi <- vec_slice(x, i)

  if (missing(j)) {
    if (missing(matrix)) {
      return(xi)
    }
    j <- tf_bracket_j(tf_mv_curve_grids(xi), matrix)
  }

  comps_i <- tf_components(xi)
  n_i <- vec_size(xi)
  if (matrix) {
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
    # Only logical NA is broadcast across every component. Typed NAs
    # (NA_real_, NA_integer_) and any non-NA value -- including a univariate
    # tf -- are rejected: they would silently make every component identical
    # or trigger a confusing downstream "Can't combine" error.
    is_logical_na <- is.logical(value) && length(value) && all(is.na(value))
    if (!is_logical_na) {
      cli::cli_abort(
        "expected logical {.code NA} or another {.cls tf_mv} on the right-hand side."
      )
    }
    # Validate length upfront so we emit a clean message rather than the
    # downstream vec_slice<- "Can't recycle" complaint. Use vctrs subassignment
    # semantics so logical / negative / named indices give the correct number
    # of replacement locations rather than `length(i)`.
    n_loc <- length(vec_as_location(
      i,
      n = vec_size(x),
      names = names(x),
      missing = "error"
    ))
    if (length(value) > 1L && length(value) != n_loc) {
      cli::cli_abort(
        "length of {.arg value} ({length(value)}) must be 1 or match \\
        the number of locations in {.arg i} ({n_loc})."
      )
    }
    rep(list(value), length(comps))
  }
  new_comps <- map2(comps, value_comps, function(comp, v) {
    comp[i] <- v
    comp
  })
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
  # Forward the joint MFPC spec, if any: setting curve names is a non-mutating
  # rename and must not silently demote a `tfb_mfpc`. (`vec_c()` runs `names<-`
  # post-restore, which would otherwise strip the spec set by `tf_mv_ptype2`.)
  new_tf_mv(comps, domain = tf_domain(x), mfpc = attr(x, "mfpc"))
}

tf_mv_subset_components <- function(x, component) {
  comp_names <- attr(x, "comp_names")
  loc <- tf_mv_component_locations(component, comp_names)
  components <- tf_components(x)[loc]

  if (!is_tfb_mfpc(x)) {
    return(new_tf_mv(components, domain = tf_domain(x)))
  }

  selected_names <- comp_names[loc]
  preserves_full_spec <- length(loc) == length(comp_names) &&
    !anyDuplicated(selected_names) &&
    setequal(selected_names, comp_names)

  if (preserves_full_spec) {
    return(new_tf_mv(
      components,
      domain = tf_domain(x),
      mfpc = mfpc_reorder_spec(attr(x, "mfpc"), selected_names)
    ))
  }

  warn_mfpc_demotion(
    "Selecting a strict subset or repeated set of components invalidates the joint MFPC eigenbasis."
  )
  x <- tfb_mfpc_demote(x)
  new_tf_mv(tf_components(x)[loc], domain = tf_domain(x))
}

tf_mv_component_locations <- function(component, comp_names) {
  if (is.character(component)) {
    bad <- setdiff(component, comp_names)
    if (length(bad)) {
      cli::cli_abort("Unknown component{?s}: {.val {bad}}.")
    }
    return(match(component, comp_names))
  }
  assert_integerish(
    component,
    any.missing = FALSE,
    lower = 1L,
    upper = length(comp_names)
  )
  as.integer(component)
}
