#' @include tfd-mv.R tfb-mv.R vctrs-mv.R
NULL

# Accessors --------------------------------------------------------------------

#' Accessors and methods for vector-valued functional data
#'
#' Utilities for `tf_mv` objects (see [tfd_mv()] / [tfb_mv()]). `tf_ncomp()`
#' returns the number of output dimensions \eqn{d}, `tf_components()` the list
#' of the `d` underlying univariate `tf` vectors, and `tf_component()` extracts
#' or replaces a single one (also available via the `$` operator, e.g. `f$x`).
#'
#' @details
#' Most univariate `tf` verbs also work on `tf_mv` objects by acting on each
#' component: [tf_rebase()] (and hence `tfd_mv`/`tfb_mv` conversion),
#' [tf_derive()], [tf_integrate()] (definite integrals return an `n x d`
#' matrix), [tf_smooth()] and [tf_zoom()]. Registration
#' ([tf_register()] / [tf_estimate_warps()] / [tf_warp()] / [tf_align()])
#' estimates a *single, shared* time-warp per curve and applies it jointly to
#' every component. The registration signal is, by default, the first
#' component; use `ref_component` to pick another component (by name/index),
#' `"norm"` for the pointwise Euclidean norm, or a function mapping the
#' `tf_mv` to a univariate `tf` vector.
#'
#' `is.na()` flags a curve as missing if **any** of its components is missing
#' (the union, not the intersection), which also drives the `na.rm` behaviour
#' of [mean()] / [median()] etc.
#'
#' @param f a `tf_mv` object.
#' @param which a component name or index.
#' @param value a univariate `tf` vector (replacement) of matching length and
#'   domain.
#' @returns `tf_ncomp()`: an integer. `tf_components()`: a named list of `tf`
#'   vectors. `tf_component()`: a single univariate `tf` vector.
#' @examples
#' f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
#' tf_ncomp(f)
#' tf_components(f)
#' tf_component(f, "y")
#' f$y
#' @family tf_mv-class
#' @name tf_mv_methods
#' @rdname tf_mv_methods
#' @export
tf_ncomp <- function(f) length(attr(f, "components"))

#' @rdname tf_mv_methods
#' @export
tf_components <- function(f) attr(f, "components")

# All component-mappers demote tfb_mfpc inputs first (loudly, see
# mfpc_demote_for_op): mapping over the components of a joint MFPC fit
# invalidates its shared scores, and reconstructing without demotion would
# leave components with an unusable per-component scoring function.
map_components <- function(x, .f, ..., .op = "component-wise operation") {
  x <- mfpc_demote_for_op(x, .op)
  new_tf_mv(map(tf_components(x), .f, ...))
}

# like map_components, but .f also receives the component name (for routing
# per-component arguments via tf_mv_component_arg())
imap_components <- function(x, .f, ..., .op = "component-wise operation") {
  x <- mfpc_demote_for_op(x, .op)
  new_tf_mv(imap(tf_components(x), .f, ...))
}

map2_components <- function(x, y, .f, ..., .op = "component-wise operation") {
  check_compatible_mv(x, y)
  x_warned <- is_tfb_mfpc(x)
  x <- mfpc_demote_for_op(x, .op)
  y <- mfpc_demote_for_op(y, .op, warn = !x_warned)
  new_tf_mv(map2(tf_components(x), tf_components(y), .f, ...))
}

#' @rdname tf_mv_methods
#' @export
tf_component <- function(f, which) {
  assert_tf_mv(f)
  comps <- tf_components(f)
  which <- check_component_index(which, comps, arg = "which")
  comps[[which]]
}

# resolve a component selector (single name or single integer index) to a
# valid positional index into `comps`, with informative errors.
check_component_index <- function(which, comps, arg = "which") {
  if (!length(comps)) {
    cli::cli_abort(
      "Cannot select component {.arg {arg}}: the object has no components."
    )
  }
  if (is.character(which)) {
    assert_string(which, .var.name = arg)
    loc <- match(which, names(comps))
    if (is.na(loc)) {
      cli::cli_abort(c(
        "Unknown component {.val {which}}.",
        "i" = "Available component{?s}: {.val {names(comps)}}."
      ))
    }
    return(loc)
  }
  if (
    !is.numeric(which) ||
      length(which) != 1L ||
      is.na(which) ||
      which != round(which) ||
      which < 1 ||
      which > length(comps)
  ) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a single component name or an integer index between 1 and {length(comps)}.",
      "x" = "You supplied {.val {which}}."
    ))
  }
  as.integer(which)
}

#' @rdname tf_mv_methods
#' @export
`tf_component<-` <- function(f, which, value) {
  assert_tf_mv(f)
  assert_tf(value)
  if (vec_size(value) != vec_size(f)) {
    cli::cli_abort(
      "Replacement component has length {vec_size(value)}, expected {vec_size(f)}."
    )
  }
  # Replacing a component invalidates the joint MFPC eigenbasis (the shared
  # scores no longer correspond to the new component), so warn and demote
  # before the value is swapped in. `tfb_mfpc_demote()` rebuilds the retained
  # components as standalone full-rank `tfb_fpc` -- merely dropping the `mfpc`
  # attribute would leave them with the abort-stub `scoring_function`, breaking
  # subsequent arithmetic / `tf_rebase()`. The same applies to a `value` taken
  # from this very fit (e.g. `mf2$x <- mf$x`), which is rebuilt as well.
  if (is_tfb_mfpc(f)) {
    warn_mfpc_demotion(
      "Replacing a component invalidates the joint MFPC eigenbasis."
    )
    value <- mfpc_demote_component_value(
      value,
      tf_components(f),
      attr(f, "mfpc")$uni
    )
    f <- tfb_mfpc_demote(f)
  }
  comps <- tf_components(f)
  if (is.character(which)) {
    # validate the scalar name *before* the vectorized `%in%` below, which
    # would otherwise error ("length > 1 in coercion to logical") on a
    # multi-element selector.
    assert_string(which, min.chars = 1L, .var.name = "which")
    if (which %in% names(comps)) {
      comps[[match(which, names(comps))]] <- value
    } else {
      # allow adding a new component by name
      comps[[which]] <- value
    }
  } else {
    which <- check_component_index(which, comps, arg = "which")
    comps[[which]] <- value
  }
  # new_tf_mv() validates that `value` is the same kind (tfd/tfb) as the other
  # components and that its domain is compatible. The joint MFPC spec was
  # dropped by tfb_mfpc_demote() above and is intentionally not forwarded.
  new_tf_mv(comps, domain = tf_domain(f))
}

#' @export
`$.tf_mv` <- function(x, name) tf_component(x, name)

#' @export
`$<-.tf_mv` <- function(x, name, value) {
  `tf_component<-`(x, name, value)
}

#-------------------------------------------------------------------------------

# do all components share (numerically) identical arg grids? `args` can be
# passed in when the caller has already extracted the per-component grids.
mv_args_shared <- function(f, args = map(tf_components(f), tf_arg)) {
  all_equal_to_first(args)
}

#' @export
tf_arg.tf_mv <- function(f) {
  comps <- tf_components(f)
  if (!length(comps)) return(numeric(0))
  args <- map(comps, tf_arg)
  all_agree <- mv_args_shared(f, args)
  if (any(map_lgl(comps, is_irreg))) {
    # all-irregular + per-curve args shared across components (the typical
    # movement-data case): collapse to a single per-curve list.
    if (all(map_lgl(comps, is_irreg)) && all_agree) return(args[[1]])
    # otherwise return per-component (genuinely different args per dim)
    return(args)
  }
  # all components are regular: collapse if they share the grid
  if (all_agree) return(args[[1]])
  args
}

# assemble per-component evaluation lists into a uniform list of per-curve
# data.frames with columns (arg, comp1, ..., compd). `grids` is either a single
# numeric vector (shared across curves) or a length-n list of per-curve grids.
# `comp_evals[[k]][[i]]` holds the k-th component's numeric evaluations at
# `grids[[i]]` (or `grids`); NA-fill where a component has no value at that arg.
assemble_mv_evals <- function(comp_evals, grids, comp_names, n) {
  if (!n) return(list())
  d <- length(comp_evals)
  shared_grid <- !is.list(grids)
  map(seq_len(n), function(i) {
    g <- if (shared_grid) grids else grids[[i]]
    if (!length(g)) g <- numeric(0)
    cols <- map(seq_len(d), function(k) {
      v <- comp_evals[[k]][[i]]
      if (is.null(v) || length(v) != length(g)) rep(NA_real_, length(g)) else v
    })
    names(cols) <- comp_names
    do.call(data_frame0, c(list(arg = g), cols))
  })
}

# private companion: drop the leading `arg` column and return the component
# evaluations as a plain numeric matrix [n_arg, d]. Used by callers (e.g.
# arclength_polyline) that need the matrix shape internally.
evals_to_matrix <- function(df) {
  if (!ncol(df) || ncol(df) == 1L) {
    return(matrix(numeric(0), nrow = nrow(df), ncol = 0))
  }
  mat <- as.matrix(df[, -1L, drop = FALSE])
  rownames(mat) <- NULL
  mat
}

tf_mv_curve_grids <- function(x) {
  n <- vec_size(x)
  comps <- tf_components(x)
  if (!length(comps)) {
    return(rep(list(numeric(0)), n))
  }
  # recompute the grid layout from the components instead of name-sniffing
  # the polymorphic tf_arg() return value (whose per-curve and per-component
  # list shapes are indistinguishable when n == d with colliding names)
  args <- map(comps, tf_arg)
  shared <- mv_args_shared(x, args)
  if (shared && !is_irreg(comps[[1]])) {
    # one shared numeric grid for all curves and components
    rep(list(args[[1]]), n)
  } else if (shared) {
    # all irregular with per-curve grids shared across components
    args[[1]]
  } else {
    # genuinely different grids per component: per-curve union grids
    comp_args <- map(args, \(a) if (is.list(a)) a else rep(list(a), n))
    lapply(seq_len(n), function(i) {
      sort(unique(unlist(
        lapply(comp_args, `[[`, i),
        use.names = FALSE
      )))
    })
  }
}

#' @export
tf_evaluations.tf_mv <- function(f) {
  # the no-`arg` branch of tf_evaluate.tf_mv is exactly this contract:
  # every component evaluated on each curve's union grid, one per-curve
  # data.frame with a shared `arg` column and NA-fill where a component has
  # no native observation.
  tf_evaluate(f)
}

#' @export
tf_count.tf_mv <- function(f) {
  comps <- tf_components(f)
  n <- vec_size(f)
  if (!length(comps) || n == 0L) {
    return(matrix(
      integer(0),
      nrow = n,
      ncol = length(comps),
      dimnames = list(names(f), attr(f, "comp_names"))
    ))
  }
  if (is_tfb_mv(f)) {
    cli::cli_abort(
      "{.fn tf_count} is not defined for basis-represented ({.cls tfb_mv}) data."
    )
  }
  counts <- map(comps, tf_count)
  mat <- do.call(cbind, counts)
  colnames(mat) <- attr(f, "comp_names")
  mat
}

#' @export
is.na.tf_mv <- function(x) {
  comp_na <- map(tf_components(x), is.na)
  if (!length(comp_na)) return(logical(0))
  Reduce(`|`, comp_na)
}

mv_complete <- function(x, na.rm = FALSE, missing = is.na(x)) {
  if (!length(missing) || !any(missing)) {
    return(x)
  }
  if (na.rm) {
    return(x[!missing])
  }
  comps <- map(tf_components(x), function(comp) {
    suppressWarnings(comp[missing] <- NA)
    comp
  })
  new_tf_mv(comps, domain = tf_domain(x))
}
