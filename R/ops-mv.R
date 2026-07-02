# Arithmetic, math, summaries (all component-wise) -----------------------------

# Demote a `tfb_mfpc` operand before Math/Ops touch its `tfb_fpc` components.
# Scoring a single MFPC component is ill-defined (see `mfpc_component_scoring`),
# so any arithmetic / Math.Generic must drop the joint spec first. We warn once
# per operation and continue along the standard component-wise path.
mfpc_demote_for_op <- function(x, op, warn = TRUE) {
  if (is_tfb_mfpc(x)) {
    if (warn) {
      warn_mfpc_demotion(paste0(
        "Operation {.code ", op, "} on a {.cls tfb_mfpc} forces per-component arithmetic."
      ))
    }
    return(tfb_mfpc_demote(x))
  }
  x
}

#' @export
#' @method vec_arith tf_mv
vec_arith.tf_mv <- function(op, x, y, ...) {
  UseMethod("vec_arith.tf_mv", y)
}

#' @export
#' @method vec_arith.tf_mv default
vec_arith.tf_mv.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.tf_mv tf_mv
vec_arith.tf_mv.tf_mv <- function(op, x, y, ...) {
  # if both operands are tfb_mfpc, warn only once for the single user-facing
  # operation: the warning for `y` is suppressed when `x` already triggered it.
  x_warns <- is_tfb_mfpc(x)
  x <- mfpc_demote_for_op(x, op)
  y <- mfpc_demote_for_op(y, op, warn = !x_warns)
  map2_components(x, y, \(a, b) vec_arith(op, a, b))
}

#' @export
#' @method vec_arith.tf_mv numeric
vec_arith.tf_mv.numeric <- function(op, x, y, ...) {
  x <- mfpc_demote_for_op(x, op)
  map_components(x, \(a) vec_arith(op, a, y))
}

#' @export
#' @method vec_arith.numeric tf_mv
vec_arith.numeric.tf_mv <- function(op, x, y, ...) {
  y <- mfpc_demote_for_op(y, op)
  map_components(y, \(b) vec_arith(op, x, b))
}

#' @export
#' @method vec_arith.tf_mv MISSING
vec_arith.tf_mv.MISSING <- function(op, x, y, ...) {
  x <- mfpc_demote_for_op(x, op)
  map_components(x, \(a) vec_arith(op, a, MISSING()))
}

#' @export
Math.tf_mv <- function(x, ...) {
  generic <- .Generic
  x <- mfpc_demote_for_op(x, generic)
  map_components(x, \(a) do.call(generic, list(a, ...)))
}

#' @export
Summary.tf_mv <- function(..., na.rm = FALSE) {
  generic <- .Generic
  dots <- list(...)
  mv_args <- map_lgl(dots, is_tf_mv)
  x <- dots[[which(mv_args)[1]]]
  walk(dots[mv_args], \(arg) check_compatible_mv(x, arg))
  missing <- do.call(mv_missing, dots[mv_args])
  dots[mv_args] <- map(
    dots[mv_args],
    mv_complete,
    missing = missing,
    na.rm = na.rm
  )
  x <- dots[[which(mv_args)[1]]]
  comps <- imap(tf_components(x), function(comp, nm) {
    comp_args <- map(dots, function(arg) {
      if (is_tf_mv(arg)) tf_component(arg, nm) else arg
    })
    do.call(generic, c(comp_args, list(na.rm = na.rm)))
  })
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
`==.tf_mv` <- function(e1, e2) {
  check_compatible_mv(e1, e2)
  # a zero-component object has no values to compare: trivially equal (and
  # `Reduce()` on an empty list would return `NULL` rather than `logical(0)`).
  if (!tf_ncomp(e1)) return(rep(TRUE, vec_size(e1)))
  eqs <- map2(tf_components(e1), tf_components(e2), \(a, b) a == b)
  Reduce(`&`, eqs)
}

#' @export
`!=.tf_mv` <- function(e1, e2) !(e1 == e2)

#' @export
mean.tf_mv <- function(x, ..., na.rm = FALSE) {
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, \(a) mean(a, ..., na.rm = na.rm))
}

#' Joint depth-median for vector-valued functional data
#'
#' The median of a `tf_mv` vector is the single *observed* curve with maximal
#' joint depth (see [tf_depth()]): one `which.max` index selects the same
#' observation across every component, so the result is never a "chimera"
#' stitched together from different curves. Note the deliberate divergence from
#' [median.tf()] on ties: the univariate median *averages* tied maximal-depth
#' curves, but averaging components would break the observed-curve guarantee,
#' so `median.tf_mv` returns the first tied curve (with a message). On tied
#' data, `median(f)$x` and `median(f$x)` can therefore differ.
#'
#' @param x a `tf_mv` vector.
#' @param na.rm if `FALSE` (default), any `NA` observation makes the result
#'   `NA`; if `TRUE`, `NA` observations are dropped first.
#' @param depth the joint depth method, see [tf_depth()].
#' @param ... passed to [tf_depth()].
#' @returns a length-1 `tf_mv`: the observed curve with maximal joint depth.
#' @export
#' @family tidyfun summary functions
median.tf_mv <- function(x, na.rm = FALSE, depth = "MBD", ...) {
  if (!na.rm && any(is.na(x))) {
    return(tf_na_like(x))
  }
  x <- x[!is.na(x)]
  if (!vec_size(x)) {
    return(x)
  }
  d <- tf_depth(x, depth = depth, na.rm = FALSE, ...)
  unname(x[depth_median_index(d)])
}

#' @export
sd.tf_mv <- function(x, na.rm = FALSE) {
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, \(a) sd(a, na.rm = na.rm))
}

#' @export
var.tf_mv <- function(x, y = NULL, na.rm = FALSE, use) {
  if (!is.null(y)) {
    cli::cli_abort(c(
      "{.fn var} on {.cls tf_mv} does not support a second argument {.arg y}.",
      "i" = "Use {.fn tf_crosscov} or {.fn tf_crosscor} for cross-(co)variance."
    ))
  }
  has_use <- !missing(use)
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, function(a) {
    if (has_use) {
      var(a, na.rm = na.rm, use = use)
    } else {
      var(a, na.rm = na.rm)
    }
  })
}
