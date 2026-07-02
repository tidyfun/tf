#' @include tfd-mv.R tfb-mv.R accessors-mv.R
NULL

# Fail-fast .tf_mv stubs ------------------------------------------------------
#
# Many univariate-tf generics do not (yet) have a well-defined extension to
# vector-valued `tf_mv` objects. Without an explicit method, dispatch falls
# through to the .tf / .tfd / .tfb method (since tf_mv classes inherit from
# "tf"), which silently produces NA garbage, internal errors deep in the call
# stack, or component-wise behaviour with unclear semantics. The stubs below
# abort *fast* with a classed condition (`tf_mv_method_unimplemented`) so that
# callers can catch them uniformly while we design real semantics
# component-by-component -- see tidyfun/tf#255.

mv_unimplemented <- function(fn) {
  cli::cli_abort(
    c(
      "{.fn {fn}} is not (yet) defined for vector-valued {.cls tf_mv}.",
      i = "See {.url https://github.com/tidyfun/tf/issues/255} for design discussion."
    ),
    class = "tf_mv_method_unimplemented",
    call = NULL
  )
}

# ---- summarize.R: summary / fivenum / quantile -------------------------------

#' @export
summary.tf_mv <- function(object, ...) mv_unimplemented("summary")

#' @export
fivenum.tf_mv <- function(x, na.rm = FALSE, ...) mv_unimplemented("fivenum")

# `quantile.tf_mv` is implemented component-wise in R/depth.R (next to
# `quantile.tf`).

# ---- depth.R: tf_depth, rank, xtfrm, sort -----------------------------------
# (`min`/`max`/`range` are handled by the existing `Summary.tf_mv` group method;
# `cummax`/`cummin`/`cumprod`/`cumsum` by `Math.tf_mv`. They are not stubbed.)

#' @export
tf_depth.tf_mv <- function(x, arg, ...) mv_unimplemented("tf_depth")

#' @export
rank.tf_mv <- function(x, ...) mv_unimplemented("rank")

#' @export
xtfrm.tf_mv <- function(x) mv_unimplemented("xtfrm")

#' @export
sort.tf_mv <- function(x, decreasing = FALSE, ...) mv_unimplemented("sort")

# `points.tf_mv` is implemented in R/plot-mv.R (mirroring `lines.tf_mv`).

# ---- calculus.R: tf_invert (permanently undefined) ---------------------------
# Function inversion is only defined for monotone *scalar* functions. A
# vector-valued f: R -> R^d has no inverse in this sense, so this is not a
# "not yet" stub but a permanent one -- we keep the `tf_mv_method_unimplemented`
# class (the walker contract test relies on it) but give a definitive message.

#' @export
tf_invert.tf_mv <- function(x, ...) {
  cli::cli_abort(
    c(
      "{.fn tf_invert} is not defined for vector-valued {.cls tf_mv}.",
      i = "Inversion requires a monotone scalar function; a vector-valued
           {.cls tf_mv} has no such inverse.",
      i = "If a single component is monotone, invert it per component, e.g.
           {.code tf_invert(f$x)}."
    ),
    class = "tf_mv_method_unimplemented",
    call = NULL
  )
}

# ---- fwise.R: tf_crosscov / tf_crosscor --------------------------------------
# Converted to generics in fwise.R (default method retains the univariate body).

#' @export
tf_crosscov.tf_mv <- function(x, y, ...) mv_unimplemented("tf_crosscov")

#' @export
tf_crosscor.tf_mv <- function(x, y, ...) mv_unimplemented("tf_crosscor")

#' Methods registered on vector-valued (`tf_mv`) classes
#'
#' `tf_mv` classes (`tfd_mv` / `tfb_mv`) inherit from `"tf"` so that
#' `tf_domain()`, the type predicates (`is_tf()`, `is_tf_mv()`, ...) and S4
#' generic reuse continue to work. **Behaviour** on `tf_mv` objects, however,
#' is supplied *only* by explicitly registered `.tf_mv` methods: any generic
#' without one aborts with a classed `tf_mv_method_unimplemented` condition
#' (e.g. `tf_depth(<tf_mv>)`, `summary(<tf_mv>)`). This avoids silent
#' fall-through to the univariate method, which would otherwise produce
#' wrong-shape results or deep internal errors.
#'
#' Real component-wise semantics (joint vs. per-component, norm-based, ...) are
#' being designed verb-by-verb in <https://github.com/tidyfun/tf/issues/255>.
#' `summary()`, `fivenum()`, `rank()`, `xtfrm()`, `sort()` and `tf_depth()`
#' await the multivariate-depth design in
#' <https://github.com/tidyfun/tf/issues/273>, and `tf_crosscov()` /
#' `tf_crosscor()` await <https://github.com/tidyfun/tf/issues/274>.
#' `tf_invert()` is *permanently* undefined for `tf_mv`: function inversion
#' requires a monotone scalar function, which a vector-valued `f: R -> R^d`
#' is not (invert a monotone component instead, e.g. `tf_invert(f$x)`).
#'
#' @name tf_mv_unimplemented
#' @keywords internal
NULL
