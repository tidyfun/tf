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

# ---- summarize.R: quantile ---------------------------------------------------
# (`summary` / `fivenum` now have real joint-depth methods in summarize.R;
# `median` / `mean` / `sd` / `var` in ops-mv.R; `tf_depth` in depth.R.)

# `quantile.tf_mv` is implemented component-wise in R/depth.R (next to
# `quantile.tf`).

# ---- depth.R: rank, xtfrm, sort (permanently undefined) ----------------------
# (`min`/`max`/`range` are handled by the existing `Summary.tf_mv` group method;
# `cummax`/`cummin`/`cumprod`/`cumsum` by `Math.tf_mv`. They are not stubbed.)
#
# `sort`/`rank`/`xtfrm` are *permanently* undefined for `tf_mv`: there is no
# canonical total order on R^d. Users must reduce to a scalar first -- see
# `tf_order(f, by = )` for the ergonomic escape hatch. We keep the classed
# `tf_mv_method_unimplemented` condition so base `sort()`/`order()`/`rank()`
# (which route through `xtfrm`) fail fast rather than silently fabricate an
# order. See tidyfun/tf#273.

mv_no_total_order <- function(fn) {
  cli::cli_abort(
    c(
      "{.fn {fn}} is undefined for vector-valued {.cls tf_mv}: there is no \\
       canonical total order on {.field R^d}.",
      i = "Reduce to a scalar first, e.g. {.code tf_order(f, by = \"norm\")}, \\
           {.code f[order(tf_norm(f))]}, or order by a single component."
    ),
    class = "tf_mv_method_unimplemented",
    call = NULL
  )
}

#' @export
rank.tf_mv <- function(x, ...) mv_no_total_order("rank")

#' @export
xtfrm.tf_mv <- function(x) mv_no_total_order("xtfrm")

#' @export
sort.tf_mv <- function(x, decreasing = FALSE, ...) mv_no_total_order("sort")

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
#' without one aborts with a classed `tf_mv_method_unimplemented` condition.
#' This avoids silent fall-through to the univariate method, which would
#' otherwise produce wrong-shape results or deep internal errors.
#'
#' Some verbs are *permanently* stubbed because they have no well-defined
#' vector-valued semantics: `sort()`, `rank()` and `xtfrm()` (no canonical
#' total order on \eqn{R^d} -- use [tf_order()] with `by =` instead), and
#' `tf_invert()`. `tf_crosscov()` / `tf_crosscor()` remain blocked pending
#' their joint design (<https://github.com/tidyfun/tf/issues/274>).
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
