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

#' @export
quantile.tf_mv <- function(x, ...) mv_unimplemented("quantile")

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

# ---- methods.R: rev ----------------------------------------------------------

#' @export
rev.tf_mv <- function(x) mv_unimplemented("rev")

# ---- graphics.R: points ------------------------------------------------------

#' @export
points.tf_mv <- function(x, ...) mv_unimplemented("points")

# ---- interpolate.R: tf_interpolate -------------------------------------------

#' @export
tf_interpolate.tf_mv <- function(object, arg, ...) mv_unimplemented("tf_interpolate")

# ---- approx.R: tf_invert -----------------------------------------------------

#' @export
tf_invert.tf_mv <- function(x, ...) mv_unimplemented("tf_invert")

# ---- where.R: tf_where / tf_anywhere -----------------------------------------
# tf_where / tf_anywhere are converted to S3 generics in where.R; tf_anywhere
# delegates to tf_where so a single stub catches both, but we also provide an
# explicit stub for symmetry/clarity.

#' @export
tf_where.tf_mv <- function(f, cond, ...) mv_unimplemented("tf_where")

#' @export
tf_anywhere.tf_mv <- function(f, cond, ...) mv_unimplemented("tf_anywhere")

# ---- fwise.R: tf_fwise / tf_fmean / tf_crosscov / tf_crosscor ----------------
# Converted to generics in fwise.R (default method retains the univariate body).

#' @export
tf_fwise.tf_mv <- function(x, .f, ...) mv_unimplemented("tf_fwise")

#' @export
tf_fmean.tf_mv <- function(x, ...) mv_unimplemented("tf_fmean")

#' @export
tf_crosscov.tf_mv <- function(x, y, ...) mv_unimplemented("tf_crosscov")

#' @export
tf_crosscor.tf_mv <- function(x, y, ...) mv_unimplemented("tf_crosscor")

# ---- rng.R: tf_sparsify / tf_jiggle ------------------------------------------

#' @export
tf_sparsify.tf_mv <- function(f, ...) mv_unimplemented("tf_sparsify")

#' @export
tf_jiggle.tf_mv <- function(f, ...) mv_unimplemented("tf_jiggle")


#' Methods registered on vector-valued (`tf_mv`) classes
#'
#' `tf_mv` classes (`tfd_mv` / `tfb_mv`) inherit from `"tf"` so that
#' `tf_domain()`, the type predicates (`is_tf()`, `is_tf_mv()`, ...) and S4
#' generic reuse continue to work. **Behaviour** on `tf_mv` objects, however,
#' is supplied *only* by explicitly registered `.tf_mv` methods: any generic
#' without one aborts with a classed `tf_mv_method_unimplemented` condition
#' (e.g. `tf_where(<tf_mv>, <cond>)`, `summary(<tf_mv>)`). This avoids silent
#' fall-through to the univariate method, which would otherwise produce
#' wrong-shape results or deep internal errors.
#'
#' Real component-wise semantics (joint vs. per-component, norm-based, ...) are
#' being designed verb-by-verb in <https://github.com/tidyfun/tf/issues/255>.
#'
#' @name tf_mv_unimplemented
#' @keywords internal
NULL
