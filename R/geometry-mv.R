# Geometric primitives for vector-valued curves --------------------------------

#' Pointwise norm and inner product for functional data
#'
#' Small geometric helpers defined by component-wise composition of the existing
#' univariate `Ops` / `Math` machinery:
#' - `tf_norm(f)`     -- pointwise Euclidean norm \eqn{\lVert f(t) \rVert};
#' - `tf_speed(f)`    -- pointwise speed \eqn{\lVert f'(t) \rVert};
#' - `tf_inner(f, g)` -- pointwise inner product \eqn{\langle f(t), g(t) \rangle};
#' - `tf_distance(f, g)` -- pointwise Euclidean distance \eqn{\lVert f(t) - g(t) \rVert};
#' - `tf_tangent(f)`  -- unit tangent \eqn{f'(t) / \lVert f'(t) \rVert}
#'   (undefined where the speed is zero -- callers get `NaN`s there);
#' - `tf_reparam_arclength(f)` -- re-parametrize the curve at constant speed
#'   (i.e. by its normalized cumulative arc length).
#'
#' These also apply to *univariate* `tfd`/`tfb` (treated as scalar-valued curves
#' \eqn{f: T \to \mathbb{R}}), where they reduce to their one-dimensional
#' specializations: \eqn{\lVert f(t) \rVert = |f(t)|},
#' \eqn{\langle f(t), g(t) \rangle = f(t)\,g(t)}, and the unit tangent
#' \eqn{f'(t) / |f'(t)| = \mathrm{sign}(f'(t))}.
#'
#' @param f,g `tf_mv` objects, or univariate `tf` (`tfd`/`tfb`) objects (with
#'   identical `d` and component names where two `tf_mv` arguments are required).
#' @returns a univariate `tfd` for `tf_norm`/`tf_speed`/`tf_inner`/`tf_distance`;
#'   `tf_tangent` returns a `tf_mv` (or a univariate `tf` for univariate input)
#'   and `tf_reparam_arclength` a `tf_mv`.
#' @family tf_mv-class
#' @examples
#' set.seed(1)
#' f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
#' tf_norm(f)
#' tf_speed(f)
#' tf_distance(f, tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2))))
#' # univariate: tf_norm reduces to the pointwise absolute value
#' u <- tf_rgp(2)
#' tf_norm(u)
#' tf_inner(u, tf_rgp(2))
#' @name tf_geom
#' @rdname tf_geom
#' @export
tf_norm <- function(f) UseMethod("tf_norm")

#' @rdname tf_geom
#' @export
tf_norm.default <- function(f) {
  cli::cli_abort(c(
    "{.fn tf_norm} is not defined for {.cls {class(f)}} objects.",
    "i" = "Supply a univariate {.cls tf} or a vector-valued {.cls tf_mv} object."
  ))
}

#' @rdname tf_geom
#' @export
tf_norm.tf <- function(f) abs(f)

# Reduce a tf_mv (or a pair of aligned tf_mvs) to a univariate `tfd` by applying
# a pointwise `reduce` to each curve's component-value matrix. `evals` is the
# per-curve list of `(arg, comp...)` data.frames from `tf_evaluate()`; the arg
# grids are read back from those. For the two-operand form, `evals2` holds the
# matching data.frames for the second object and `reduce(m1, m2)` is called.
# This is the shared tail of the tfd-valued geometry reductions (norm, inner).
mv_reduce_to_tfd <- function(evals, reduce, domain, nms, evals2 = NULL) {
  vals <- if (is.null(evals2)) {
    map(evals, \(cdf) if (nrow(cdf)) reduce(evals_to_matrix(cdf)) else numeric(0))
  } else {
    map2(evals, evals2, \(a, b) {
      if (nrow(a)) reduce(evals_to_matrix(a), evals_to_matrix(b)) else numeric(0)
    })
  }
  names(vals) <- nms
  tfd(vals, arg = map(evals, `[[`, "arg"), domain = domain)
}

#' @rdname tf_geom
#' @export
tf_norm.tf_mv <- function(f) {
  if (!tf_ncomp(f) || !vec_size(f)) return(tfd(numeric(0), domain = tf_domain(f)))
  # Components may live on different argument grids (the constructor allows
  # this), so evaluate every component on each curve's union grid -- a plain
  # `Reduce(`+`, comp^2)` would error or misalign, and on tfb would rebase
  # `comp^2` lossily. `tf_evaluate()` does the union-grid + NA-fill.
  mv_reduce_to_tfd(tf_evaluate(f), \(m) sqrt(rowSums(m^2)), tf_domain(f), names(f))
}

#' @rdname tf_geom
#' @export
tf_speed <- function(f) tf_norm(tf_derive(f))

#' @rdname tf_geom
#' @export
tf_inner <- function(f, g) UseMethod("tf_inner")

#' @rdname tf_geom
#' @export
tf_inner.default <- function(f, g) {
  cli::cli_abort(c(
    "{.fn tf_inner} is not defined for {.cls {class(f)}} objects.",
    "i" = "Supply univariate {.cls tf} or vector-valued {.cls tf_mv} objects."
  ))
}

#' @rdname tf_geom
#' @export
tf_inner.tf <- function(f, g) {
  if (!inherits(g, "tf") || is_tf_mv(g)) {
    cli::cli_abort(c(
      "{.arg g} must be a univariate {.cls tf} object to match {.arg f}.",
      "x" = "You supplied a {.cls {class(g)}} object."
    ))
  }
  f * g
}

#' @rdname tf_geom
#' @export
tf_inner.tf_mv <- function(f, g) {
  if (!is_tf_mv(g)) {
    cli::cli_abort(c(
      "{.arg g} must be a vector-valued {.cls tf_mv} object to match {.arg f}.",
      "x" = "You supplied a {.cls {class(g)}} object."
    ))
  }
  check_compatible_mv(f, g)
  assert_compatible_size("tf_inner", f, g)
  # vctrs recycling: a size-0 operand makes the whole result size 0.
  n <- if (min(vec_size(f), vec_size(g)) == 0L) {
    0L
  } else {
    max(vec_size(f), vec_size(g))
  }
  if (!tf_ncomp(f) || !n) return(tfd(numeric(0), domain = tf_domain(f)))
  # the inner product is only defined where both curves are -- the intersection
  # of their domains. Evaluating on the union would step outside one operand's
  # support (mirrors univariate `tfd * tfd`, which errors on non-overlapping
  # domains and restricts to the common arguments otherwise).
  dom <- c(
    max(tf_domain(f)[1], tf_domain(g)[1]),
    min(tf_domain(f)[2], tf_domain(g)[2])
  )
  if (dom[1] > dom[2]) {
    cli::cli_abort(
      "{.fn tf_inner} is not permitted for non-overlapping domains
       ({.val {tf_domain(f)}} and {.val {tf_domain(g)}})."
    )
  }
  grids <- tf_mv_pair_grids(f, g, domain = dom)
  nms <- if (vec_size(f) >= vec_size(g)) names(f) else names(g)
  mv_reduce_to_tfd(
    tf_mv_evaluate_on_grids(f, grids),
    \(mf, mg) rowSums(mf * mg),
    dom, nms,
    evals2 = tf_mv_evaluate_on_grids(g, grids)
  )
}

#' @rdname tf_geom
#' @export
tf_distance <- function(f, g) tf_norm(f - g)

#' @rdname tf_geom
#' @export
tf_tangent <- function(f) UseMethod("tf_tangent")

#' @rdname tf_geom
#' @export
tf_tangent.default <- function(f) {
  cli::cli_abort(c(
    "{.fn tf_tangent} is not defined for {.cls {class(f)}} objects.",
    "i" = "Supply a univariate {.cls tf} or a vector-valued {.cls tf_mv} object."
  ))
}

#' @rdname tf_geom
#' @export
tf_tangent.tf <- function(f) {
  df <- tf_derive(f)
  df / tf_norm(df)
}

#' @rdname tf_geom
#' @export
tf_tangent.tf_mv <- function(f) {
  df <- tf_derive(f)
  inv_speed <- 1 / tf_norm(df)
  grids <- tf_mv_curve_grids(df)
  df_evals <- tf_evaluate(df, arg = grids)
  speed_evals <- tf_evaluate(inv_speed, arg = grids)
  comp_names <- attr(df, "comp_names")
  comps <- map(comp_names, function(nm) {
    vals <- map2(df_evals, speed_evals, \(cdf, speed) cdf[[nm]] * speed)
    names(vals) <- names(df)
    tfd(vals, arg = grids, domain = tf_domain(df))
  })
  names(comps) <- comp_names
  new_tf_mv(comps, domain = tf_domain(df))
}

tf_mv_pair_grids <- function(x, y, domain = NULL) {
  sizes <- c(vec_size(x), vec_size(y))
  # vctrs recycling: a size-0 operand collapses the common size to 0.
  n <- if (min(sizes) == 0L) 0L else max(sizes)
  if (!n) return(list())
  x_grids <- tf_mv_curve_grids(x)
  y_grids <- tf_mv_curve_grids(y)
  grid_at <- function(grids, i) {
    if (!length(grids)) return(numeric(0))
    grids[[if (length(grids) == 1L) 1L else i]]
  }
  map(seq_len(n), function(i) {
    g <- sort(unique(c(grid_at(x_grids, i), grid_at(y_grids, i))))
    if (!is.null(domain)) g <- g[g >= domain[1] & g <= domain[2]]
    g
  })
}

tf_mv_evaluate_on_grids <- function(x, grids) {
  if (vec_size(x) == length(grids)) {
    return(tf_evaluate(x, arg = grids))
  }
  tf_evaluate(x[rep(1L, length(grids))], arg = grids)
}

#' @rdname tf_geom
#' @export
tf_reparam_arclength <- function(f) {
  if (!vec_size(f)) return(f)
  s <- tf_arclength(f, definite = FALSE) # cumulative s(t), one per curve
  L <- tf_arclength(f) # total length per curve
  dom <- tf_domain(f)
  # curves that are constant in every component have zero (or undefined) arc
  # length, so `s / L` would be 0/0 = NaN and produce an invalid (non-monotone)
  # warp. Reparametrize only the well-defined curves; leave the rest unchanged.
  degenerate <- !is.finite(L) | L == 0
  good <- which(!degenerate)
  if (!length(good)) {
    if (any(degenerate)) {
      cli::cli_warn(c(
        "!" = "{sum(degenerate)} curve{?s} with zero/undefined arc length left unchanged.",
        "i" = "Arc-length reparametrization is undefined for curves that are constant in all components."
      ))
    }
    return(f)
  }
  # u(t) maps the domain monotonically onto itself. `tf_warp(f, w)` computes
  # `f o w^{-1}`, so passing `u` (not its inverse) gives the arc-length-
  # parameterised curve `f o u^{-1}`.
  u <- dom[1] + diff(dom) * (s[good] / L[good])
  warped <- tf_warp(f[good], u)
  if (!any(degenerate)) {
    return(warped)
  }
  # `warped` may have a more general per-component type than `f` (e.g.
  # tfd_irreg from a tfd_reg input), so in-place `out[good] <- warped`
  # can fail with a vctrs ptype mismatch. Build the output by ptype-aware
  # concatenation of warped + untouched, then reorder back to input index.
  cli::cli_warn(c(
    "!" = "{sum(degenerate)} curve{?s} with zero/undefined arc length left unchanged.",
    "i" = "Arc-length reparametrization is undefined for curves that are constant in all components."
  ))
  bad <- which(degenerate)
  common <- vctrs::vec_ptype_common(warped, f[bad])
  out <- vctrs::vec_c(
    vctrs::vec_cast(warped, common),
    vctrs::vec_cast(f[bad], common)
  )
  out <- out[order(c(good, bad))]
  names(out) <- names(f)
  out
}

# Arc length -------------------------------------------------------------------

#' Arc length of vector-valued functional data
#'
#' For a vector-valued curve `f: [a, b] -> R^d`, the arc length is
#' \eqn{\int_a^b \lVert f'(t) \rVert\, dt} -- the length traced out by `f` in
#' `R^d`.
#'
#' Two methods are supported:
#'
#' * **`"polyline"`** (default): sum of the Euclidean lengths of the line
#'   segments between consecutive sample points (in `R^d`). Each curve is
#'   evaluated on the union of its components' argument grids (or a supplied
#'   `arg`) and the segment-sum is computed in closed form. For raw `tfd_mv`
#'   data this is more accurate than `"derive"` because it avoids the
#'   compounding error of numerical differentiation followed by quadrature.
#' * **`"derive"`**: composes the existing verbs -- per-component
#'   differentiation ([tf_derive()]), pointwise speed [tf_speed()], then
#'   [tf_integrate()]. Best for `tfb_mv` (analytical derivatives) or when a
#'   custom `tf_integrate(...)` argument is needed.
#'
#' @param f a `tf_mv` object.
#' @param arg,lower,upper optional evaluation/integration grid and limits.
#' @param definite `TRUE` (default) returns a numeric vector of total arc
#'   lengths per curve; `FALSE` returns the cumulative arc length
#'   \eqn{s(t) = \int_a^t \lVert f'(u) \rVert\, du} as a univariate `tfd`.
#' @param method `"polyline"` (default) or `"derive"`.
#' @param ... forwarded to [tf_integrate()] when `method = "derive"`.
#' @returns a numeric vector (definite) or a univariate `tfd` (indefinite).
#' @family tf_mv-class
#' @examples
#' # unit circle parameterised on [0, 1] -- arc length is 2*pi
#' t <- seq(0, 1, length.out = 401)
#' circ <- tfd_mv(list(
#'   x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
#'   y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
#' ))
#' tf_arclength(circ)
#' tf_arclength(circ, lower = 0, upper = 0.25) # quarter -> pi/2
#' tf_arclength(circ, definite = FALSE)        # cumulative s(t)
#' @export
tf_arclength <- function(f, ...) UseMethod("tf_arclength")

#' @rdname tf_arclength
#' @export
tf_arclength.default <- function(f, ...) .NotYetImplemented()

#' @rdname tf_arclength
#' @export
tf_arclength.tf_mv <- function(
  f,
  arg = NULL,
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  method = c("polyline", "derive"),
  ...
) {
  method <- match.arg(method)
  assert_number(lower, finite = TRUE)
  assert_number(upper, finite = TRUE)
  if (lower > upper) {
    cli::cli_abort(c(
      "{.arg lower} must not exceed {.arg upper}.",
      "x" = "You supplied {.arg lower} = {.val {lower}} and {.arg upper} = {.val {upper}}."
    ))
  }
  dom <- tf_domain(f)
  if (lower < dom[1] || upper > dom[2]) {
    cli::cli_abort(c(
      "{.arg lower}/{.arg upper} must lie within the domain {.val {dom}}.",
      "x" = "You supplied {.arg lower} = {.val {lower}} and {.arg upper} = {.val {upper}}."
    ))
  }
  if (!definite && lower == upper) {
    cli::cli_abort(c(
      "Cumulative arc length ({.code definite = FALSE}) is undefined for a \\
       zero-width interval.",
      "x" = "You supplied {.arg lower} == {.arg upper} == {.val {lower}}."
    ))
  }
  if (method == "derive") {
    speed <- tf_speed(f)
    call_args <- list(
      speed,
      lower = lower,
      upper = upper,
      definite = definite,
      ...
    )
    if (!is.null(arg)) call_args$arg <- arg
    return(do.call(tf_integrate, call_args))
  }
  arclength_polyline(f, arg, lower, upper, definite)
}

# Polyline arc length: evaluate the multivariate curve on each curve's
# argument grid (or a supplied common `arg`), then sum Euclidean distances
# between consecutive d-dimensional sample points.
arclength_polyline <- function(f, arg, lower, upper, definite) {
  n <- vec_size(f)
  if (!n) {
    return(if (definite) numeric(0) else tfd(numeric(0)))
  }
  # per-curve evaluation grids
  grids <- if (!is.null(arg)) {
    rep(list(sort(unique(arg))), n)
  } else {
    tf_mv_curve_grids(f)
  }
  # clamp each curve's grid to the intersection of [lower, upper] with its own
  # observed argument range. Without this, irregular curves that don't span the
  # full global domain would be evaluated outside their support and yield NA.
  grids <- lapply(grids, function(g) {
    if (lower == upper) return(lower)
    if (!length(g)) return(numeric(0))
    lo_i <- max(lower, min(g))
    up_i <- min(upper, max(g))
    if (lo_i > up_i) return(numeric(0))
    g <- g[g >= lo_i & g <= up_i]
    sort(unique(c(lo_i, g, up_i)))
  })
  empty <- vapply(grids, function(g) length(g) == 0L, logical(1))
  needs_eval <- vapply(grids, function(g) length(g) >= 2L, logical(1))
  paired_dfs <- vector("list", n)
  if (any(needs_eval)) {
    paired_dfs[needs_eval] <- tf_evaluate(
      f[needs_eval],
      arg = grids[needs_eval]
    )
  }
  # `tf_evaluate.tf_mv` now returns a uniform list of per-curve data.frames
  # `(arg, comp1, ..., compd)`. Drop the arg column to get the value matrix
  # required by the polyline segment-length computation.
  incomplete <- map_lgl(paired_dfs, function(df) {
    is.data.frame(df) && nrow(df) > 0L && anyNA(df[, -1L, drop = FALSE])
  })
  if (any(incomplete)) {
    idx <- which(incomplete)
    cli::cli_abort(c(
      "Cannot compute polyline arc length with missing paired component evaluations.",
      "i" = "Affected curve {cli::qty(length(idx))}index{?/es}: {.val {idx}}.",
      "i" = "Set {.arg lower}/{.arg upper} to a common observed interval or use an evaluator that supplies all requested component values."
    ))
  }
  per_curve_segs <- map(seq_len(n), function(i) {
    if (empty[i]) return(NA_real_)
    if (length(grids[[i]]) < 2L) return(numeric(0))
    df <- paired_dfs[[i]]
    if (is.null(df) || !nrow(df)) return(NA_real_)
    mat <- evals_to_matrix(df)
    if (nrow(mat) < 2L) return(numeric(0))
    sqrt(rowSums(diff(mat)^2))
  })
  if (definite) {
    setNames(
      map_dbl(per_curve_segs, \(s) if (anyNA(s)) NA_real_ else sum(s)),
      names(f)
    )
  } else {
    if (any(map_lgl(per_curve_segs, anyNA))) {
      cli::cli_abort(
        "Cannot compute cumulative arc length for missing vector-valued curves."
      )
    }
    cum_evals <- map(per_curve_segs, function(s) c(0, cumsum(s)))
    same_grid <- length(unique(lengths(grids))) == 1L &&
      all(map_lgl(grids[-1], \(g) isTRUE(all.equal(g, grids[[1]]))))
    if (same_grid) {
      tfd(do.call(rbind, cum_evals), arg = grids[[1]])
    } else {
      tfd(cum_evals, arg = grids)
    }
  }
}
