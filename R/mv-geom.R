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

#' @rdname tf_geom
#' @export
tf_norm.tf_mv <- function(f) {
  comps <- tf_components(f)
  if (!length(comps)) return(tfd(numeric(0)))
  sqrt(Reduce(`+`, map(comps, \(comp) comp^2)))
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
  prods <- map2(tf_components(f), tf_components(g), \(a, b) a * b)
  if (!length(prods)) return(tfd(numeric(0)))
  Reduce(`+`, prods)
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
  map_components(df, \(comp) comp * inv_speed)
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
  out <- f
  good <- which(!degenerate)
  if (length(good)) {
    # u(t) maps the domain monotonically onto itself. `tf_warp(f, w)` computes
    # `f o w^{-1}`, so passing `u` (not its inverse) gives the arc-length-
    # parameterised curve `f o u^{-1}`.
    u <- dom[1] + diff(dom) * (s[good] / L[good])
    out[good] <- tf_warp(f[good], u)
  }
  if (any(degenerate)) {
    cli::cli_warn(c(
      "!" = "{sum(degenerate)} curve{?s} with zero/undefined arc length left unchanged.",
      "i" = "Arc-length reparametrization is undefined for curves that are constant in all components."
    ))
  }
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
    if (!length(g)) return(numeric(0))
    lo_i <- max(lower, min(g))
    up_i <- min(upper, max(g))
    if (lo_i > up_i) return(numeric(0))
    g <- g[g >= lo_i & g <= up_i]
    sort(unique(c(lo_i, g, up_i)))
  })
  empty <- vapply(grids, function(g) length(g) < 2L, logical(1))
  paired_evals <- vector("list", n)
  if (any(!empty)) {
    paired_evals[!empty] <- tf_evaluate(f[!empty], arg = grids[!empty])
  }
  incomplete <- map_lgl(paired_evals, \(mat) is.matrix(mat) && anyNA(mat))
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
    mat <- paired_evals[[i]]
    if (is.null(mat)) return(NA_real_)
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
