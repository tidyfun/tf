#' @include tfd-mv.R tfb-mv.R mv-vctrs.R
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
#' @rdname tf_mv-methods
#' @export
tf_ncomp <- function(f) length(attr(f, "components"))

#' @rdname tf_mv-methods
#' @export
tf_components <- function(f) attr(f, "components")

#' @rdname tf_mv-methods
#' @export
tf_component <- function(f, which) {
  comps <- tf_components(f)
  if (is.character(which)) {
    which <- match.arg(which, names(comps))
  }
  comps[[which]]
}

#' @rdname tf_mv-methods
#' @export
`tf_component<-` <- function(f, which, value) {
  assert_tf(value)
  if (vec_size(value) != vec_size(f)) {
    cli::cli_abort(
      "Replacement component has length {vec_size(value)}, expected {vec_size(f)}."
    )
  }
  comps <- tf_components(f)
  if (is.character(which) && !(which %in% names(comps))) {
    # allow adding a new component by name
    comps[[which]] <- value
  } else {
    if (is.character(which)) which <- match(which, names(comps))
    comps[[which]] <- value
  }
  new_tf_mv(comps, domain = tf_domain(f))
}

#' @export
`$.tf_mv` <- function(x, name) tf_component(x, name)

#' @export
`$<-.tf_mv` <- function(x, name, value) {
  `tf_component<-`(x, name, value)
}

#-------------------------------------------------------------------------------

#' @export
tf_arg.tf_mv <- function(f) {
  comps <- tf_components(f)
  if (!length(comps)) return(numeric(0))
  args <- map(comps, tf_arg)
  all_agree <- length(args) == 1L ||
    all(map_lgl(args[-1], \(a) isTRUE(all.equal(a, args[[1]]))))
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

# assemble per-component evaluation lists into a list of (n_arg x d) matrices
assemble_mv_evals <- function(comp_evals, comp_names, n) {
  if (!n) return(list())
  map(seq_len(n), function(i) {
    cols <- map(comp_evals, \(ce) ce[[i]])
    if (any(map_lgl(cols, is.null))) return(NULL)
    if (length(unique(lengths(cols))) > 1L) {
      # components on differing grids: cannot form a single matrix
      return(setNames(cols, comp_names))
    }
    mat <- do.call(cbind, cols)
    colnames(mat) <- comp_names
    mat
  })
}

#' @export
tf_evaluations.tf_mv <- function(f) {
  comp_evals <- map(tf_components(f), tf_evaluations)
  assemble_mv_evals(comp_evals, attr(f, "comp_names"), vec_size(f))
}

#' @export
tf_count.tf_mv <- function(f) {
  counts <- map(tf_components(f), tf_count)
  mat <- do.call(cbind, counts)
  if (!is.null(mat)) colnames(mat) <- attr(f, "comp_names")
  mat
}

#' @export
is.na.tf_mv <- function(x) {
  comp_na <- map(tf_components(x), is.na)
  if (!length(comp_na)) return(logical(0))
  Reduce(`|`, comp_na)
}

#-------------------------------------------------------------------------------
# class predicates already live in methods.R; mv-specific ones too.

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
`[.tf_mv` <- function(x, i, j, interpolate = TRUE, matrix = TRUE, component = NULL) {
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
    j <- tf_arg(xi)
  }

  comps_i <- tf_components(xi)
  if (matrix) {
    if (missing(j)) {
      arg_vals <- tf_arg(xi)
      j <- if (is.list(arg_vals)) sort_unique(arg_vals, simplify = TRUE) else arg_vals
    }
    mats <- map(comps_i, \(comp) comp[, j, interpolate = interpolate, matrix = TRUE])
    arr <- array(
      unlist(mats, use.names = FALSE),
      dim = c(nrow(mats[[1]]), ncol(mats[[1]]), length(comps_i)),
      dimnames = list(rownames(mats[[1]]), colnames(mats[[1]]), comp_names)
    )
    return(arr)
  }
  # matrix = FALSE: list of per-curve data.frames with arg + one col per comp
  dfs <- map(comps_i, \(comp) comp[, j, interpolate = interpolate, matrix = FALSE])
  n_i <- vec_size(xi)
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

# Arithmetic, math, summaries (all component-wise) -----------------------------

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
  check_compatible_mv(x, y)
  comps <- map2(tf_components(x), tf_components(y), \(a, b) vec_arith(op, a, b))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
#' @method vec_arith.tf_mv numeric
vec_arith.tf_mv.numeric <- function(op, x, y, ...) {
  comps <- map(tf_components(x), \(a) vec_arith(op, a, y))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
#' @method vec_arith.numeric tf_mv
vec_arith.numeric.tf_mv <- function(op, x, y, ...) {
  comps <- map(tf_components(y), \(b) vec_arith(op, x, b))
  names(comps) <- attr(y, "comp_names")
  new_tf_mv(comps)
}

#' @export
#' @method vec_arith.tf_mv MISSING
vec_arith.tf_mv.MISSING <- function(op, x, y, ...) {
  comps <- map(tf_components(x), \(a) vec_arith(op, a, MISSING()))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
Math.tf_mv <- function(x, ...) {
  generic <- .Generic
  comps <- map(tf_components(x), \(a) do.call(generic, list(a, ...)))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
Summary.tf_mv <- function(..., na.rm = FALSE) {
  generic <- .Generic
  x <- ..1
  comps <- map(
    tf_components(x),
    \(a) do.call(generic, list(a, na.rm = na.rm))
  )
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
`==.tf_mv` <- function(e1, e2) {
  check_compatible_mv(e1, e2)
  eqs <- map2(tf_components(e1), tf_components(e2), \(a, b) a == b)
  Reduce(`&`, eqs)
}

#' @export
`!=.tf_mv` <- function(e1, e2) !(e1 == e2)

#' @export
mean.tf_mv <- function(x, ...) {
  comps <- map(tf_components(x), \(a) mean(a, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
median.tf_mv <- function(x, na.rm = FALSE, ...) {
  comps <- map(tf_components(x), \(a) median(a, na.rm = na.rm, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
sd.tf_mv <- function(x, ...) {
  comps <- map(tf_components(x), \(a) sd(a, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
var.tf_mv <- function(x, ...) {
  comps <- map(tf_components(x), \(a) var(a, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

# Printing / formatting --------------------------------------------------------

#' @export
format.tf_mv <- function(x, ...) {
  comps <- tf_components(x)
  if (!length(comps)) return(character(0))
  per_comp <- map(comps, \(comp) format(comp, ...))
  n <- vec_size(x)
  map_chr(seq_len(n), function(i) {
    paste(map_chr(per_comp, \(p) p[i]), collapse = " | ")
  })
}

#' @export
print.tf_mv <- function(x, n = 6, ...) {
  comp_names <- attr(x, "comp_names")
  d <- tf_ncomp(x)
  domain <- tf_domain(x) |> map_chr(format)
  if (d == 0L) {
    range_str <- "R^0"
  } else {
    range_str <- map_chr(tf_components(x), function(comp) {
      r <- suppressWarnings(safe_range_evals(comp)) |> map_chr(format)
      paste0("[", r[1], ", ", r[2], "]")
    }) |>
      paste(collapse = " x ")
  }
  cat(paste0(
    class(x)[1], "<d=", d, ">[", length(x), "] (",
    paste(comp_names, collapse = ", "), "): [",
    domain[1], ", ", domain[2], "] -> ", range_str, "\n"
  ))
  len <- length(x)
  if (len > 0) {
    format(x[seq_len(min(n, len))], ...) |>
      paste0("[", seq_len(min(n, len)), "]: ", x = _) |>
      cat(sep = "\n")
    cat("\n")
    if (n < len) {
      cat(paste0("    [....]   (", len - n, " not shown)\n"))
    }
  }
  invisible(x)
}

# dynamically exported in zzz.R (pillar glimpse), mirrors format_glimpse.tf
format_glimpse.tf_mv <- function(x, ...) {
  format.tf_mv(x, ...)
}

# Plotting (rudimentary) -------------------------------------------------------

#' Plot vector-valued functional data
#'
#' Two simple display modes for `tf_mv` objects: `"facet"` draws one panel per
#' output dimension (delegating to the univariate [plot.tfd][tf::plot.tf]);
#' `"trajectory"` (only for `d == 2`) draws the curves in the plane, i.e.
#' \eqn{y(t)} against \eqn{x(t)} -- the natural view for movement data.
#'
#' @param x a `tf_mv` object.
#' @param y ignored.
#' @param type `"facet"` (default) or `"trajectory"`.
#' @param ... passed to the underlying plotting calls.
#' @returns `x`, invisibly.
#' @family tf_mv-class
#' @export
plot.tf_mv <- function(x, y, ..., type = c("facet", "trajectory")) {
  type <- match.arg(type)
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")
  if (type == "trajectory") {
    if (length(comps) != 2) {
      cli::cli_abort("{.code type = \"trajectory\"} requires exactly 2 components.")
    }
    mx <- as.matrix(comps[[1]])
    my <- as.matrix(comps[[2]])
    plot(
      range(mx, na.rm = TRUE), range(my, na.rm = TRUE),
      type = "n", xlab = comp_names[1], ylab = comp_names[2], ...
    )
    for (i in seq_len(nrow(mx))) {
      graphics::lines(mx[i, ], my[i, ], ...)
    }
    return(invisible(x))
  }
  op <- graphics::par(mfrow = grDevices::n2mfrow(length(comps)))
  on.exit(graphics::par(op))
  iwalk(comps, \(comp, nm) plot(comp, main = nm, ...))
  invisible(x)
}

#' @rdname plot.tf_mv
#' @export
lines.tf_mv <- function(x, ..., type = c("facet", "trajectory")) {
  type <- match.arg(type)
  comps <- tf_components(x)
  if (type == "trajectory" && length(comps) == 2) {
    mx <- as.matrix(comps[[1]])
    my <- as.matrix(comps[[2]])
    for (i in seq_len(nrow(mx))) graphics::lines(mx[i, ], my[i, ], ...)
    return(invisible(x))
  }
  walk(comps, \(comp) graphics::lines(comp, ...))
  invisible(x)
}

# Conversion / interop ---------------------------------------------------------

#' @export
as.matrix.tf_mv <- function(x, arg, ...) {
  comps <- tf_components(x)
  has_arg <- !missing(arg)
  mats <- map(comps, \(comp) {
    if (has_arg) as.matrix(comp, arg = arg, ...) else as.matrix(comp, ...)
  })
  arr <- array(
    unlist(mats, use.names = FALSE),
    dim = c(nrow(mats[[1]]), ncol(mats[[1]]), length(comps)),
    dimnames = list(rownames(mats[[1]]), colnames(mats[[1]]), attr(x, "comp_names"))
  )
  arr
}

#' @export
as.data.frame.tf_mv <- function(x, row.names = NULL, optional = FALSE, unnest = FALSE, ...) {
  if (!unnest) {
    out <- vctrs::new_data_frame(list(x), n = vec_size(x))
    names(out) <- "data"
    return(out)
  }
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")
  # one long (id, arg, <comp_name>) per component, then full-outer-join on
  # (id, arg). For components that share arg structure this gives the same
  # rows as a side-by-side cbind would; for mixed regular/irregular or
  # otherwise-misaligned components NAs are filled where a component has no
  # observation at that (id, arg).
  per_comp <- map2(comps, comp_names, function(comp, nm) {
    df <- as.data.frame(comp, unnest = TRUE)
    names(df)[names(df) == "value"] <- nm
    df
  })
  out <- per_comp[[1]]
  for (k in seq_along(per_comp)[-1]) {
    out <- merge(out, per_comp[[k]], by = c("id", "arg"),
                 all = TRUE, sort = FALSE)
  }
  out[order(out$id, out$arg), , drop = FALSE]
}

# Re-representation, calculus, smoothing (component-wise) ----------------------

#' @export
tf_rebase.tf_mv <- function(object, basis_from, arg = NULL, ...) {
  cn <- attr(object, "comp_names")
  comps <- tf_components(object)
  if (is_tf_mv(basis_from)) {
    check_compatible_mv(object, basis_from)
    bases <- tf_components(basis_from)
    new_comps <- map2(comps, bases, function(o, b) {
      if (is.null(arg)) tf_rebase(o, b, ...) else tf_rebase(o, b, arg = arg, ...)
    })
  } else {
    new_comps <- map(comps, function(o) {
      if (is.null(arg)) {
        tf_rebase(o, basis_from, ...)
      } else {
        tf_rebase(o, basis_from, arg = arg, ...)
      }
    })
  }
  names(new_comps) <- cn
  new_tf_mv(new_comps)
}

#' @export
tf_derive.tf_mv <- function(f, arg, order = 1, ...) {
  has_arg <- !missing(arg)
  comps <- map(tf_components(f), function(comp) {
    if (has_arg) {
      tf_derive(comp, arg = arg, order = order, ...)
    } else {
      tf_derive(comp, order = order, ...)
    }
  })
  names(comps) <- attr(f, "comp_names")
  new_tf_mv(comps)
}

#' @export
tf_integrate.tf_mv <- function(f, arg, lower, upper, definite = TRUE, ...) {
  cn <- attr(f, "comp_names")
  has_arg <- !missing(arg)
  has_lower <- !missing(lower)
  has_upper <- !missing(upper)
  results <- map(tf_components(f), function(comp) {
    call_args <- list(comp, definite = definite, ...)
    if (has_arg) call_args$arg <- arg
    if (has_lower) call_args$lower <- lower
    if (has_upper) call_args$upper <- upper
    do.call(tf_integrate, call_args)
  })
  if (is.numeric(results[[1]])) {
    mat <- do.call(cbind, results)
    colnames(mat) <- cn
    return(mat)
  }
  names(results) <- cn
  new_tf_mv(results)
}

#' @export
tf_smooth.tf_mv <- function(x, ...) {
  comps <- map(tf_components(x), \(comp) tf_smooth(comp, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
tf_zoom.tf_mv <- function(f, begin = tf_domain(f)[1], end = tf_domain(f)[2], ...) {
  comps <- map(tf_components(f), \(comp) tf_zoom(comp, begin = begin, end = end, ...))
  names(comps) <- attr(f, "comp_names")
  new_tf_mv(comps)
}

# Geometric primitives for vector-valued curves --------------------------------

#' Pointwise norm and inner product for vector-valued functional data
#'
#' Small geometric helpers for `tf_mv` objects, defined by component-wise
#' composition of the existing univariate `Ops` / `Math` machinery:
#' - `tf_norm(f)`     -- pointwise Euclidean norm \eqn{\lVert f(t) \rVert};
#' - `tf_speed(f)`    -- pointwise speed \eqn{\lVert f'(t) \rVert};
#' - `tf_inner(f, g)` -- pointwise inner product \eqn{\langle f(t), g(t) \rangle};
#' - `tf_distance(f, g)` -- pointwise Euclidean distance \eqn{\lVert f(t) - g(t) \rVert};
#' - `tf_tangent(f)`  -- unit tangent \eqn{f'(t) / \lVert f'(t) \rVert}, returned
#'   as a `tf_mv` (undefined where the speed is zero -- callers get `NaN`s there);
#' - `tf_reparam_arclength(f)` -- re-parametrize the curve at constant speed
#'   (i.e. by its normalized cumulative arc length).
#'
#' @param f,g `tf_mv` objects (with identical `d` and component names where
#'   two arguments are required).
#' @returns a univariate `tfd` for `tf_norm`/`tf_speed`/`tf_inner`/`tf_distance`,
#'   a `tf_mv` for `tf_tangent`/`tf_reparam_arclength`.
#' @family tf_mv-class
#' @examples
#' set.seed(1)
#' f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
#' tf_norm(f)
#' tf_speed(f)
#' tf_distance(f, tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2))))
#' @rdname tf_geom
#' @export
tf_norm <- function(f) {
  sqrt(Reduce(`+`, map(tf_components(f), \(c) c^2)))
}

#' @rdname tf_geom
#' @export
tf_speed <- function(f) tf_norm(tf_derive(f))

#' @rdname tf_geom
#' @export
tf_inner <- function(f, g) {
  check_compatible_mv(f, g)
  Reduce(`+`, map2(tf_components(f), tf_components(g), \(a, b) a * b))
}

#' @rdname tf_geom
#' @export
tf_distance <- function(f, g) tf_norm(f - g)

#' @rdname tf_geom
#' @export
tf_tangent <- function(f) {
  df <- tf_derive(f)
  inv_speed <- 1 / tf_norm(df)
  comps <- map(tf_components(df), \(c) c * inv_speed)
  names(comps) <- attr(f, "comp_names")
  new_tf_mv(comps)
}

#' @rdname tf_geom
#' @export
tf_reparam_arclength <- function(f) {
  s <- tf_arclength(f, definite = FALSE) # cumulative s(t), one per curve
  L <- tf_arclength(f)                   # total length per curve
  u <- s / L                             # u(t) = s(t)/L : domain -> [0, 1]
  # `tf_warp(f, w)` computes `f o w^{-1}`, so passing `u` (not its inverse)
  # gives the desired arc-length-parameterised curve `f o u^{-1}`.
  tf_warp(f, u)
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
  f, arg = NULL,
  lower = tf_domain(f)[1], upper = tf_domain(f)[2],
  definite = TRUE,
  method = c("polyline", "derive"),
  ...
) {
  method <- match.arg(method)
  if (method == "derive") {
    speed <- tf_speed(f)
    call_args <- list(speed, lower = lower, upper = upper,
                      definite = definite, ...)
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
  comps <- tf_components(f)
  if (!n) {
    return(if (definite) numeric(0) else tfd(numeric(0)))
  }
  # per-curve evaluation grids
  grids <- if (!is.null(arg)) {
    rep(list(sort(unique(arg))), n)
  } else {
    a <- tf_arg(f)
    if (is.numeric(a)) {
      rep(list(a), n)
    } else if (all(map_lgl(a, is.numeric))) {
      a # case 1: per-curve, shared across components
    } else {
      # case 2/3: per-component args -> union per curve
      lapply(seq_len(n), function(i) {
        sort(unique(unlist(lapply(comps, function(comp) {
          ai <- tf_arg(comp); if (is.list(ai)) ai[[i]] else ai
        }))))
      })
    }
  }
  # clamp to [lower, upper] and guarantee endpoints (for accurate sub-interval
  # lengths even when the limits don't fall on sample points)
  grids <- lapply(grids, function(g) {
    g <- g[g >= lower & g <= upper]
    sort(unique(c(lower, g, upper)))
  })
  # evaluate each component on each curve's grid (tf_evaluate.tfd accepts a
  # per-curve arg list)
  comp_evals <- map(comps, function(comp) tf_evaluate(comp, arg = grids))
  per_curve_segs <- map(seq_len(n), function(i) {
    mat <- do.call(cbind, lapply(comp_evals, \(ev) ev[[i]]))
    if (nrow(mat) < 2L) return(numeric(0))
    sqrt(rowSums(diff(mat)^2))
  })
  if (definite) {
    setNames(map_dbl(per_curve_segs, sum), names(f))
  } else {
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

# Registration: one shared time-warp per curve, applied to all components ------

# univariate signal used to estimate the (joint) warp for a multivariate curve
mv_registration_signal <- function(x, ref_component = 1L) {
  if (is.function(ref_component)) {
    return(ref_component(x))
  }
  if (identical(ref_component, "norm")) {
    return(tf_norm(x))
  }
  tf_component(x, ref_component)
}

#' @export
tf_warp.tf_mv <- function(x, warp, ...) {
  comps <- map(tf_components(x), \(comp) tf_warp(comp, warp, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
tf_align.tf_mv <- function(x, warp, ...) {
  comps <- map(tf_components(x), \(comp) tf_align(comp, warp, ...))
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @export
tf_estimate_warps.tf_mv <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "cc", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2,
  ref_component = 1L
) {
  method <- match.arg(method)
  signal <- mv_registration_signal(x, ref_component)
  tmpl <- if (is_tf_mv(template)) {
    mv_registration_signal(template, ref_component)
  } else {
    template
  }
  warps <- tf_estimate_warps(
    signal,
    ...,
    template = tmpl,
    method = method,
    max_iter = max_iter,
    tol = tol
  )
  # drop the (univariate) template attribute so tf_register() derives a
  # multivariate template via mean() of the aligned components instead.
  attr(warps, "template") <- NULL
  warps
}
