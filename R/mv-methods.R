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
#' @param f,x a `tf_mv` object.
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
  args <- map(tf_components(f), tf_arg)
  if (
    length(args) > 1 &&
      all(map_lgl(args[-1], \(a) isTRUE(all.equal(a, args[[1]]))))
  ) {
    return(args[[1]])
  }
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
  cat(paste0(
    class(x)[1], "<d=", d, ">[", length(x), "] (",
    paste(comp_names, collapse = ", "), "): [",
    domain[1], ", ", domain[2], "] -> R^", d, "\n"
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
  base <- as.data.frame(comps[[1]], unnest = TRUE)
  names(base)[names(base) == "value"] <- comp_names[1]
  if (length(comps) > 1) {
    for (k in 2:length(comps)) {
      vals <- as.data.frame(comps[[k]], unnest = TRUE)$value
      base[[comp_names[k]]] <- vals
    }
  }
  base
}

# Re-representation, calculus, smoothing (component-wise) ----------------------

#' @rdname tf_mv-methods
#' @details
#' Most univariate `tf` verbs also work on `tf_mv` objects by acting on each
#' component: `tf_rebase()` (and hence `tfd_mv`/`tfb_mv` conversion),
#' `tf_derive()`, `tf_integrate()` (definite integrals return an `n x d`
#' matrix), `tf_smooth()` and `tf_zoom()`. Registration
#' ([tf_register()]/[tf_estimate_warps()]/[tf_warp()]/[tf_align()]) estimates a
#' *single, shared* time-warp per curve (by default from the pointwise
#' Euclidean norm across components, or from a chosen `ref_component`) and
#' applies it jointly to all components, so the dimensions stay synchronized.
#' The registration signal is, by default, the first component; use
#' `ref_component` to pick another component (by name/index), `"norm"` for the
#' pointwise Euclidean norm across components, or a function mapping the
#' `tf_mv` to a univariate `tf` vector.
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

# Registration: one shared time-warp per curve, applied to all components ------

# univariate signal used to estimate the (joint) warp for a multivariate curve
mv_registration_signal <- function(x, ref_component = 1L) {
  if (is.function(ref_component)) {
    return(ref_component(x))
  }
  if (identical(ref_component, "norm")) {
    return(sqrt(Reduce(`+`, map(tf_components(x), \(comp) comp^2))))
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
