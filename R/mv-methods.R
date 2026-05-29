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

map_components <- function(x, .f, ...) {
  comps <- map(tf_components(x), .f, ...)
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

map2_components <- function(x, y, .f, ...) {
  check_compatible_mv(x, y)
  comps <- map2(tf_components(x), tf_components(y), .f, ...)
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps)
}

#' @rdname tf_mv_methods
#' @export
tf_component <- function(f, which) {
  comps <- tf_components(f)
  if (is.character(which)) {
    loc <- match(which, names(comps))
    if (anyNA(loc)) {
      cli::cli_abort("Unknown component {.val {which}}.")
    }
    which <- loc
  }
  comps[[which]]
}

#' @rdname tf_mv_methods
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

tf_mv_curve_grids <- function(x) {
  n <- vec_size(x)
  arg_vals <- tf_arg(x)
  if (is.numeric(arg_vals)) {
    rep(list(arg_vals), n)
  } else if (
    all(map_lgl(arg_vals, is.numeric)) &&
      !identical(names(arg_vals), attr(x, "comp_names"))
  ) {
    arg_vals
  } else {
    comps <- tf_components(x)
    lapply(seq_len(n), function(i) {
      sort(unique(unlist(
        lapply(comps, function(comp) {
          comp_arg <- tf_arg(comp)
          if (is.list(comp_arg)) comp_arg[[i]] else comp_arg
        }),
        use.names = FALSE
      )))
    })
  }
}

#' @export
tf_evaluations.tf_mv <- function(f) {
  comp_evals <- map(tf_components(f), tf_evaluations)
  assemble_mv_evals(comp_evals, attr(f, "comp_names"), vec_size(f))
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
  if (length(comps) && all(map_lgl(comps, is_tfb))) {
    cli::cli_abort(
      "{.fn tf_count} is not defined for basis-represented ({.cls tfb_mv}) data."
    )
  }
  counts <- map(comps, tf_count)
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
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps, domain = tf_domain(x))
}

mv_missing <- function(...) {
  mv_args <- list(...)
  if (!length(mv_args)) {
    return(logical(0))
  }
  Reduce(`|`, map(mv_args, is.na))
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
`[.tf_mv` <- function(
  x,
  i,
  j,
  interpolate = TRUE,
  matrix = TRUE,
  component = NULL
) {
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
    j <- tf_mv_curve_grids(xi)
  }

  comps_i <- tf_components(xi)
  n_i <- vec_size(xi)
  if (matrix) {
    if (missing(j)) j <- sort_unique(tf_mv_curve_grids(xi), simplify = TRUE)
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
    # a scalar (typically NA) is broadcast to every component
    rep(list(value), length(comps))
  }
  new_comps <- map2(comps, value_comps, function(comp, v) {
    comp[i] <- v
    comp
  })
  names(new_comps) <- attr(x, "comp_names")
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
  names(comps) <- attr(x, "comp_names")
  new_tf_mv(comps, domain = tf_domain(x))
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
  map2_components(x, y, \(a, b) vec_arith(op, a, b))
}

#' @export
#' @method vec_arith.tf_mv numeric
vec_arith.tf_mv.numeric <- function(op, x, y, ...) {
  map_components(x, \(a) vec_arith(op, a, y))
}

#' @export
#' @method vec_arith.numeric tf_mv
vec_arith.numeric.tf_mv <- function(op, x, y, ...) {
  map_components(y, \(b) vec_arith(op, x, b))
}

#' @export
#' @method vec_arith.tf_mv MISSING
vec_arith.tf_mv.MISSING <- function(op, x, y, ...) {
  map_components(x, \(a) vec_arith(op, a, MISSING()))
}

#' @export
Math.tf_mv <- function(x, ...) {
  generic <- .Generic
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

#' @export
median.tf_mv <- function(x, na.rm = FALSE, ...) {
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, \(a) median(a, na.rm = na.rm, ...))
}

#' @export
sd.tf_mv <- function(x, na.rm = FALSE) {
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, \(a) sd(a, na.rm = na.rm))
}

#' @export
var.tf_mv <- function(x, y = NULL, na.rm = FALSE, use) {
  has_use <- !missing(use)
  if (!is.null(y) && is_tf_mv(y)) {
    check_compatible_mv(x, y)
    missing <- mv_missing(x, y)
    x <- mv_complete(x, missing = missing, na.rm = na.rm)
    y <- mv_complete(y, missing = missing, na.rm = na.rm)
    return(map2_components(x, y, function(a, b) {
      if (has_use) {
        var(a, y = b, na.rm = na.rm, use = use)
      } else {
        var(a, y = b, na.rm = na.rm)
      }
    }))
  }
  x <- mv_complete(x, na.rm = na.rm)
  map_components(x, function(a) {
    if (has_use) {
      var(a, y = y, na.rm = na.rm, use = use)
    } else {
      var(a, y = y, na.rm = na.rm)
    }
  })
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

# one-line description of a single component's representation, mirroring the
# "evaluations / interpolation / basis" lines of print.tfd / print.tfb.
mv_component_info <- function(comp) {
  if (is_tfb(comp)) {
    return(paste0(
      "in basis representation: ",
      trimws(paste(attr(comp, "basis_label"), attr(comp, "family_label")))
    ))
  }
  evaluator <- paste0("interpolation by ", attr(comp, "evaluator_name"))
  if (is_irreg(comp)) {
    n_evals <- tf_count(comp[!is.na(comp)])
    grid <- if (length(n_evals)) {
      paste0(
        "based on ",
        min(n_evals),
        " to ",
        max(n_evals),
        " evaluations each"
      )
    } else {
      "irregular"
    }
    return(paste0(grid, ", ", evaluator))
  }
  paste0("based on ", length(tf_arg(comp)), " evaluations each, ", evaluator)
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
    class(x)[1],
    "<d=",
    d,
    ">[",
    length(x),
    "] (",
    paste(comp_names, collapse = ", "),
    "): [",
    domain[1],
    ", ",
    domain[2],
    "] -> ",
    range_str,
    "\n"
  ))
  if (d > 0L) {
    info <- map_chr(tf_components(x), mv_component_info)
    if (length(unique(info)) == 1L) {
      # all components share the same grid / interpolator / basis
      cat(paste0("components ", info[1], "\n"))
    } else {
      for (k in seq_along(info)) {
        cat(paste0("  ", comp_names[k], ": ", info[k], "\n"))
      }
    }
  }
  len <- length(x)
  if (len > 0) {
    n_show <- min(n, len)
    formatted <- format(x[seq_len(n_show)], ...)
    paste0("[", seq_len(n_show), "]: ", formatted) |>
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

# graphical parameters that should be recycled *per curve* in trajectory plots
traj_curve_par <- c("col", "lty", "lwd", "pch", "cex", "lend", "ljoin")

# Evaluate the two components of a 2-d tf_mv on a *common* argument grid so the
# trajectory y(t)-vs-x(t) can be drawn as paired points. The components may be
# observed on different (or per-curve irregular) grids, so we evaluate both on
# the union of all their argument values (interpolating, NA outside each
# component's observed range).
trajectory_xy <- function(comps) {
  grid <- sort(unique(unlist(
    lapply(comps, \(comp) as.numeric(unlist(tf_arg(comp), use.names = FALSE))),
    use.names = FALSE
  )))
  list(
    x = as.matrix(comps[[1]], arg = grid, interpolate = TRUE),
    y = as.matrix(comps[[2]], arg = grid, interpolate = TRUE)
  )
}

# Draw each curve (row of mx/my) as a column of a matrix so that matlines()
# recycles col/lty/lwd/... across curves -- matching univariate plot.tf().
# A single lines() call per curve would only honour the first element of e.g.
# `col`, so passing `col = 1:n` would draw every curve in the same colour.
draw_trajectory <- function(mx, my, dots) {
  line_args <- modifyList(
    list(col = 1, lty = 1),
    dots[intersect(names(dots), traj_curve_par)]
  )
  do.call(graphics::matlines, c(list(t(mx), t(my)), line_args))
}

# default display: "trajectory" for 2-d curves (the movement-data view),
# "facet" otherwise.
mv_plot_type <- function(type, comps) {
  type <- type %||% if (length(comps) == 2L) "trajectory" else "facet"
  match.arg(type, c("facet", "trajectory"))
}

#' Plot vector-valued functional data
#'
#' Two simple display modes for `tf_mv` objects: `"facet"` draws one panel per
#' output dimension (delegating to the univariate [plot.tf()]);
#' `"trajectory"` (only for `d == 2`) draws the curves in the plane, i.e.
#' \eqn{y(t)} against \eqn{x(t)} -- the natural view for movement data.
#'
#' @details
#' In `"trajectory"` mode the two components must be paired at common argument
#' values to form \eqn{(x(t), y(t))} points. When the components are sampled on
#' different (or per-curve irregular) grids they are therefore evaluated on the
#' union of their argument grids with `interpolate = TRUE` (values outside a
#' component's observed range become `NA` and are skipped). For components that
#' already share a grid this is a no-op.
#'
#' @param x a `tf_mv` object.
#' @param y ignored.
#' @param type `"trajectory"` or `"facet"`. Defaults to `"trajectory"` for
#'   two-component (`d == 2`) objects and to `"facet"` otherwise.
#' @param ... passed to the underlying plotting calls. Per-curve graphical
#'   parameters (`col`, `lty`, `lwd`, ...) are recycled across curves.
#' @returns `x`, invisibly.
#' @family tf_mv-class
#' @export
plot.tf_mv <- function(x, y, ..., type = NULL) {
  comps <- tf_components(x)
  type <- mv_plot_type(type, comps)
  comp_names <- attr(x, "comp_names")
  if (type == "trajectory") {
    if (length(comps) != 2) {
      cli::cli_abort(
        "{.code type = \"trajectory\"} requires exactly 2 components."
      )
    }
    xy <- trajectory_xy(comps)
    mx <- xy$x
    my <- xy$y
    dots <- list(...)
    # set up the plotting region without per-curve params, then draw the curves
    setup_dots <- dots[setdiff(names(dots), traj_curve_par)]
    do.call(
      plot,
      c(
        list(
          range(mx, na.rm = TRUE),
          range(my, na.rm = TRUE),
          type = "n",
          xlab = comp_names[1],
          ylab = comp_names[2]
        ),
        setup_dots
      )
    )
    draw_trajectory(mx, my, dots)
    return(invisible(x))
  }
  op <- graphics::par(mfrow = grDevices::n2mfrow(length(comps)))
  on.exit(graphics::par(op))
  iwalk(comps, \(comp, nm) plot(comp, main = nm, ...))
  invisible(x)
}

#' @rdname plot.tf_mv
#' @importFrom graphics par lines matlines
#' @importFrom grDevices n2mfrow
#' @export
lines.tf_mv <- function(x, ..., type = NULL) {
  comps <- tf_components(x)
  type <- mv_plot_type(type, comps)
  if (type == "trajectory" && length(comps) == 2) {
    xy <- trajectory_xy(comps)
    draw_trajectory(xy$x, xy$y, list(...))
    return(invisible(x))
  }
  walk(comps, \(comp) graphics::lines(comp, ...))
  invisible(x)
}

# Conversion / interop ---------------------------------------------------------

#' @export
as.matrix.tf_mv <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg)) {
    x[,, interpolate = interpolate, matrix = TRUE]
  } else {
    x[, arg, interpolate = interpolate, matrix = TRUE]
  }
}

#' @export
as.data.frame.tf_mv <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  unnest = FALSE,
  ...
) {
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
    out <- merge(
      out,
      per_comp[[k]],
      by = c("id", "arg"),
      all = TRUE,
      sort = FALSE
    )
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
      if (is.null(arg)) tf_rebase(o, b, ...) else
        tf_rebase(o, b, arg = arg, ...)
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
  map_components(f, function(comp) {
    if (has_arg) {
      tf_derive(comp, arg = arg, order = order, ...)
    } else {
      tf_derive(comp, order = order, ...)
    }
  })
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
  map_components(x, \(comp) tf_smooth(comp, ...))
}

#' @export
tf_zoom.tf_mv <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  map_components(f, \(comp) tf_zoom(comp, begin = begin, end = end, ...))
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
#' @name tf_geom
#' @rdname tf_geom
#' @export
tf_norm <- function(f) {
  comps <- tf_components(f)
  if (!length(comps)) return(tfd(numeric(0)))
  sqrt(Reduce(`+`, map(comps, \(comp) comp^2)))
}

#' @rdname tf_geom
#' @export
tf_speed <- function(f) tf_norm(tf_derive(f))

#' @rdname tf_geom
#' @export
tf_inner <- function(f, g) {
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
tf_tangent <- function(f) {
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
  # clamp to [lower, upper] and guarantee endpoints (for accurate sub-interval
  # lengths even when the limits don't fall on sample points)
  grids <- lapply(grids, function(g) {
    g <- g[g >= lower & g <= upper]
    sort(unique(c(lower, g, upper)))
  })
  paired_evals <- tf_evaluate(f, arg = grids)
  incomplete <- map_lgl(paired_evals, \(mat) is.matrix(mat) && anyNA(mat))
  if (any(incomplete)) {
    cli::cli_abort(c(
      "Cannot compute polyline arc length with missing paired component evaluations.",
      "i" = "Affected curve index{?es}: {.val {which(incomplete)}}.",
      "i" = "Set {.arg lower}/{.arg upper} to a common observed interval or use an evaluator that supplies all requested component values."
    ))
  }
  per_curve_segs <- map(seq_len(n), function(i) {
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
  map_components(x, \(comp) tf_warp(comp, warp, ...))
}

#' @export
tf_align.tf_mv <- function(x, warp, ...) {
  map_components(x, \(comp) tf_align(comp, warp, ...))
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
