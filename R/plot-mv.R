# Plotting (rudimentary) -------------------------------------------------------

# graphical parameters that should be recycled *per curve* in trajectory plots
traj_curve_par <- c("col", "lty", "lwd", "pch", "cex", "lend", "ljoin")

# Extract paired (x(t), y(t)) matrices from a 2-component tf_mv for matlines.
# Delegates to the *exported* `as.matrix.tf_mv`, which already does the
# union-grid + NA-fill coercion. Kept as a 2-line internal so the plot
# methods read clearly; downstream consumers wanting the same semantics
# should use `as.matrix.tf_mv()` or `as.data.frame.tf_mv(long = TRUE)`
# directly rather than reach into this private helper.
mv_paired_xy <- function(x) {
  arr <- as.matrix(x, interpolate = TRUE)  # [n_curve, n_arg, 2]
  list(x = matrix(arr[, , 1L], nrow = dim(arr)[1L], ncol = dim(arr)[2L],
                  dimnames = dimnames(arr)[1:2]),
       y = matrix(arr[, , 2L], nrow = dim(arr)[1L], ncol = dim(arr)[2L],
                  dimnames = dimnames(arr)[1:2]))
}

# Draw each curve (row of mx/my) as a column of a matrix so that matlines()
# recycles col/lty/lwd/... across curves -- matching univariate plot.tf().
# A single lines() call per curve would only honour the first element of e.g.
# `col`, so passing `col = 1:n` would draw every curve in the same colour.
# `alpha` (if given) tints the line colour(s) -- consistent with plot.tf().
draw_trajectory <- function(mx, my, dots) {
  line_args <- modifyList(
    list(col = 1, lty = 1),
    dots[intersect(names(dots), traj_curve_par)]
  )
  if (!is.null(dots$alpha)) {
    line_args$col <- grDevices::adjustcolor(line_args$col, alpha.f = dots$alpha)
  }
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
    xy <- mv_paired_xy(x)
    mx <- xy$x
    my <- xy$y
    dots <- list(...)
    # set up the plotting region without per-curve params, then draw the curves
    setup_dots <- dots[setdiff(names(dots), c(traj_curve_par, "alpha"))]
    plot_args <- modifyList(
      list(xlab = comp_names[1], ylab = comp_names[2]),
      setup_dots
    )
    do.call(
      plot,
      c(
        list(
          range(mx, na.rm = TRUE),
          range(my, na.rm = TRUE),
          type = "n"
        ),
        plot_args
      )
    )
    draw_trajectory(mx, my, dots)
    return(invisible(x))
  }
  # Prefer a single row for up to 3 components (wider figures are typical);
  # fall back to n2mfrow's roughly-square layout for larger d.
  mfrow_layout <- if (length(comps) <= 3L) c(1L, length(comps)) else
    grDevices::n2mfrow(length(comps))
  op <- graphics::par(mfrow = mfrow_layout)
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
    xy <- mv_paired_xy(x)
    draw_trajectory(xy$x, xy$y, list(...))
    return(invisible(x))
  }
  walk(comps, \(comp) graphics::lines(comp, ...))
  invisible(x)
}
