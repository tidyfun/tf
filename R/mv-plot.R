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
