#' Preprocess evaluation grid for plotting
#'
#' (internal function exported for re-use in upstream packages)
#' @param f a `tf`-object
#' @param n_grid length of evaluation grid
#' @returns a semi-regular grid rounded down to appropriate resolution
#' @export
#' @family tidyfun developer tools
prep_plotting_arg <- function(f, n_grid) {
  if (!isTRUE(n_grid > 1)) {
    tf_arg(f)
  } else {
    resolution <- get_resolution(tf_arg(f))
    seq(tf_domain(f)[1], tf_domain(f)[2], length.out = n_grid) |>
      round_resolution(resolution) |>
      setdiff(
        round_resolution(unlist(tf_arg(f)), resolution)
      ) |>
      union(unlist(tf_arg(f))) |>
      sort()
  }
}

#' `base` plots for `tf`s
#'
#' Some `base` functions for displaying functional data in
#' spaghetti- (i.e., line plots) and lasagna- (i.e., heat map) flavors.
#'
#' If no second argument `y` is given, evaluation points (`arg`) for the functions
#' are given by the union of the `tf`'s `arg` and an equidistant grid
#' over its domain with `n_grid` points. If you want to only see the original
#' data for `tfd`-objects without inter-/extrapolation, use `n_grid < 1` or
#' `n_grid = NA`.
#'
#' @param x an `tf` object
#' @param y (optional) numeric vector to be used as `arg`
#'   (i.e., for the **x**-axis...!)
#' @param n_grid minimal size of equidistant grid used for plotting,
#'   defaults to 50. See details.
#' @param points should the original evaluation points be marked by points?
#'   Defaults to `TRUE` for irregular `tfd` and FALSE for all others
#' @param type "spaghetti": line plots, "lasagna": heat maps.
#' @param alpha alpha-value (see[grDevices::rgb()]) for noodle transparency.
#'   Defaults to 2/(no. of observations). Lower is more transparent.
#' @param ... additional arguments for [matplot()] ("spaghetti") or
#'   [image()] ("lasagna")
#' @returns the plotted `tf`-object, invisibly.
#' @importFrom utils modifyList
#' @importFrom graphics matplot image axis
#' @importFrom grDevices heat.colors rgb
#' @rdname tfviz
#' @family tidyfun visualization
#' @export
#' @references `r format_bib("swihart2010lasagna")`
plot.tf <- function(x, y, n_grid = 50, points = is_irreg(x),
                    type = c("spaghetti", "lasagna"),
                    alpha = min(1, max(0.05, 2 / length(x))), ...) {
  type <- match.arg(type)
  assert_logical(points)
  assert_number(n_grid, na.ok = TRUE)
  if (missing(y)) {
    arg <- prep_plotting_arg(x, n_grid)
    # irreg args need to be turned to a vector for as.matrix below:
    if (is.list(arg)) arg <- sort(unique(unlist(arg)))
  } else {
    arg <- y
  }
  m <- if (is_tfd(x)) {
    as.matrix(x, arg = arg, interpolate = TRUE)
  } else {
    as.matrix(x, arg = arg)
  }
  if (type == "spaghetti") {
    args <- modifyList(
      list(
        x = drop(attr(m, "arg")), y = t(m), type = "l",
        ylab = deparse(substitute(x)), xlab = "", lty = 1,
        col = rgb(0, 0, 0, alpha)
      ),
      list(...)
    )
    do.call(matplot, args)
    if (points) {
      pointsargs <- modifyList(
        list(
          x = x,
          n_grid = NA, points = TRUE, interpolate = FALSE,
          pch = 19, ol = rgb(0, 0, 0, alpha)
        ),
        list(...)
      )
      do.call(linespoints_tf, pointsargs)
    }
  }
  if (type == "lasagna") {
    args <- modifyList(
      list(
        x = drop(attr(m, "arg")), y = seq_len(nrow(m)),
        z = t(m), col = heat.colors(25),
        ylab = "id", xlab = "", yaxt = "n"
      ),
      list(...)
    )
    m <- m[rev(seq_len(nrow(m))), ] # so first obs is on top
    do.call(image, args)
    axis(2, at = seq_len(nrow(m)), labels = rev(names(x) %||% seq_len(nrow(m))))
  }
  invisible(x)
}

#' @importFrom graphics matlines
linespoints_tf <- function(x, arg, n_grid = 50, points = TRUE,
                           alpha = min(1, max(0.05, 2 / length(x))),
                           interpolate = TRUE, ...) {
  assert_number(n_grid, na.ok = TRUE)
  if (missing(arg)) {
    arg <- prep_plotting_arg(x, n_grid)
    # irreg args need to be turned to a vector for as.matrix below:
    if (is.list(arg)) arg <- sort(unique(unlist(arg)))
  }
  m <- if (is_tfd(x)) {
    suppressWarnings(
      as.matrix(x, arg = arg, interpolate = interpolate)
    )
  } else {
    as.matrix(x, arg = arg)
  }
  args <- modifyList(
    list(
      x = drop(attr(m, "arg")), y = t(m), type = ifelse(points, "p", "l"),
      lty = 1, col = rgb(0, 0, 0, alpha), pch = 19
    ),
    list(...)
  )
  do.call(matlines, args)
}

#' @export
#' @rdname tfviz
#' @family tidyfun visualization
lines.tf <- function(x, arg, n_grid = 50,
                     alpha = min(1, max(0.05, 2 / length(x))), ...) {
  args <- c(modifyList(
    head(formals(lines.tf), -1),
    as.list(match.call())[-1]
  ), points = FALSE)
  # eval here so pipe finds it later
  args$x <- x
  do.call(linespoints_tf, args)
  invisible(x)
}
#' @export
#' @rdname tfviz
#' @family tidyfun visualization
#' @param arg evaluation grid (vector)
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for arg for which no original data is available? Only relevant for
#'   tfd, defaults to FALSE
points.tf <- function(x, arg, n_grid = NA,
                      alpha = min(1, max(0.05, 2 / length(x))),
                      interpolate = FALSE, ...) {
  args <- c(modifyList(
    head(formals(points.tf), -1),
    as.list(match.call())[-1]
  ), points = TRUE)
  # eval here so pipe finds it later
  args$x <- x
  do.call(linespoints_tf, args)
  invisible(x)
}
