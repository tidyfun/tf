#' Basic visualization functions for `tf`s
#'
#' Some `base` functions for displaying functional data in
#' spaghetti- (i.e., line plots) and lasagna- (i.e., heat map) flavors.
#'
#' If no `arg` are provided, evaluation points (`arg`) for the functions
#' are given by the union of the `tf`'s `arg` and an equidistant grid
#' over its domain with `n_grid` points. If you want to only see the original
#' data for `tfd`-objects without inter-/extrapolation, use `n_grid < 1` or
#' `n_grid = NA`.
#'
#' @param f an `tf` object
#' @param arg (optional) numeric vector to be used as `arg`. See details.
#' @return for `funplot`: the `ggplot`-object for further modification. For the
#'  others: the plotted `tf`-object, invisibly.
#' @import ggplot2
#' @importFrom modelr seq_range
#' @export
#' @rdname tfviz
funplot <- function(f, arg, n_grid = 50, points = is_irreg(f),
                    type = c("spaghetti", "lasagna"), 
                    alpha = min(1, max(.05, 2 / length(f)))) {
  assert_class(f, "tf")
  assert_number(n_grid, na.ok = TRUE)
  assert_flag(points)
  type <- match.arg(type)
  if (missing(arg)) {
    arg <- prep_plotting_arg(f, n_grid)
  }
  d <- if (is_tfd(f)) {
    tf_unnest(f, arg = arg, interpolate = TRUE)
  } else {
    tf_unnest(f, arg = arg)
  }
  
  if (type == "spaghetti") {
    p <- ggplot(d, aes(x = arg, y = value, group = id)) +
      geom_line(alpha = alpha)
    if (points) {
      p <- p +
        geom_point(data = tf_unnest(f, arg = tf_arg(f)), alpha = alpha)
    }
  }
  if (type == "lasagna") {
    d <- d %>%
      mutate(id_num = as.numeric(as.factor(id))) %>%
      group_by(id) %>%
      mutate(xmax = c(arg[-1], max(arg) + mean(diff(arg))))
    p <- ggplot(d, aes(
      y = id_num, xmin = arg, xmax = xmax,
      ymin = id_num - .5, ymax = id_num + .5, fill = value
    )) +
      geom_rect(colour = NA) +
      scale_fill_gradient2(deparse(substitute(f)),
                           midpoint = median(d$value, na.rm = TRUE)
      ) +
      scale_y_continuous(name = "id", breaks = seq_along(f), labels = names(f))
  }
  p
}
