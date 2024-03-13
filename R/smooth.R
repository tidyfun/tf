#' @title Simple smoothing of `tf` objects
#'
#' @description Apply running means or medians, `lowess` or Savitzky-Golay
#'   filtering to smooth functional data. This does nothing for `tfb`-objects,
#'   which should be smoothed by using a smaller basis / stronger penalty.
#'
#' @details `tf_smooth.tfd` overrides/automatically sets some defaults of the
#'   used methods:
#'
#'   - **`lowess`** uses a span parameter of `f` = 0.15 (instead of 0.75)
#'   by default.
#'   - **`rollmean`/`median`** use a window size of `k` = $<$number of
#'   grid points$>$/20 (i.e., the nearest odd integer to that) and sets `fill=
#'   "extend"` (i.e., constant extrapolation to replace missing values at the
#'   extremes of the domain) by default. Use `fill= NA` for `zoo`'s default
#'   behavior of shortening the smoothed series.
#'   - **`savgol`** uses a window size of `k` = $<$number of
#'   grid points$>$/10 (i.e., the nearest odd integer to that).
#'
#' @param x a `tf` object containing functional data
#' @param method one of "lowess" (see [stats::lowess()]), "rollmean",
#'   "rollmedian" (see [zoo::rollmean()]) or "savgol" (see [pracma::savgol()])
#' @param verbose give lots of diagnostic messages? Defaults to TRUE
#' @param ... arguments for the respective `method`. See Details.
#' @returns a smoothed version of the input. For some methods/options, the
#'   smoothed functions may be shorter than the original ones (at both ends).
#' @export
#' @family tidyfun nonparametric smoothers
tf_smooth <- function(x, ...) {
  UseMethod("tf_smooth")
}

#' @export
#' @rdname tf_smooth
tf_smooth.tfb <- function(x, verbose = TRUE, ...) {
  if (verbose) warning(
    "you called tf_smooth on a tfb object, not on a tfd object -- ",
    "just use a smaller basis or stronger penalization.\n",
    "Returning unchanged tfb object.",
    call. = FALSE
  )
  x
}

#' @importFrom stats lowess
#' @importFrom zoo rollmean rollmedian
#' @importFrom pracma savgol
#' @importFrom stats lowess
#' @rdname tf_smooth
#' @export
#' @examples
#' library(zoo)
#' library(pracma)
#' f <- tf_sparsify(tf_jiggle(tf_rgp(4, 201, nugget = 0.05)))
#' f_lowess <- tf_smooth(f, "lowess")
#' # these methods ignore the distances between arg-values:
#' f_mean <- tf_smooth(f, "rollmean")
#' f_median <- tf_smooth(f, "rollmean", k = 31)
#' f_sg <- tf_smooth(f, "savgol", fl = 31)
#' layout(t(1:4))
#' plot(f, points = FALSE, main = "original")
#' plot(f_lowess,
#'   points = FALSE, col = "blue", main = "lowess (default,\n span 0.9 in red)"
#' )
#' lines(tf_smooth(f, "lowess", f = 0.9), col = "red", alpha = 0.2)
#' plot(f_mean,
#'   points = FALSE, col = "blue", main = "rolling means &\n medians (red)"
#' )
#' lines(f_median, col = "red", alpha = 0.2) # note constant extrapolation at both ends!
#' plot(f, points = FALSE, main = "orginal and\n savgol (red)")
#' lines(f_sg, col = "red")
tf_smooth.tfd <- function(x,
                          method = c("lowess", "rollmean", "rollmedian", "savgol"),
                          verbose = TRUE, ...) {
  method <- match.arg(method)
  smoother <- get(method, mode = "function")
  dots <- list(...)
  # nocov start
  if (method %in% c("savgol", "rollmean", "rollmedian")) {
    if (verbose & !is_equidist(x)) {
      warning(
        "non-equidistant arg-values in ", sQuote(deparse(substitute(x))),
        " ignored by ", method, ".",
        call. = FALSE
      )
    }
    if (grepl("rollm", method, fixed = TRUE)) {
      if (is.null(dots$k)) {
        dots$k <- ceiling(0.05 * min(tf_count(x)))
        dots$k <- dots$k + !(dots$k %% 2) # make uneven
        if (verbose) message("using k = ", dots$k, " observations for rolling data window.")
      }
      if (is.null(dots$fill)) {
        if (verbose) message("setting fill = 'extend' for start/end values.")
        dots$fill <- "extend"
      }
    }
    if (method == "savgol") {
      if (is.null(dots$fl)) {
        dots$fl <- ceiling(0.15 * min(tf_count(x)))
        dots$fl <- dots$fl + !(dots$fl %% 2) # make uneven
        if (verbose) message("using fl = ", dots$fl, " observations for rolling data window.")
      }
    }
    smoothed <- map(
      tf_evaluations(x), \(x) do.call(smoother, append(list(x), dots))
    )
  }
  # nocov end
  if (method == "lowess") {
    if (is.null(dots$f)) {
      dots$f <- 0.15
      if (verbose) message("using f = ", dots$f, " as smoother span for lowess")
    }
    smoothed <- map(
      tf_evaluations(x), \(x) do.call(smoother, append(list(x), dots))$y
    )
  }

  tfd(smoothed, tf_arg(x),
    evaluator = !!attr(x, "evaluator_name"),
    domain = tf_domain(x)
  )
}

#' @export
tf_smooth.default <- function(x, ...) .NotYetImplemented()
