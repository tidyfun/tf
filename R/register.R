#' Warp a tf vector
#'
#' Apply (inverse of given aligning functions)/(given warping functions) to
#' aligned functional data to get back to the original unaligned data:
#' $x(s) \to x(h(s)) = x(t)$.
#'
#' @param x tf vector of (registered) functions.
#' @param warp tf vector of warping functions.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_arg re-evaluate on warped arg values after un-warping or return
#'   tf on un-warped arg-vals (default)?
#' @returns the warped tf vector, i.e. the unregistered functions.
#' @export
tf_warp <- function(x, warp, ..., keep_arg = FALSE) {
  rlang::check_dots_used()
  UseMethod("tf_warp")
}

#' @export
tf_warp.tfd <- function(x, warp, ..., keep_arg = FALSE) {
  assert_tfd(warp)
  assert_flag(keep_arg)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }
  if (!all(tf_domain(x) == tf_domain(warp))) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same domain.")
  }
  if (!all(map_lgl(tf_frange(warp), \(x) all(x == tf_domain(warp))))) {
    cli::cli_abort("{.arg warp} domain and range must be the same.")
  }

  ret <- tfd(tf_evaluations(x), arg = tf_evaluations(warp), ...)
  if (!keep_arg) {
    ret <- tfd(ret, arg = tf_arg(x), ...)
  }
  ret
}

#' Unwarp a tf vector
#'
#' Apply (inverse of given warping functions)/(given aligning functions) to
#' functional data: $x(t) \to x(h^{-1}(t)) = x(s)$.
#'
#' @param x tf vector of unregistered functions.
#' @param warp tf vector of aligning functions.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_arg re-eval on original arg after warping or return (irregular)
#'   tf on warped arg (default)?
#' @returns the unwarped tf vector, i.e. the registered functions.
#' @export
tf_unwarp <- function(x, warp, ..., keep_arg = FALSE) {
  rlang::check_dots_used()
  UseMethod("tf_unwarp")
}

#' @export
tf_unwarp.tfd <- function(x, warp, ..., keep_arg = FALSE) {
  assert_tfd(warp)
  assert_flag(keep_arg)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }
  if (!is_reg(warp)) {
    cli::cli_abort("{.arg warp} must be of {.cls tfd_reg} vector.")
  }
  if (!all(tf_domain(x) == tf_domain(warp))) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same domain.")
  }
  if (!all(map_lgl(tf_frange(warp), \(x) all(x == tf_domain(warp))))) {
    cli::cli_abort("{.arg warp} domain and range must be the same.")
  }

  x_arg <- tf_arg(x)
  warp_arg <- tf_arg(warp)
  inv_warp <- map(tf_evaluations(warp), function(h) {
    stats::approx(x = h, y = warp_arg, xout = x_arg, rule = 2)$y
  })
  ret <- tfd(tf_evaluations(x), arg = inv_warp)

  if (!keep_arg) {
    ret <- tfd(ret, arg = x_arg, ...)
  }
  ret
}

#' Register a tf vector against a template function
#'
#' @param x a tf vector of functions to register.
#' @param ... additional arguments passed to further methods.
#' @param template a tf vector of a template function to register against.
#' @param method the implementation method to choose.
#' @returns tf vector of the aligning functions, i.e. the warping functions.
#' @export
tf_register_template <- function(x, ..., template = NULL, method = "srvf") {
  rlang::check_dots_used()
  assert_tfd(x)
  assert_tfd(template, null_ok = TRUE)
  assert_choice(method, c("srvf", "fda"))
  # TODO: should we allow length 1 and recycle?
  if (
    !is.null(template) && length(template) != 1 && length(x) != length(template)
  ) {
    cli::cli_abort("{.arg x} and {.arg template} must have the same length.")
  }

  arg <- tf_arg(x)
  domain <- tf_domain(x)
  lwr <- domain[1]
  upr <- domain[2]

  if (method == "srvf") {
    rlang::check_installed("fdasrvf")
    # Karcher mean
    x <- as.matrix(x)
    if (is.null(template)) {
      ret <- suppressMessages(fdasrvf::time_warping(f = t(x), time = arg, ...))
      warp <- t(ret$warping_functions)
    } else {
      template <- as.matrix(template)
      warp <- matrix(nrow = nrow(x), ncol = ncol(x))
      for (i in seq_len(nrow(x))) {
        warp[i, ] <- fdasrvf::pair_align_functions(
          f1 = x[i, ],
          f2 = if (nrow(template) == 1) template[1, ] else template[i, ],
          time = arg,
          ...
        )$gam
      }
    }
    for (i in seq_len(nrow(warp))) {
      warp[i, ] <- lwr + warp[i, ] * (upr - lwr)
    }
  } else {
    rlang::check_installed("fda")
    yfd <- as_fd(x)
    if (is.null(template)) {
      y0fd <- do.call(fda::mean.fd, list(yfd))
    } else {
      y0fd <- as_fd(template)
    }
    utils::capture.output(
      ret <- fda::register.fd(
        y0fd = y0fd,
        yfd = yfd,
        dbglev = 0,
        ...
      )
    )
    warp <- fd_to_matrix(ret$warpfd, tf_count(x))
    # TODO: returns values outside the domain
    # fda::eval.fd() returns values outside the domain
    for (i in seq_len(nrow(warp))) {
      h_min <- min(warp[i, ])
      h_max <- max(warp[i, ])
      warp[i, ] <- lwr + (warp[i, ] - h_min) / (h_max - h_min) * (upr - lwr)
    }
  }

  tfd(warp, arg = arg)
}

as_fd <- function(x, ..., nbasis = NULL, lambda = 0) {
  rlang::check_installed("fda")

  domain <- tf_domain(x)
  arg <- tf_arg(x)
  y_mat <- t(as.matrix(x))
  nbasis <- nbasis %||% min(25, round(length(arg) / 4))

  basis <- fda::create.bspline.basis(
    rangeval = domain,
    nbasis = nbasis,
    norder = 4
  )
  param <- fda::fdPar(basis, lambda = lambda)
  fda::smooth.basis(argvals = arg, y = y_mat, fdParobj = param, ...)$fd
}
