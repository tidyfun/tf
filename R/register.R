#' Register a tf vector
#'
#' @param x a tf vector to register.
#' @param ... additional arguments passed to [fdasrvf::time_warping()].
#' @param arg the arg vector.
#' @param verbose print progress messages?
#' @returns `list()` containing: registered tf vector `x` and warping functions `warp`.
#' @export
tf_register <- function(x, ..., arg = NULL, verbose = FALSE) {
  rlang::check_dots_used()
  UseMethod("tf_register")
}

#' @export
tf_register.tfd <- function(x, ..., arg = NULL, verbose = FALSE) {
  rlang::check_installed("fdasrvf")
  arg <- assert_arg(arg, x, null_ok = TRUE) %||% tf_arg(x)
  assert_flag(verbose)

  x <- t(as.matrix(x))
  if (verbose) {
    reg <- fdasrvf::time_warping(f = x, time = arg, ...)
  } else {
    reg <- suppressMessages(fdasrvf::time_warping(f = x, time = arg, ...))
  }
  list(
    x = tfd(t(reg$fn), arg = arg),
    warp = tfd(t(reg$warping_functions), arg = unit_seq(length(arg)))
  )
}

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
  if (!is_reg(warp)) {
    cli::cli_abort("{.arg warp} must be of {.cls tfd_reg} vector.")
  }

  arg <- tf_arg(x)
  lwr <- arg[[1]]
  upr <- arg[[length(arg)]]
  new_arg <- map(tf_evaluations(warp), \(h) (upr - lwr) * h + lwr)
  warped <- tfd(x, arg = new_arg, ...)
  if (!keep_arg) {
    warped <- tfd(warped, arg = arg, ...)
  }
  warped
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
tf_unwarp.tfd <- function(x, warp, keep_arg = FALSE, ...) {
  assert_tfd(warp)
  assert_flag(keep_arg)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }
  if (!is_reg(warp)) {
    cli::cli_abort("{.arg warp} must be of {.cls tfd_reg} vector.")
  }

  arg <- tf_arg(x)
  lwr <- arg[[1]]
  upr <- arg[[length(arg)]]

  inv_warp <- map(tf_evaluations(warp), function(h) {
    stats::approx(
      x = (upr - lwr) * h + lwr,
      y = arg,
      xout = arg,
      method = "linear"
    )$y
  })

  ret <- tfd(x, arg = inv_warp, ...)
  if (!keep_arg) {
    ret <- tfd(ret, arg = arg, ...)
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
  assert_tfd(x)
  assert_tfd(template, null_ok = TRUE)
  assert_choice(method, c("srvf", "fda"))

  if (method == "srvf") {
    rlang::check_installed("fdasrvf")
    arg <- tf_arg(x)

    # Karcher mean
    if (is.null(template)) {
      ret <- suppressMessages(fdasrvf::time_warping(
        f = t(as.matrix(x)),
        time = arg,
        ...
      ))
      warp <- tfd(t(ret$warping_functions), arg = unit_seq(length(arg)))
      return(warp)
    }

    x <- as.matrix(x)
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
    return(tfd(warp, arg = unit_seq(length(arg))))
  }

  if (method == "fda") {
    rlang::check_installed("fda")
    yfd <- as.fd(x)
    if (is.null(template)) {
      y0fd <- do.call(fda::mean.fd, list(yfd))
    }
    ret <- fda::register.fd(y0fd = y0fd, yfd = yfd, ...)
    ret <- fd_to_matrix(ret$warpfd, tf_count(x))
    ret <- tfd(t(ret), arg = tf_arg(x))
    ret
  }
}

#' @export
as.fd.tfd <- function(x, nbasis = NULL, lambda = 0, ...) {
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

unit_seq <- function(n) seq(0, 1, length.out = n)
