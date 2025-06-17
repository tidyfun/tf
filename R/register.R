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
  assert_warp(warp, x)
  assert_flag(keep_arg)

  arg <- tf_arg(x)
  warp <- tfd(warp, arg = arg)
  ret <- tfd(tf_evaluations(x), tf_evaluations(warp), ...)

  if (!keep_arg) {
    ret <- tfd(ret, arg = arg, ...)
  }
  ret
}

#' @export
tf_warp.tfb <- function(x, warp, ..., keep_arg = FALSE) {
  x <- as.tfd(x)
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  x <- tf_warp(x, warp, ..., keep_arg = keep_arg)
  as.tfb(x)
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
  assert_warp(warp, x)
  assert_flag(keep_arg)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }

  x_arg <- tf_arg(x)
  warp_arg <- tf_arg(warp)
  # TODO: swap w/ tf_invert() once implemented
  warp <- tfd(warp, arg = x_arg)
  inv_warp <- map(tf_evaluations(warp), function(h) {
    stats::approx(x = h, y = warp_arg, xout = x_arg, rule = 2)$y
  })
  ret <- tfd(tf_evaluations(x), arg = inv_warp)

  if (!keep_arg) {
    ret <- tfd(ret, arg = x_arg, ...)
  }
  ret
}

#' @export
tf_unwarp.tfb <- function(x, warp, ..., keep_arg = FALSE) {
  x <- as.tfd(x)
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  x <- tf_unwarp(x, warp, ..., keep_arg = keep_arg)
  as.tfb(x)
}

tf_invert <- function(x) {
  .NotYetImplemented()
}

#' Register a tf vector against a template function
#'
#' @param x a tf vector of functions to register.
#' @param ... additional arguments passed to further methods.
#' @param template a tf vector of a template function to register against.
#' @param method the implementation method to choose.
#' @returns tf vector of the aligning functions, i.e. the warping functions.
#' @export
tf_register <- function(x, ..., template = NULL, method = "srvf") {
  rlang::check_dots_used()
  assert_tfd(x)
  assert_tfd(template, null_ok = TRUE)
  assert_choice(method, c("srvf", "fda"))
  if (
    !is.null(template) && length(template) != 1 && length(template) != length(x)
  ) {
    cli::cli_abort(
      "{.arg template} must be of length 1 or the same length as {.arg x}."
    )
  }

  switch(
    method,
    srvf = tf_register_srvf(x, template, ...),
    fda = tf_register_fda(x, template, ...)
  )
}

tf_register_srvf <- function(x, template, ...) {
  rlang::check_installed("fdasrvf")

  arg <- tf_arg(x)
  domain <- tf_domain(x)
  lwr <- domain[1]
  upr <- domain[2]

  # Karcher mean
  x <- as.matrix(x)
  if (is.null(template)) {
    ret <- suppressMessages(fdasrvf::time_warping(f = t(x), time = arg, ...))
    warp <- t(ret$warping_functions)
  } else {
    is_single_template <- length(template) == 1
    template <- as.matrix(template)
    warp <- matrix(nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      warp[i, ] <- fdasrvf::pair_align_functions(
        f1 = x[i, ],
        f2 = if (is_single_template) template[1, ] else template[i, ],
        time = arg,
        ...
      )$gam
    }
  }
  for (i in seq_len(nrow(warp))) {
    warp[i, ] <- lwr + warp[i, ] * (upr - lwr)
  }
  tfd(warp, arg = arg)
}

tf_register_fda <- function(x, template, ...) {
  rlang::check_installed("fda")

  yfd <- tf_2_fd(x)
  if (is.null(template)) {
    y0fd <- do.call(fda::mean.fd, list(yfd))
  } else {
    y0fd <- tf_2_fd(template)
  }

  utils::capture.output(
    ret <- fda::register.fd(y0fd = y0fd, yfd = yfd, dbglev = 0, ...)
  )
  arg <- tf_arg(x)
  n <- length(arg)
  domain <- tf_domain(x)
  warp <- fda::eval.monfd(arg, ret$Wfd)
  # TODO: check if faster via apply/sweep etc.
  warp <- domain[1] +
    (domain[2] - 1) * warp / (matrix(1, nrow = n) %*% warp[n, ])
  tfd(t(warp), arg = arg)
}
