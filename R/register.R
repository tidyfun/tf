#' Warp a tf vector
#'
#' Apply (inverse of given aligning functions)/(given warping functions) to
#' aligned functional data to get back to the original unaligned data:
#' \eqn{x(s) \to x(h(s)) = x(t)}.
#'
#' @param x tf vector of (registered) functions.
#' @param warp tf vector of warping functions.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_arg re-evaluate on warped arg values after un-warping or return
#'   tf on un-warped arg-vals (default)?
#' @returns the warped tf vector, i.e. the unregistered functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # TODO: add examples
#' tf_warp(x, warp)
#' }
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
#' functional data: \eqn{x(t) \to x(h^{-1}(t)) = x(s)}.
#'
#' @param x tf vector of unregistered functions.
#' @param warp tf vector of aligning functions.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_arg re-eval on original arg after warping or return (irregular)
#'   tf on warped arg (default)?
#' @returns the unwarped tf vector, i.e. the registered functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # TODO: add examples
#' tf_unwarp(x, warp)
#' }
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

  inv_warp <- tfd(warp, arg = x_arg) |> tf_invert() |> tfd(arg = x_arg)
  ret <- tfd(tf_evaluations(x), arg = tf_evaluations(inv_warp))

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

#' Invert a tf vector
#'
#' Computes the functional inverse of each function in the tf vector, such that
#' if \eqn{y = f(x)}, then \eqn{x = f^{-1}(y)}.
#'
#' @param x a tf vector.
#' @returns a tf vector of the inverted functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # TODO: add examples
#' tf_invert(x)
#' }
tf_invert <- function(x) {
  UseMethod("tf_invert")
}

#' @export
tf_invert.tfd <- function(x) {
  # TODO: move to calculus.R eventually
  assert_tfd(x)
  arg <- ensure_list(tf_arg(x))
  if (length(x) > 1 && length(arg) == 1) {
    arg <- rep(arg, length(x))
  }
  tfd(arg, arg = tf_evaluations(x))
}

#' @export
tf_invert.tfb <- function(x) {
  # TODO: tfb_spline: invert then tf_rebase into original basis (unless link function is present...)
  # TODO: tfb_fpc: new fpc basis
  .NotYetImplemented()
}

#' Register a tf vector against a template function
#'
#' `tf_register()` performs functional data registration (alignment) by finding
#' warping functions that optimally align a set of functions to a template function.
#' Registration removes phase variation (horizontal shifts and stretches) while
#' preserving amplitude variation, making it easier to analyze the intrinsic
#' shape characteristics of functional data.
#'
#' @param x a tf vector of functions to register.
#' @param ... additional arguments passed to further methods.
#' @param template an optional tf vector of a template function to register against.
#'   If `NULL`, the Karcher mean (for SRVF) or arithmetic mean (for FDA) is used as the template.
#' @param method the implementation method to choose. Either `"srvf"` or `"fda"`.
#'   * `srvf`: uses the Square-Root Velocity Function (SRVF) framework for registration.
#'     For details, see [fdasrvf::time_warping()] and [fdasrvf::pair_align_functions()].
#'   * `fda`: uses the functional data analysis approach for registration.
#'     For details, see [fda::register.fd()].
#' @returns tf vector of the the warping functions with the same length as `x`.
#'
#' @references `r format_bib("ramsay2009functional", "srivastava2011registration", "tucker2013generative")`
#' @export
#' @examplesIf rlang::is_installed(c("fdasrvf", "fda"))
#' heigth_female <- subset(growth, gender == "female", select = heigth, drop = TRUE)
#' growth_female <- tf_derive(height_female)
#' plot(growth_female, xlab = "Age (years)", ylab = "Growth Rate (cm/year)")
#' warp <- tf_register(growth_female)
#' plot(warp, xlab = "Clock Year", ylab = "Biological Year")
#' growth_female_reg <- tf_unwarp(growth_female, warp)
#' plot(growth_female_reg, xlab = "Age (years)", ylab = "Growth Rate (cm/year)",)
tf_register <- function(x, ..., template = NULL, method = "srvf") {
  rlang::check_dots_used()
  assert_tfd(x)
  assert_choice(method, c("srvf", "fda"))
  assert_tfd(template, null_ok = TRUE)
  if (!is.null(template)) {
    if (length(template) != 1 && length(template) != length(x)) {
      cli::cli_abort(
        "{.arg template} must be of length 1 or the same length as {.arg x}."
      )
    }
    if (!all(tf_domain(x) == tf_domain(template))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same domain.")
    }
    template_arg <- tf_arg(template)
    if (is_irreg(x) && length(template) == 1) {
      template_arg <- rep(ensure_list(template_arg), length(x))
    }
    if (!isTRUE(all.equal(tf_arg(x), template_arg))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same grid.")
    }
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
        f1 = if (is_single_template) template[1, ] else template[i, ],
        f2 = x[i, ],
        time = arg,
        ...
      )$gam
    }
  }
  warp <- lwr + (upr - lwr) * warp
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
  lwr <- domain[1]
  upr <- domain[2]

  warp <- fda::eval.monfd(arg, ret$Wfd)
  warp <- lwr + (upr - lwr) * warp / (matrix(1, nrow = n) %*% warp[n, ])
  tfd(t(warp), arg = arg)
}
