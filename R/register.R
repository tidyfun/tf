#' Elastic Deformation: warp and unwarp `tf` vectors

#' @description
#' These functions stretch and/or compress regions of the domain of functional data:
#'
#' - `tf_warp()` applies warping functions to aligned (registered) functional data
#' to recover the original unregistered curves: \eqn{x(s) \to x(h(s)) = x(t)}.
#' - `tf_unwarp()` applies the *inverse* warping function to unregistered data
#' to obtain aligned (registered) functions: \eqn{x(t) \to x(h^{-1}(t)) = x(s)}.
#'
#' @details
#' These functions will work best with functions evaluated on suitably dense and
#' regular grids.
#'
#' Warping functions \eqn{h(s) = t} are strictly monotone increasing (no time
#' travel backwards or infinite time dilation) with identical domain and
#' co-domain: \eqn{h:T \to T}. Their input is the aligned "system" time \eqn{s},
#' their output is the unaligned "observed" time \eqn{t}.
#'
#' By default (`keep_new_arg = FALSE`), these will return function objects
#' re-evaluated on the same grids as the original inputs,
#' which will typically incur some additional interpolation error because (un)warping
#' changes the underlying grids. Set to `TRUE` to avoid.
#'
#'
#' @param x `tf` vector of functions. For `tf_warp()`, these should be
#'   registered/aligned functions and unaligned functions for `tf_unwarp()`.
#' @param warp `tf` vector of warping functions used for transformation. See Details.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_new_arg keep new `arg` values after (un)warping or return
#'   `tf` vector on `arg` values of the input (default `FALSE` is the latter)? See Details.
#' @returns
#' * `tf_warp()`: the warped `tf` vector (un-registered functions)
#' * `tf_unwarp()`: the unwarped `tf` vector (registered/aligned functions)
#'
#' @examples
#' # generate "template" function shape:
#' template <- tf_rgp(1, arg = 201L)
#' # generate random warping functions (monotone, [0, 1] -> [0, 1]):
#' warp <- {
#'   tmp <- tf_rgp(5)
#'   tmp <- exp(tmp - mean(tmp)) # center at identity warping
#'   tf_integrate(tmp, definite = FALSE) / tf_integrate(tmp)
#' }
#' x <- tf_warp(rep(template, 5), warp)
#' layout(t(1:3))
#' plot(template); plot(warp, col = 1:5); plot(x, col = 1:5)
#' # register the functions:
#' warp_estimate <- tf_register(x)
#' template_estimate <- tf_unwarp(x, warp_estimate)
#' layout(t(1:2))
#' plot(warp_estimate, col = 1:5); lines(warp, lty = 2, col = 1:5)
#' plot(template_estimate, col = 1:5); lines(template, lty = 2)
#' @export
#' @author Maximilian Muecke
tf_warp <- function(x, warp, ..., keep_new_arg = FALSE) {
  rlang::check_dots_used()
  UseMethod("tf_warp")
}

#' @export
tf_warp.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
  assert_warp(warp, x)
  assert_flag(keep_new_arg)

  arg <- tf_arg(x)
  warp <- tfd(warp, arg = arg)
  ret <- tfd(tf_evaluations(x), tf_evaluations(warp), ...)

  if (!keep_new_arg) {
    ret <- tfd(ret, arg = arg, ...)
  }
  ret
}

#' @export
tf_warp.tfb <- function(x, warp, ..., keep_new_arg = FALSE) {
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  x |>
    as.tfd() |>
    tf_warp(warp, ..., keep_new_arg = keep_new_arg) |>
    tf_rebase(x)
}

#' @rdname tf_warp
#' @export
tf_unwarp <- function(x, warp, ..., keep_new_arg = FALSE) {
  rlang::check_dots_used()
  UseMethod("tf_unwarp")
}

#' @export
tf_unwarp.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
  assert_warp(warp, x)
  assert_flag(keep_new_arg)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }

  arg <- tf_arg(x)
  inv_warp <- warp |> tfd(arg = arg) |> tf_invert() |> tfd(arg = arg)
  ret <- tfd(tf_evaluations(x), arg = tf_evaluations(inv_warp))

  if (!keep_new_arg) {
    ret <- tfd(ret, arg = arg, ...)
  }
  ret
}

#' @export
tf_unwarp.tfb <- function(x, warp, ..., keep_new_arg = FALSE) {
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  x |>
    as.tfd() |>
    tf_unwarp(warp, ..., keep_new_arg = keep_new_arg) |>
    tf_rebase(x)
}

#' Register / align a `tf` vector against a template function
#'
#' `tf_register()` performs functional data registration (alignment) by finding
#' warping functions that optimally align a set of functions to a template function.
#' Registration removes phase variation (horizontal shifts and stretches) while
#' preserving amplitude (i.e., vertical) variation, making it easier to analyze
#' the intrinsic shape characteristics of functional data.
#'
#' @param .x a `tf` vector of functions to register.
#' @param ... additional arguments passed to further methods.
#' @param .template an optional `tf` vector of a template function to register against.
#'   If `NULL`, the Karcher mean (for SRVF) or arithmetic mean (for FDA) is used as the template.
#' @param .method the implementation method to choose. Either `"srvf"` or `"fda"`.
#'   * `srvf`: Square Root Velocity Framework (SRVF) framework.
#'     For details, see [fdasrvf::time_warping()] and [fdasrvf::pair_align_functions()].
#'   * `fda`: continuous‐criterion registration. For details, see [fda::register.fd()].
#' @returns `tf` vector of the warping functions with the same length as `x`.
#'
#' @references
#' `r format_bib("ramsay2009functional", "srivastava2011registration", "tucker2013generative")`
#' @export
#' @author Maximilian Muecke
#' @examplesIf rlang::is_installed(c("fdasrvf", "fda"))
#' height_female <- subset(growth, gender == "female", select = height, drop = TRUE)
#' growth_female <- tf_derive(height_female) |> tfd(arg = seq(1.125, 17.8), l = 101)
#' layout(t(1:3))
#' plot(growth_female, xlab = "Chronological Age (years)", ylab = "Growth Rate (cm/year)")
#' # warping functions map from "observed","nominal" time to "system","standardized" time:
#' warp <- tf_register(growth_female)
#' plot(warp, xlab = "Chronological Age", ylab = "Biological Age")
#' growth_female_reg <- tf_unwarp(growth_female, warp)
#' plot(growth_female_reg, xlab = "Biological Age (years)", ylab = "Growth Rate (cm/year)")
tf_register <- function(.x, ..., .template = NULL, .method = "srvf") {
  UseMethod("tf_register")
}

#' @export
tf_register.tfd <- function(.x, ..., .template = NULL, .method = "srvf") {
  rlang::check_dots_used()
  assert_tfd(.x)
  assert_choice(.method, c("srvf", "fda"))
  assert_tfd(.template, null_ok = TRUE)
  if (!is.null(.template)) {
    if (length(.template) != 1 && length(.template) != length(.x)) {
      cli::cli_abort(
        "{.arg template} must be of length 1 or the same length as {.arg x}."
      )
    }
    if (!all(tf_domain(.x) == tf_domain(.template))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same domain.")
    }
    template_arg <- tf_arg(.template)
    if (is_irreg(.x) && length(.template) == 1) {
      template_arg <- rep(ensure_list(template_arg), length(.x))
    }
    if (!isTRUE(all.equal(tf_arg(.x), template_arg))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same grid.")
    }
  }

  switch(
    .method,
    srvf = tf_register_srvf(.x, .template, ...),
    fda = tf_register_fda(.x, .template, ...)
  )
}

#' @export
tf_register.tfb <- function(.x, ..., .template = NULL, .method = "srvf") {
  .x |> as.tfd() |> tf_register(.template = .template, .method = .method, ...)
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

# TODO: add simple shift/dilate/compress registration using affine warping functions
# TODO: add landmark registration?
