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
#' By default (`keep_new_arg = FALSE`), the `tfd` methods will return function objects
#' re-evaluated on the same grids as the original inputs,
#' which will typically incur some additional interpolation error because (un)warping
#' changes the underlying grids, which are then changed back. Set to `TRUE` to avoid.
#' This option is not available for `tfb`-objects.
#'
#'
#' @param x `tf` vector of functions. For `tf_warp()`, these should be
#'   registered/aligned functions and unaligned functions for `tf_unwarp()`.
#' @param warp `tf` vector of warping functions used for transformation. See Details.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_new_arg keep new `arg` values after (un)warping or return
#'   `tfd` vector on `arg` values of the input (default `FALSE` is the latter)? See Details.
#' @returns
#' * `tf_warp()`: the warped `tf` vector (un-registered functions)
#' * `tf_unwarp()`: the unwarped `tf` vector (registered/aligned functions)
#'
#' @examples
#' # generate "template" function shape on [0, 1]:
#' set.seed(1351)
#' template <- tf_rgp(1, arg = 201L, nugget = 0)
#' # generate random warping functions (strictly monotone inc., [0, 1] -> [0, 1]):
#' warp <- {
#'   tmp <- tf_rgp(5)
#'   tmp <- exp(tmp - mean(tmp)) # centered at identity warping
#'   tf_integrate(tmp, definite = FALSE) / tf_integrate(tmp)
#' }
#' x <- tf_warp(rep(1, 5) * template, warp)
#' layout(t(1:3))
#' plot(template); plot(warp, col = 1:5); plot(x, col = 1:5)
#' # register the functions:
#' warp_estimate <- tf_register(x)
#' registered <- tf_unwarp(x, warp_estimate)
#' layout(t(1:2))
#' plot(warp_estimate, col = 1:5); lines(warp, lty = 2, col = 1:5)
#' plot(registered, col = 1:5); lines(template, lty = 2)
#' @export
#' @author Maximilian Muecke
tf_warp <- function(x, warp, ...) {
  rlang::check_dots_used()
  assert_warp(warp, x)
  UseMethod("tf_warp")
}
#' @rdname tf_warp
#' @export
tf_warp.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
  assert_flag(keep_new_arg)
  arg <- tf_arg(x)
  warp <- tfd(warp, arg = arg)
  ret <- tfd(tf_evaluations(x), tf_evaluations(warp), ...)

  if (!keep_new_arg) {
    ret <- tfd(ret, arg = arg, ...)
  }
  ret
}
#' @rdname tf_warp
#' @export
tf_warp.tfb <- function(x, warp, ...) {
  # keep_new_arg forced to FALSE here, otherwise basis matrix blows up:
  # would keep every unique gridpoint & cause plots to fail (resolution too small)
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  dots <- list(...)
  if (isTRUE(dots$keep_new_arg)) {
    cli::cli_warn(
      "{.arg keep_new_arg} reset to FALSE - not applicable for {.cls tfb}."
    )
    dots$keep_new_arg <- FALSE
  }
  do.call(tf_warp, list(dots, x = as.tfd(x), warp = warp) |> flatten()) |>
    tf_rebase(x)
}

#-------------------------------------------------------------------------------

#' @rdname tf_warp
#' @export
tf_unwarp <- function(x, warp, ...) {
  rlang::check_dots_used()
  assert_warp(warp, x)
  UseMethod("tf_unwarp")
}
#' @rdname tf_warp
#' @export
tf_unwarp.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
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
#' @rdname tf_warp
#' @export
tf_unwarp.tfb <- function(x, warp, ...) {
  # keep_new_arg forced to FALSE here, otherwise basis matrix blows up:
  # would keep every unique gridpoint & cause plots to fail (resolution too small)
  if (is_tfb(warp)) {
    warp <- as.tfd(warp)
  }
  dots <- list(...)
  if (isTRUE(dots$keep_new_arg)) {
    cli::cli_warn(
      "{.arg keep_new_arg} reset to FALSE - not applicable for {.cls tfb}."
    )
    dots$keep_new_arg <- FALSE
  }
  do.call(tf_unwarp, list(dots, x = as.tfd(x), warp = warp) |> flatten()) |>
    tf_rebase(x)
}

#-------------------------------------------------------------------------------

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
#' # warping functions map from "observed"/"nominal" time to "system"/"standardized" time:
#' warp <- tf_register(growth_female)
#' plot(warp, xlab = "Chronological Age", ylab = "Biological Age")
#' growth_female_reg <- tf_unwarp(growth_female, warp)
#' plot(growth_female_reg, xlab = "Biological Age (years)", ylab = "Growth Rate (cm/year)")
tf_register <- function(.x, ..., .template = NULL, .method = "srvf") {
  UseMethod("tf_register")
}

#' @export
tf_register.tfd_reg <- function(.x, ..., .template = NULL, .method = "srvf") {
  rlang::check_dots_used()
  assert_tfd(.x)
  assert_choice(.method, c("srvf", "fda"))
  if (!is.null(.template)) {
    assert_tf(.template)
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

#' @export
tf_register.tfd_irreg <- function(.x, ..., .template = NULL, .method = "srvf") {
  cli::cli_abort(
    "{.cls tfd_irreg} objects cannot be registered. Please convert to {.cls tfd_reg} first."
  )
}

#-------------------------------------------------------------------------------

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
  # avoid numerical over/underflow issue:
  warp[, 1] <- arg[1]
  warp[, length(arg)] <- arg[length(arg)]
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
  # avoid numerical over/underflow issues:
  warp[1, ] <- lwr
  warp[n, ] <- upr
  tfd(t(warp), arg = arg)
}

#-------------------------------------------------------------------------------

#' Landmark Registration
#'
#' @description
#' `tf_register_landmark()` performs piecewise linear registration by aligning
#' user-specified landmarks across functions. This is useful when functions have
#' identifiable features (peaks, valleys, zero-crossings) that should be aligned.
#'
#' `tf_landmarks_extrema()` is a helper function that automatically finds the
#' locations of extrema (maxima and/or minima) in each function.
#'
#' @details
#' Landmark registration creates piecewise linear warping functions that map
#' each curve's landmark locations to template landmark locations. Between
#' landmarks, the warping function interpolates linearly, stretching or
#' compressing the time axis to align the features.
#'
#' The landmarks matrix must have the same number of rows as functions in `.x`.
#' Each row contains the observed landmark positions for that function, and
#' each column represents one landmark feature. Landmarks must be strictly
#' increasing within each row and within the domain of `.x`.
#'
#' @param .x a `tf` vector of functions to register.
#' @param .landmarks a numeric matrix of landmark positions with one row per
#'   function and one column per landmark feature. Landmarks must be strictly
#'   increasing within each row.
#' @param .template_landmarks optional numeric vector of template landmark
#'   positions to align to. Must have the same length as `ncol(.landmarks)`.
#'   If `NULL` (default), uses the column-wise means of `.landmarks`.
#' @returns a `tfd` vector of warping functions with the same length as `.x`.
#'   These can be used with [tf_unwarp()] to align the functions.
#' @seealso [tf_register()] for elastic registration, [tf_register_affine()]
#'   for affine registration
#'
#' @examples
#' # Create functions with shifted peaks
#' t <- seq(0, 1, length.out = 101)
#' shifts <- c(0.4, 0.5, 0.6) # peak locations
#' x <- tfd(
#'   t(sapply(shifts, function(s) dnorm(t, mean = s, sd = 0.1))),
#'   arg = t
#' )
#' plot(x, col = 1:3)
#'
#' # Find peak locations and register
#' peaks <- tf_landmarks_extrema(x, "max")
#' warp <- tf_register_landmark(x, peaks)
#' x_aligned <- tf_unwarp(x, warp)
#' plot(x_aligned, col = 1:3)
#'
#' @export
#' @family registration functions
tf_register_landmark <- function(.x, .landmarks, .template_landmarks = NULL) {
  assert_tf(.x)
  assert_matrix(.landmarks, mode = "numeric", nrows = length(.x), min.cols = 1)

  domain <- tf_domain(.x)
  n <- length(.x)
  n_landmarks <- ncol(.landmarks)

  validate_landmarks(.landmarks, domain, n, n_landmarks)
  .template_landmarks <- validate_template_landmarks(
    .template_landmarks,
    .landmarks,
    domain,
    n_landmarks
  )

  # Create piecewise linear warping functions:
  # Template arg: domain boundaries + template landmarks
  # Warping values: domain boundaries + observed landmarks
  template_arg <- c(domain[1], .template_landmarks, domain[2])
  warp_values <- cbind(domain[1], .landmarks, domain[2])

  tfd(warp_values, arg = template_arg)
}

# Helper: validate landmark matrix
validate_landmarks <- function(landmarks, domain, n, n_landmarks) {
  # Check strictly increasing within each row
  for (i in seq_len(n)) {
    if (n_landmarks > 1 && !all(diff(landmarks[i, ]) > 0)) {
      cli::cli_abort(
        "Landmarks must be strictly increasing within each row. Problem at row {i}."
      )
    }
  }
  # Check within domain
  if (any(landmarks < domain[1]) || any(landmarks > domain[2])) {
    cli::cli_abort(
      "All landmarks must be within the domain [{domain[1]}, {domain[2]}]."
    )
  }
  invisible(landmarks)
}

# Helper: validate and return template landmarks
validate_template_landmarks <- function(
  template,
  landmarks,
  domain,
  n_landmarks
) {
  if (is.null(template)) {
    return(colMeans(landmarks))
  }

  assert_numeric(template, len = n_landmarks, any.missing = FALSE)

  if (n_landmarks > 1 && !all(diff(template) > 0)) {
    cli::cli_abort("Template landmarks must be strictly increasing.")
  }
  if (any(template < domain[1]) || any(template > domain[2])) {
    cli::cli_abort(
      "Template landmarks must be within the domain [{domain[1]}, {domain[2]}]."
    )
  }
  template
}

#' @rdname tf_register_landmark
#' @param x a `tf` vector for `tf_landmarks_extrema()`.
#' @param which character specifying which extrema to find: `"max"` for maxima,
#'   `"min"` for minima, or `"both"` for both (maxima first, then minima).
#' @returns For `tf_landmarks_extrema()`: a numeric matrix with one row per
#'   function containing the locations of the specified extrema.
#' @export
tf_landmarks_extrema <- function(x, which = c("max", "min", "both")) {
  assert_tf(x)
  which <- match.arg(which)

  extrema <- switch(
    which,
    max = tf_where(x, value == max(value), "first"),
    min = tf_where(x, value == min(value), "first"),
    both = cbind(
      tf_where(x, value == max(value), "first"),
      tf_where(x, value == min(value), "first")
    )
  )

  if (!is.matrix(extrema)) {
    extrema <- matrix(extrema, ncol = 1)
  }

  extrema
}

#-------------------------------------------------------------------------------

#' Affine Registration
#'
#' @description
#' `tf_register_affine()` performs registration using affine (linear) warping
#' functions of the form \eqn{h(t) = a \cdot t + b}, where \eqn{a} controls
#' scaling (dilation/compression) and \eqn{b} controls shift (translation).
#'
#' @details
#' Affine registration is simpler than elastic registration and is appropriate
#' when the phase variability in the data consists only of shifts and/or
#' uniform scaling of the time axis.
#'
#' Three types of affine registration are available:
#'
#' - `"shift"`: Only horizontal translation (\eqn{a = 1}, optimize \eqn{b}).
#'   The functions are shifted left or right to align with the template.
#'
#' - `"scale"`: Only scaling (\eqn{b} determined by anchor, optimize \eqn{a}).
#'   The time axis is stretched or compressed around the anchor point.
#'
#' - `"shift_scale"`: Both shift and scale (optimize both \eqn{a} and \eqn{b}).
#'   Combines translation and scaling for more flexible alignment.
#'
#' The `.anchor` parameter determines the fixed point for scaling:
#' - `"start"`: The domain start is fixed; scaling expands/contracts toward the end.
#' - `"end"`: The domain end is fixed; scaling expands/contracts toward the start.
#' - `"center"`: The domain center is fixed; scaling expands/contracts symmetrically.
#'
#' Optimization minimizes the L2 distance between each (affinely warped)
#' function and the template.
#'
#' @param .x a `tf` vector of functions to register.
#' @param .template an optional `tf` vector of length 1 to use as the template.
#'   If `NULL` (default), the cross-sectional mean of `.x` is used.
#' @param .type character specifying the type of affine transformation:
#'   `"shift"`, `"scale"`, or `"shift_scale"`.
#' @param .anchor character specifying the anchor point for scaling:
#'   `"start"`, `"end"`, or `"center"`. Only used when `.type` includes scaling.
#' @returns a `tfd` vector of warping functions with the same length as `.x`.
#'   These can be used with [tf_unwarp()] to align the functions.
#' @seealso [tf_register()] for elastic registration, [tf_register_landmark()]
#'   for landmark-based registration
#'
#' @examples
#' # Create shifted sinusoids
#' t <- seq(0, 2 * pi, length.out = 101)
#' shifts <- seq(-0.5, 0.5, length.out = 5)
#' x <- tfd(t(sapply(shifts, function(s) sin(t + s))), arg = t)
#' plot(x, col = 1:5)
#'
#' # Shift registration
#' warp <- tf_register_affine(x, .type = "shift")
#' x_aligned <- tf_unwarp(x, warp)
#' plot(x_aligned, col = 1:5)
#'
#' @export
#' @family registration functions
#' @importFrom stats approx optim
tf_register_affine <- function(
  .x,
  .template = NULL,
  .type = c("shift", "scale", "shift_scale"),
  .anchor = c("start", "end", "center")
) {
  assert_tf(.x)
  .type <- match.arg(.type)
  .anchor <- match.arg(.anchor)

  domain <- tf_domain(.x)
  .template <- validate_affine_template(.template, .x, domain)

  # Get optimization configuration for this type
  config <- affine_config(.type, .anchor, domain)

  # Prepare data matrices on common grid
  arg <- tf_arg(.x)
  if (is.list(arg)) arg <- sort_unique(arg, simplify = TRUE)

  x_mat <- as.matrix(tfd(.x, arg = arg))
  template_vec <- as.matrix(tfd(.template, arg = arg))[1, ]

  # Optimize each curve
  warp_mat <- map(
    seq_len(nrow(x_mat)),
    \(i) optimize_affine_warp(x_mat[i, ], template_vec, arg, domain, config)
  ) |>
    do.call(what = rbind)

  tfd(warp_mat, arg = arg)
}

# Helper: validate template for affine registration
validate_affine_template <- function(template, x, domain) {
  if (is.null(template)) return(mean(x))

  assert_tf(template)
  if (length(template) != 1) {
    cli::cli_abort("{.arg .template} must be of length 1.")
  }
  if (!all(domain == tf_domain(template))) {
    cli::cli_abort("{.arg .x} and {.arg .template} must have the same domain.")
  }
  template
}

# Helper: get optimization configuration based on type
affine_config <- function(type, anchor, domain) {
  lwr <- domain[1]
  upr <- domain[2]
  half_range <- (upr - lwr) / 2

  anchor_point <- switch(
    anchor,
    start = lwr,
    end = upr,
    center = (lwr + upr) / 2
  )

  # Named constants for optimization bounds
  scale_lower <- 0.5
  scale_upper <- 2.0

  switch(
    type,
    shift = list(
      type = "shift",
      init = 0,
      lower = -half_range,
      upper = half_range,
      anchor = anchor_point
    ),
    scale = list(
      type = "scale",
      init = 1,
      lower = scale_lower,
      upper = scale_upper,
      anchor = anchor_point
    ),
    shift_scale = list(
      type = "shift_scale",
      init = c(1, 0),
      lower = c(scale_lower, -half_range),
      upper = c(scale_upper, half_range),
      anchor = anchor_point
    )
  )
}

# Helper: optimize affine warp for one curve
optimize_affine_warp <- function(x_vec, template_vec, arg, domain, config) {
  lwr <- domain[1]
  upr <- domain[2]
  penalty <- 1e10

  # Objective: L2 distance after affine warping
  objective <- function(params) {
    ab <- extract_affine_params(params, config)
    warped_arg <- ab$a * arg + ab$b

    # Penalize out-of-domain warping
    if (any(warped_arg < lwr) || any(warped_arg > upr)) return(penalty)

    x_warped <- approx(arg, x_vec, xout = warped_arg, rule = 2)$y
    sum((x_warped - template_vec)^2)
  }

  opt <- optim(
    par = config$init,
    fn = objective,
    method = "L-BFGS-B",
    lower = config$lower,
    upper = config$upper
  )

  ab <- extract_affine_params(opt$par, config)
  warp_values <- ab$a * arg + ab$b

  # Clamp boundaries
  warp_values[1] <- max(warp_values[1], lwr)
  warp_values[length(warp_values)] <- min(warp_values[length(warp_values)], upr)

  warp_values
}

# Helper: extract (a, b) parameters from optimizer output
extract_affine_params <- function(params, config) {
  switch(
    config$type,
    shift = list(a = 1, b = params[1]),
    scale = list(a = params[1], b = config$anchor * (1 - params[1])),
    shift_scale = list(a = params[1], b = params[2])
  )
}
