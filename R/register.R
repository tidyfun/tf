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
  domain <- tf_domain(x)

  # Check if warp is NOT domain-preserving (true affine warps are not)

  # Domain-preserving means h(lwr) = lwr and h(upr) = upr (bijection on domain)
  # Non-domain-preserving includes:
  #   - Expansion: warp values extend OUTSIDE domain (shift)
  #   - Shrinkage: warp values stay in PROPER SUBSET (scale with a < 1)
  warp_evals <- tf_evaluations(warp)
  is_non_domain_preserving <- any(sapply(warp_evals, function(v) {
    warp_min <- min(v, na.rm = TRUE)
    warp_max <- max(v, na.rm = TRUE)
    # Check for expansion OR shrinkage
    warp_min < domain[1] ||
      warp_max > domain[2] ||
      warp_min > domain[1] + 1e-10 ||
      warp_max < domain[2] - 1e-10
  }))

  if (is_non_domain_preserving) {
    # For warps that go outside the domain (true affine warps), compute x(h(t))
    # The optimizer finds h such that x(h(s)) ≈ template(s)
    # Create irregular tfd with only valid points, then re-evaluate on grid
    arg_vec <- if (is.list(arg)) arg[[1]] else arg

    x_evals <- tf_evaluations(x)
    evaluator_name <- attr(x, "evaluator_name") %||% "tf_approx_linear"

    # Build (arg, value) pairs for each function, keeping only valid points
    valid_data <- map2(x_evals, warp_evals, function(x_vals, warp_vals) {
      # warp_vals = h(arg_vec), we want x(h(arg_vec)) at each arg point
      # Use rule=1 to get NA where warp goes outside original domain
      reg_vals <- approx(arg_vec, x_vals, xout = warp_vals, rule = 1)$y
      valid <- !is.na(reg_vals)
      list(arg = arg_vec[valid], value = reg_vals[valid])
    })

    # Create irregular tfd with only valid points
    ret <- new_tfd(
      arg = map(valid_data, "arg"),
      datalist = map(valid_data, "value"),
      regular = FALSE,
      domain = domain,
      evaluator = evaluator_name
    )

    if (!keep_new_arg) {
      # Re-evaluate on regular grid (evaluator returns NA outside valid range)
      ret <- tfd(ret, arg = arg, ...)
    }
    return(ret)
  } else {
    # Original approach for domain-preserving warps
    inv_warp <- warp |> tfd(arg = arg) |> tf_invert() |> tfd(arg = arg)
    inv_warp_evals <- tf_evaluations(inv_warp)
    ret <- tfd(tf_evaluations(x), arg = inv_warp_evals)
    if (!keep_new_arg) {
      ret <- tfd(ret, arg = arg, ...)
    }
    return(ret)
  }
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
#' @param x a `tf` vector of functions to register.
#' @param ... method-specific arguments (see Details).
#' @param template an optional `tf` vector of length 1 to use as the template.
#'   If `NULL`, a default template is computed (method-dependent).
#'   Not used for `method = "landmark"`.
#' @param method the registration method to use:
#'   * `"srvf"`: Square Root Velocity Framework (elastic registration).
#'     For details, see [fdasrvf::time_warping()]. Default template is the Karcher mean.
#'   * `"fda"`: continuous-criterion registration via [fda::register.fd()].
#'     Default template is the arithmetic mean.
#'   * `"affine"`: affine (linear) registration with warps of the form
#'     \eqn{h(t) = a \cdot t + b}. Simpler than elastic registration, appropriate
#'     when phase variability consists only of shifts and/or uniform scaling.
#'     Default template is the arithmetic mean.
#'   * `"landmark"`: piecewise-linear warps that align user-specified landmark
#'     features. Requires `landmarks` argument.
#'
#' @section Method-specific arguments passed via `...`:
#'
#' **For `method = "affine"`:**
#' \describe{
#'   \item{`type`}{character: `"shift"` (translation only), `"scale"` (scaling only),
#'     or `"shift_scale"` (both). Default is `"shift"`.}
#'   \item{`shift_range`}{numeric(2): bounds for shift parameter. Default is
#'     `c(-range/2, range/2)` where range is the domain width.
#'     Larger bounds allow greater shifts but may result in more `NA` values.}
#'   \item{`scale_range`}{numeric(2): bounds for scale parameter. Default is
#'     `c(0.5, 2)`. Must have `lower > 0`.}
#' }
#'
#' **For `method = "landmark"`:**
#' \describe{
#'   \item{`landmarks`}{**(required)** numeric matrix of landmark positions with
#'     one row per function and one column per landmark. Use [tf_landmarks_extrema()]
#'     to find peaks/valleys automatically.}
#'   \item{`template_landmarks`}{numeric vector of target landmark positions.
#'     Default is column-wise mean of `landmarks`.}
#' }
#'
#' @returns `tfd` vector of warping functions with the same length as `x`.
#'   Apply with [tf_unwarp()] to obtain registered functions.
#'
#' @references
#' `r format_bib("ramsay2009functional", "srivastava2011registration", "tucker2013generative")`
#' @export
#' @author Maximilian Muecke
#' @family registration functions
#'
#' @examplesIf rlang::is_installed(c("fdasrvf", "fda"))
#' # Elastic registration (SRVF method)
#' height_female <- subset(growth, gender == "female", select = height, drop = TRUE)
#' growth_female <- tf_derive(height_female) |> tfd(arg = seq(1.125, 17.8), l = 101)
#' layout(t(1:3))
#' plot(growth_female, xlab = "Chronological Age (years)", ylab = "Growth Rate (cm/year)")
#' warp <- tf_register(growth_female)
#' plot(warp, xlab = "Chronological Age", ylab = "Biological Age")
#' growth_female_reg <- tf_unwarp(growth_female, warp)
#' plot(growth_female_reg, xlab = "Biological Age (years)", ylab = "Growth Rate (cm/year)")
#'
#' @examples
#' # Affine registration (shift only)
#' t <- seq(0, 2 * pi, length.out = 101)
#' x <- tfd(t(sapply(c(-0.3, 0, 0.3), function(s) sin(t + s))), arg = t)
#' warp <- tf_register(x, method = "affine", type = "shift")
#' plot(tf_unwarp(x, warp), col = 1:3)
#'
#' # Landmark registration
#' peaks <- tf_landmarks_extrema(x, "max")
#' warp <- tf_register(x, method = "landmark", landmarks = peaks)
#' plot(tf_unwarp(x, warp), col = 1:3)
#'
tf_register <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark")
) {
  UseMethod("tf_register")
}

#' @export
tf_register.tfd_reg <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark")
) {
  assert_tfd(x)
  method <- match.arg(method)

  # Landmark method doesn't use template, uses landmarks instead

  if (method == "landmark") {
    return(register_landmark(x, ...))
  }

  # Affine method has its own simpler validation
  if (method == "affine") {
    return(register_affine(x, template = template, ...))
  }

  # SRVF/FDA methods: validate template compatibility
  rlang::check_dots_used()
  if (!is.null(template)) {
    assert_tf(template)
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

#' @export
tf_register.tfb <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark")
) {
  x |> as.tfd() |> tf_register(template = template, method = method, ...)
}

#' @export
tf_register.tfd_irreg <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark")
) {
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

# Internal: Landmark registration implementation
# Called by tf_register() with method = "landmark"
register_landmark <- function(x, landmarks, template_landmarks = NULL) {
  assert_tf(x)
  assert_matrix(landmarks, mode = "numeric", nrows = length(x), min.cols = 1)

  domain <- tf_domain(x)
  n <- length(x)
  n_landmarks <- ncol(landmarks)

  validate_landmarks(landmarks, domain, n, n_landmarks)
  template_landmarks <- validate_template_landmarks(
    template_landmarks,
    landmarks,
    domain,
    n_landmarks
  )

  # Create piecewise linear warping functions:
  # Template arg: domain boundaries + template landmarks
  # Warping values: domain boundaries + observed landmarks
  template_arg <- c(domain[1], template_landmarks, domain[2])
  warp_values <- cbind(domain[1], landmarks, domain[2])

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

#' Find Extrema Locations in Functional Data
#'
#' Helper function that finds the locations of extrema (maxima and/or minima)
#' in each function. Useful for landmark registration via [tf_register()].
#'
#' @param x a `tf` vector.
#' @param which character specifying which extrema to find: `"max"` for maxima,
#'   `"min"` for minima, or `"both"` for both (maxima first, then minima).
#' @returns A numeric matrix with one row per function containing the
#'   locations (arg values) of the specified extrema.
#' @seealso [tf_register()] with `method = "landmark"`
#' @export
#' @family registration functions
#' @examples
#' # Create functions with shifted peaks
#' t <- seq(0, 1, length.out = 101)
#' x <- tfd(t(sapply(c(0.3, 0.5, 0.7), function(s) dnorm(t, s, 0.1))), arg = t)
#' tf_landmarks_extrema(x, "max")
#' tf_landmarks_extrema(x, "both")
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

# Internal: Affine registration implementation
# Called by tf_register() with method = "affine"
#' @importFrom stats approx optim
register_affine <- function(
  x,
  template = NULL,

  type = c("shift", "scale", "shift_scale"),
  shift_range = NULL,
  scale_range = NULL
) {
  assert_tf(x)
  type <- match.arg(type)

  # Validate range parameters
  if (!is.null(shift_range)) {
    assert_numeric(shift_range, len = 2, sorted = TRUE, any.missing = FALSE)
  }
  if (!is.null(scale_range)) {
    assert_numeric(
      scale_range,
      len = 2,
      lower = 1e-10,
      sorted = TRUE,
      any.missing = FALSE
    )
  }

  domain <- tf_domain(x)
  template <- validate_affine_template(template, x, domain)

  # Get optimization configuration for this type
  config <- affine_config(type, domain, shift_range, scale_range)

  # Prepare data matrices on common grid
  arg <- tf_arg(x)
  if (is.list(arg)) arg <- sort_unique(arg, simplify = TRUE)

  x_mat <- as.matrix(tfd(x, arg = arg))
  template_vec <- as.matrix(tfd(template, arg = arg))[1, ]

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
# True affine warps: h(s) = a*s + b with admissible bounds
affine_config <- function(
  type,
  domain,
  shift_range = NULL,
  scale_range = NULL
) {
  lwr <- domain[1]
  upr <- domain[2]
  range <- upr - lwr
  center <- (lwr + upr) / 2

  # Default bounds
  shift_lower <- shift_range[1] %||% (-range / 2)
  shift_upper <- shift_range[2] %||% (range / 2)
  scale_lower <- scale_range[1] %||% 0.5
  scale_upper <- scale_range[2] %||% 2.0

  switch(
    type,
    shift = list(
      type = "shift",
      init = 0,
      lower = shift_lower,
      upper = shift_upper,
      center = center
    ),
    scale = list(
      type = "scale",
      init = 1,
      lower = scale_lower,
      upper = scale_upper,
      center = center
    ),
    shift_scale = list(
      type = "shift_scale",
      init = c(1, 0),
      lower = c(scale_lower, shift_lower),
      upper = c(scale_upper, shift_upper),
      center = center
    )
  )
}

# Helper: optimize affine warp for one curve
# Returns a true affine warp h(s) = a*s + b that achieves the best alignment
optimize_affine_warp <- function(x_vec, template_vec, arg, domain, config) {
  # Objective: L2 distance after warping
  # Use rule=2 (extrapolate) for optimizer to work smoothly
  # Final tf_unwarp will use rule=1 (NA) for proper incomplete functions
  objective <- function(params) {
    ab <- extract_affine_params(params, config)
    warp_values <- create_affine_warp(ab$a, ab$b, arg)

    # Evaluate x at warped positions (extrapolate for optimization)
    x_warped <- approx(arg, x_vec, xout = warp_values, rule = 2)$y
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
  create_affine_warp(ab$a, ab$b, arg)
}

# Helper: create true affine warp h(s) = a*s + b
# No clamping - warp values can extend outside domain
create_affine_warp <- function(a, b, arg) {
  a * arg + b
}

# Helper: extract (a, b) parameters from optimizer output
# For scale type, b is computed to center scaling around domain midpoint
extract_affine_params <- function(params, config) {
  switch(
    config$type,
    shift = list(a = 1, b = params[1]),
    scale = list(a = params[1], b = config$center * (1 - params[1])),
    shift_scale = list(a = params[1], b = params[2])
  )
}
