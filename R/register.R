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
  warp <- coerce_warp_to_tfd(warp)
  assert_warp(warp, x)
  UseMethod("tf_warp")
}

coerce_warp_to_tfd <- function(warp) {
  if (is_tfb(warp)) {
    return(as.tfd(warp))
  }
  warp
}

strictify_boundary_ties <- function(values, domain, tol_abs) {
  n_values <- length(values)
  if (n_values <= 1) {
    return(values)
  }

  diffs <- diff(values)
  is_increasing <- all(diffs >= 0)
  is_decreasing <- all(diffs <= 0)
  if (!is_increasing && !is_decreasing) {
    return(values)
  }

  tie_pos <- which(diffs == 0)
  if (length(tie_pos) == 0) {
    return(values)
  }
  near_boundary <- abs(values - domain[1]) <= tol_abs |
    abs(values - domain[2]) <= tol_abs
  tie_near_boundary <- near_boundary[tie_pos] | near_boundary[tie_pos + 1]
  if (!all(tie_near_boundary)) {
    return(values)
  }

  domain_span <- diff(domain)
  if (domain_span <= 0) {
    return(values)
  }
  eps <- min(tol_abs, domain_span / (max(1, n_values - 1) * 2))
  if (eps <= 0) {
    return(values)
  }

  adjusted <- if (is_increasing) {
    cummax(values) + eps * seq.int(0, n_values - 1)
  } else {
    -cummax(-values) - eps * seq.int(0, n_values - 1)
  }

  shift_min <- domain[1] - min(adjusted)
  shift_max <- domain[2] - max(adjusted)
  if (shift_min <= shift_max) {
    shift <- min(max(0, shift_min), shift_max)
    return(adjusted + shift)
  }

  if (is_increasing) {
    return(seq(domain[1], domain[2], length.out = n_values))
  }
  seq(domain[2], domain[1], length.out = n_values)
}

stabilize_warp_values <- function(
  values,
  domain,
  tol = sqrt(.Machine$double.eps)
) {
  tol_abs <- max(1, diff(domain)) * tol
  values[values < domain[1] & values >= domain[1] - tol_abs] <- domain[1]
  values[values > domain[2] & values <= domain[2] + tol_abs] <- domain[2]
  values <- strictify_boundary_ties(values, domain = domain, tol_abs = tol_abs)
  values
}

apply_tfb_warp <- function(fun, x, warp, dots = list()) {
  # keep_new_arg forced to FALSE here, otherwise basis matrix blows up:
  # would keep every unique gridpoint & cause plots to fail (resolution too small)
  warp <- coerce_warp_to_tfd(warp)
  if (isTRUE(dots$keep_new_arg)) {
    cli::cli_warn(
      "{.arg keep_new_arg} reset to FALSE - not applicable for {.cls tfb}."
    )
    dots$keep_new_arg <- FALSE
  }
  args <- c(list(x = as.tfd(x), warp = warp), dots)
  do.call(fun, args) |> tf_rebase(x)
}

is_non_domain_preserving_warp <- function(warp_evals, domain) {
  any(map_lgl(warp_evals, \(warp_vals) {
    finite_vals <- warp_vals[is.finite(warp_vals)]
    if (length(finite_vals) == 0) {
      return(TRUE)
    }
    warp_min <- min(finite_vals)
    warp_max <- max(finite_vals)
    # Check for expansion OR shrinkage
    warp_min < domain[1] ||
      warp_max > domain[2] ||
      warp_min > domain[1] + 1e-10 ||
      warp_max < domain[2] - 1e-10
  }))
}

unwarp_non_domain_preserving <- function(
  arg_list,
  x_evals,
  warp_evals,
  domain,
  evaluator_name
) {
  # Build (arg, value) pairs for each function, keeping only valid points
  valid_data <- pmap(
    list(arg_list, x_evals, warp_evals),
    \(arg_i, x_vals, warp_vals) {
      # warp_vals = h(arg_i), we want x(h(arg_i)) at each arg point
      # Use rule=1 to get NA where warp goes outside original domain
      reg_vals <- approx(arg_i, x_vals, xout = warp_vals, rule = 1)$y
      valid <- !is.na(reg_vals)
      list(arg = arg_i[valid], value = reg_vals[valid])
    }
  )

  # Create irregular tfd with only valid points
  new_tfd(
    arg = map(valid_data, "arg"),
    datalist = map(valid_data, "value"),
    regular = FALSE,
    domain = domain,
    evaluator = evaluator_name
  )
}

unwarp_domain_preserving <- function(x_evals, warp_evals, arg_list, domain) {
  inv_warp <- tfd(warp_evals, arg = arg_list, domain = domain) |>
    tf_invert(domain = domain) |>
    tfd(arg = arg_list, domain = domain)
  inv_warp_evals <- tf_evaluations(inv_warp) |>
    map(\(vals) stabilize_warp_values(vals, domain))
  tfd(x_evals, arg = inv_warp_evals, domain = domain)
}

#' @rdname tf_warp
#' @export
tf_warp.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
  assert_flag(keep_new_arg)
  arg <- tf_arg(x)
  domain <- tf_domain(x)
  warp <- tfd(warp, arg = arg)
  warp_evals <- map(
    tf_evaluations(warp),
    \(vals) stabilize_warp_values(vals, domain)
  )
  ret <- tfd(tf_evaluations(x), warp_evals, domain = domain, ...)

  if (!keep_new_arg) {
    ret <- tfd(ret, arg = arg, ...)
  }
  ret
}
#' @rdname tf_warp
#' @export
tf_warp.tfb <- function(x, warp, ...) {
  apply_tfb_warp(tf_warp, x = x, warp = warp, dots = list(...))
}

#-------------------------------------------------------------------------------

#' @rdname tf_warp
#' @export
tf_unwarp <- function(x, warp, ...) {
  rlang::check_dots_used()
  warp <- coerce_warp_to_tfd(warp)
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

  domain <- tf_domain(x)
  arg <- tf_arg(x)
  arg_list <- ensure_list(arg)
  if (length(x) > 1 && length(arg_list) == 1) {
    arg_list <- rep(arg_list, length(x))
  }
  warp <- tfd(warp, arg = arg_list)

  x_evals <- tf_evaluations(x)
  warp_evals <- tf_evaluations(warp) |>
    map(\(vals) stabilize_warp_values(vals, domain))

  # Check if warp is NOT domain-preserving (true affine warps are not)

  # Domain-preserving means h(lwr) = lwr and h(upr) = upr (bijection on domain)
  # Non-domain-preserving includes:
  #   - Expansion: warp values extend OUTSIDE domain (shift)
  #   - Shrinkage: warp values stay in PROPER SUBSET (scale with a < 1)
  is_non_domain_preserving <- is_non_domain_preserving_warp(warp_evals, domain)

  if (is_non_domain_preserving) {
    # For warps that go outside the domain (true affine warps), compute x(h(t))
    # The optimizer finds h such that x(h(s)) ≈ template(s)
    # Create irregular tfd with only valid points, then re-evaluate on grid
    evaluator_name <- attr(x, "evaluator_name") %||% "tf_approx_linear"
    ret <- unwarp_non_domain_preserving(
      arg_list = arg_list,
      x_evals = x_evals,
      warp_evals = warp_evals,
      domain = domain,
      evaluator_name = evaluator_name
    )

    if (!keep_new_arg) {
      # Re-evaluate on regular grid (evaluator returns NA outside valid range)
      ret <- tfd(ret, arg = arg, ...)
    }
    return(ret)
  } else {
    # Original approach for domain-preserving warps
    ret <- unwarp_domain_preserving(
      x_evals = x_evals,
      warp_evals = warp_evals,
      arg_list = arg_list,
      domain = domain
    )
    if (!keep_new_arg) {
      ret <- tfd(ret, arg = arg, ...)
    }
    return(ret)
  }
}
#' @rdname tf_warp
#' @export
tf_unwarp.tfb <- function(x, warp, ...) {
  apply_tfb_warp(tf_unwarp, x = x, warp = warp, dots = list(...))
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
  has_na <- anyNA(landmarks)

  # Validate non-NA landmarks only
  validate_landmarks(landmarks, domain, n, n_landmarks)
  template_landmarks <- validate_template_landmarks(
    template_landmarks,
    landmarks,
    domain,
    n_landmarks
  )

  if (!has_na) {
    # Fast path: all landmarks present, vectorized construction
    template_arg <- c(domain[1], template_landmarks, domain[2])
    warp_values <- cbind(domain[1], landmarks, domain[2])
    return(tfd(warp_values, arg = template_arg))
  }

  # NA-aware path: build per-curve warps using only available landmarks
  arg <- as.numeric(tf_arg(x))
  warp_list <- lapply(seq_len(n), \(i) {
    valid <- !is.na(landmarks[i, ])
    t_arg <- c(domain[1], template_landmarks[valid], domain[2])
    w_vals <- c(domain[1], landmarks[i, valid], domain[2])
    approx(t_arg, w_vals, xout = arg, rule = 2)$y
  })
  tfd(do.call(rbind, warp_list), arg = arg)
}

# Helper: validate landmark matrix
validate_landmarks <- function(landmarks, domain, n, n_landmarks) {
  # Check strictly increasing within each row (skip NAs)
  for (i in seq_len(n)) {
    row_vals <- landmarks[i, !is.na(landmarks[i, ])]
    if (length(row_vals) > 1 && !all(diff(row_vals) > 0)) {
      cli::cli_abort(
        "Landmarks must be strictly increasing within each row. Problem at row {i}."
      )
    }
  }
  # Check strictly inside domain (skip NAs).
  # Landmarks at exact domain boundaries would create duplicate knots when

  # boundaries are appended in register_landmark().
  lm_vals <- landmarks[!is.na(landmarks)]
  if (
    length(lm_vals) > 0 &&
      (any(lm_vals <= domain[1]) || any(lm_vals >= domain[2]))
  ) {
    cli::cli_abort(c(
      "All landmarks must be strictly inside the domain ({domain[1]}, {domain[2]}).",
      "i" = "Boundary landmarks are redundant with the domain anchors."
    ))
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
    return(colMeans(landmarks, na.rm = TRUE))
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
#' Find landmark locations for registration
#'
#' Detects local maxima, minima, and/or zero crossings in each function and
#' returns a landmark matrix suitable for [tf_register()] with
#' `method = "landmark"`. Uses position-based clustering across curves to
#' establish feature correspondence and majority-count filtering to discard
#' unstable landmarks.
#'
#' @param x a `tf` vector.
#' @param which character: which features to detect. Either `"all"` (maxima,
#'   minima, and zero crossings), `"both"` (maxima and minima), or any subset
#'   of `c("max", "min", "zero")`.
#' @param smooth logical: if `TRUE` (default for `tfd`), smooth curves with
#'   [tf_smooth()] before feature detection to suppress spurious noise peaks.
#' @param threshold numeric in (0, 1]: minimum proportion of curves that must
#'   contain a feature for it to be retained as a landmark. Defaults to `0.5`.
#' @param boundary_tol numeric: features within this distance of the domain
#'   boundary are dropped (they are redundant with the boundary anchors in
#'   landmark registration). Defaults to 2x the grid spacing. Set to `0` to
#'   keep all features.
#' @param ... additional arguments passed to [tf_smooth()].
#' @returns A numeric matrix with one row per function and one column per
#'   landmark, sorted left-to-right on the domain. Has attribute
#'   `"feature_types"` (character vector of `"max"`, `"min"`, or `"zero"` for
#'   each column). Contains `NA` where a curve is missing a landmark.
#' @seealso [tf_register()] with `method = "landmark"`
#' @export
#' @family registration functions
#' @examples
#' t <- seq(0, 1, length.out = 101)
#' x <- tfd(t(sapply(c(0.3, 0.5, 0.7), function(s) dnorm(t, s, 0.1))), arg = t)
#' tf_landmarks_extrema(x, "max")
#' tf_landmarks_extrema(x, "both")
tf_landmarks_extrema <- function(
  x,
  which = "all",
  smooth = TRUE,
  threshold = 0.5,
  boundary_tol = NULL,
  ...
) {
  assert_tf(x)
  assert_number(threshold, lower = 0, upper = 1)
  if (!is.null(boundary_tol)) assert_number(boundary_tol, lower = 0)
  n <- length(x)

  # Parse `which`
  if (identical(which, "all")) {
    which <- c("max", "min", "zero")
  } else if (identical(which, "both")) {
    which <- c("max", "min")
  }
  assert_subset(which, c("max", "min", "zero"))

  # Smooth tfd to suppress noise-induced spurious features
  x_proc <- x
  if (smooth && inherits(x, "tfd")) {
    x_proc <- tf_smooth(x, verbose = FALSE, ...)
  }

  domain <- tf_domain(x_proc)
  # Get per-curve arg grids for feature detection
  arg_list <- tf_arg(x_proc)
  if (!is.list(arg_list)) {
    # Regular tfd: single shared grid → replicate for uniform interface
    arg_list <- rep(list(as.numeric(arg_list)), n)
  }
  # Representative grid spacing (for bandwidth/boundary defaults)
  grid_spacing <- median(vapply(arg_list, \(a) median(diff(a)), numeric(1)))

  if (is.null(boundary_tol)) {
    boundary_tol <- 2 * grid_spacing
  }

  # --- Detect features per curve (each on its own grid) ---
  features <- detect_landmarks(x_proc, arg_list, which)

  # --- Drop boundary features ---
  features <- lapply(features, \(f) {
    if (nrow(f) == 0) return(f)
    interior <- f$position > (domain[1] + boundary_tol) &
      f$position < (domain[2] - boundary_tol)
    f[interior, , drop = FALSE]
  })

  # --- Cluster features across curves ---
  # Bandwidth controls how far apart features (across different curves) can be
  # and still be considered the same landmark. Default: 15% of domain range,
  # which handles moderate warping. Increase for heavily warped data.
  bandwidth <- max(5 * grid_spacing, 0.15 * diff(domain))
  clusters <- cluster_landmarks(features, n, bandwidth, threshold)

  if (nrow(clusters) == 0) {
    cli::cli_warn("No stable landmarks detected across curves.")
    lm_mat <- matrix(NA_real_, nrow = n, ncol = 0)
    attr(lm_mat, "feature_types") <- character(0)
    return(lm_mat)
  }

  # --- Build landmark matrix ---
  lm_mat <- build_landmark_matrix(features, clusters, n, bandwidth)

  # --- Check for NAs ---
  n_na <- sum(is.na(lm_mat))
  if (n_na > 0) {
    na_per_col <- colSums(is.na(lm_mat))
    cols_with_na <- which(na_per_col > 0)
    cli::cli_warn(c(
      "{n_na} missing landmark{?s} across {length(cols_with_na)} column{?s}.",
      "i" = "Some curves lack features near {round(clusters$center[cols_with_na], 3)}."
    ))
  }

  lm_mat
}

# --- Landmark Detection Helpers ------------------------------------------------

#' Detect local extrema and zero crossings per curve
#' @param x tf object (already smoothed if needed)
#' @param arg_list list of numeric vectors: per-curve evaluation grids
#' @param which character vector: subset of c("max", "min", "zero")
#' @returns list of n data.frames with columns (position, type)
#' @keywords internal
detect_landmarks <- function(x, arg_list, which) {
  x_evals <- tf_evaluations(x)
  n <- length(x)
  need_extrema <- any(c("max", "min") %in% which)
  need_zero <- "zero" %in% which

  lapply(seq_len(n), \(i) {
    vals <- x_evals[[i]]
    arg_i <- arg_list[[i]]
    positions <- numeric(0)
    types <- character(0)

    if (need_extrema) {
      dv <- diff(vals)
      # Compare sign of consecutive differences to find local extrema.
      # Local max: slope positive then negative (dv[j] > 0, dv[j+1] < 0).
      # Local min: slope negative then positive (dv[j] < 0, dv[j+1] > 0).
      # Extremum is at arg_i[j + 1] (the middle point of the 3-point window).
      for (j in seq_len(length(dv) - 1)) {
        if ("max" %in% which && dv[j] > 0 && dv[j + 1] < 0) {
          positions <- c(positions, arg_i[j + 1])
          types <- c(types, "max")
        }
        if ("min" %in% which && dv[j] < 0 && dv[j + 1] > 0) {
          positions <- c(positions, arg_i[j + 1])
          types <- c(types, "min")
        }
      }
    }

    if (need_zero) {
      for (j in seq_len(length(vals) - 1)) {
        if (vals[j] * vals[j + 1] < 0) {
          # Strict sign change: interpolate zero position
          pos <- arg_i[j] -
            vals[j] * (arg_i[j + 1] - arg_i[j]) / (vals[j + 1] - vals[j])
          positions <- c(positions, pos)
          types <- c(types, "zero")
        } else if (
          vals[j] == 0 &&
            j > 1 &&
            j < length(vals) - 1 &&
            sign(vals[j - 1]) != sign(vals[j + 1]) &&
            vals[j - 1] != 0 &&
            vals[j + 1] != 0
        ) {
          # Exact zero at grid point with sign change around it
          positions <- c(positions, arg_i[j])
          types <- c(types, "zero")
        }
      }
    }

    ord <- order(positions)
    data.frame(
      position = positions[ord],
      type = types[ord],
      stringsAsFactors = FALSE
    )
  })
}

#' Cluster detected features across curves by position
#'
#' Clusters within each feature type separately (max with max, min with min,
#' etc.) to avoid merging adjacent features of different types. Then combines
#' and sorts by position.
#'
#' @param features list of per-curve data.frames from detect_landmarks()
#' @param n number of curves
#' @param bandwidth merge distance for clustering
#' @param threshold minimum proportion of curves for a cluster to be retained
#' @returns data.frame with columns: center, type, count, proportion
#' @keywords internal
cluster_landmarks <- function(features, n, bandwidth, threshold) {
  # Pool all features with curve ID
  all_f <- do.call(
    rbind,
    lapply(seq_along(features), \(i) {
      f <- features[[i]]
      if (nrow(f) == 0) return(NULL)
      data.frame(
        position = f$position,
        type = f$type,
        curve = i,
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(all_f) || nrow(all_f) == 0) {
    return(data.frame(
      center = numeric(0),
      type = character(0),
      count = integer(0),
      proportion = numeric(0)
    ))
  }

  # Cluster within each feature type separately, then combine
  feature_types <- unique(all_f$type)
  all_clusters <- list()

  for (ftype in feature_types) {
    sub <- all_f[all_f$type == ftype, , drop = FALSE]
    sub <- sub[order(sub$position), ]
    if (nrow(sub) == 0) next

    # Greedy clustering: merge features within bandwidth of cluster center
    cur <- list(positions = sub$position[1], curves = sub$curve[1])

    for (i in seq_len(nrow(sub))[-1]) {
      if (sub$position[i] - mean(cur$positions) <= bandwidth) {
        cur$positions <- c(cur$positions, sub$position[i])
        cur$curves <- c(cur$curves, sub$curve[i])
      } else {
        all_clusters[[length(all_clusters) + 1]] <- list(
          center = mean(cur$positions),
          type = ftype,
          count = length(unique(cur$curves))
        )
        cur <- list(positions = sub$position[i], curves = sub$curve[i])
      }
    }
    all_clusters[[length(all_clusters) + 1]] <- list(
      center = mean(cur$positions),
      type = ftype,
      count = length(unique(cur$curves))
    )
  }

  result <- data.frame(
    center = vapply(all_clusters, `[[`, numeric(1), "center"),
    type = vapply(all_clusters, `[[`, character(1), "type"),
    count = vapply(all_clusters, `[[`, integer(1), "count"),
    stringsAsFactors = FALSE
  )
  result$proportion <- result$count / n

  # Filter by threshold, then sort by position
  result <- result[result$proportion >= threshold, , drop = FALSE]
  result[order(result$center), , drop = FALSE]
}

#' Build landmark matrix by matching per-curve features to clusters
#' @param features list of per-curve data.frames
#' @param clusters data.frame from cluster_landmarks()
#' @param n number of curves
#' @param bandwidth matching distance
#' @returns n x k matrix with feature_types attribute
#' @keywords internal
build_landmark_matrix <- function(features, clusters, n, bandwidth) {
  k <- nrow(clusters)
  lm_mat <- matrix(NA_real_, nrow = n, ncol = k)

  for (i in seq_len(n)) {
    f <- features[[i]]
    if (nrow(f) == 0) next
    used <- logical(nrow(f))
    for (j in seq_len(k)) {
      # Find features of matching type within bandwidth, not yet assigned
      matches <- which(
        abs(f$position - clusters$center[j]) <= bandwidth &
          f$type == clusters$type[j] &
          !used
      )
      if (length(matches) > 0) {
        best <- matches[which.min(abs(
          f$position[matches] - clusters$center[j]
        ))]
        lm_mat[i, j] <- f$position[best]
        used[best] <- TRUE
      }
    }
  }

  attr(lm_mat, "feature_types") <- clusters$type
  lm_mat
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
    cli::cli_abort("{.arg template} must be of length 1.")
  }
  if (!all(domain == tf_domain(template))) {
    cli::cli_abort("{.arg x} and {.arg template} must have the same domain.")
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
