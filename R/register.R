#' Elastic Deformation: warp and align `tf` vectors
#'
#' @description
#' These functions stretch and/or compress regions of the domain of functional data:
#'
#' - `tf_warp()` applies warping functions to aligned (registered) functional data
#' to recover the original unregistered curves: \eqn{x(s) \to x(h(s)) = x(t)}.
#' - `tf_align()` applies the *inverse* warping function to unregistered data
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
#'   registered/aligned functions and unaligned functions for `tf_align()`.
#' @param warp `tf` vector of warping functions used for transformation. See Details.
#' @param ... additional arguments passed to [tfd()].
#' @param keep_new_arg keep new `arg` values after (un)warping or return
#'   `tfd` vector on `arg` values of the input (default `FALSE` is the latter)? See Details.
#' @returns
#' * `tf_warp()`: the warped `tf` vector (un-registered functions)
#' * `tf_align()`: the aligned `tf` vector (registered functions)
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
#' if (requireNamespace("fdasrvf", quietly = TRUE)) {
#'   reg <- tf_register(x)
#' } else {
#'   reg <- tf_register(x, method = "affine", type = "shift_scale")
#' }
#' layout(t(1:3))
#' plot(x, col = 1:5)
#' plot(tf_inv_warps(reg), col = 1:5); lines(tf_invert(warp), lty = 3, lwd = 1.5, col = 1:5)
#' plot(tf_aligned(reg), col = 1:5, points = FALSE); lines(template, lty = 2)
#' @export
#' @author Maximilian Muecke, Fabian Scheipl, Claude Opus 4.6
#' @family registration functions
tf_warp <- function(x, warp, ...) {
  rlang::check_dots_used()
  warp <- coerce_warp_to_tfd(warp)
  assert_warp(warp, x)
  UseMethod("tf_warp")
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

#' Apply warping functions to align functional data
#'
#' `tf_align()` applies the *inverse* warping function to unregistered data
#' to obtain aligned (registered) functions.
#'
#' @inheritParams tf_warp
#' @returns the aligned `tf` vector (registered functions)
#' @family registration functions
#' @examples
#' # Estimate warps, then align manually:
#' t <- seq(0, 2 * pi, length.out = 101)
#' x <- tfd(t(sapply(c(-0.3, 0, 0.3), function(s) sin(t + s))), arg = t)
#' warps <- tf_estimate_warps(x, method = "affine", type = "shift")
#' aligned <- tf_align(x, warps)
#' plot(aligned, col = 1:3)
#' @export
tf_align <- function(x, warp, ...) {
  rlang::check_dots_used()
  warp <- coerce_warp_to_tfd(warp)
  assert_warp(warp, x)
  UseMethod("tf_align")
}
#' @rdname tf_align
#' @export
tf_align.tfd <- function(x, warp, ..., keep_new_arg = FALSE) {
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
#' @rdname tf_align
#' @export
tf_align.tfb <- function(x, warp, ...) {
  apply_tfb_warp(tf_align, x = x, warp = warp, dots = list(...))
}

#-------------------------------------------------------------------------------

#' Register / align a `tf` vector against a template function
#'
#' `tf_register()` is the high-level entry point for functional data registration.
#' It estimates warping functions, applies them to align the data, and returns a
#' [tf_registration] result object containing the aligned curves, inverse
#' warping functions (observed to aligned time), and template. Use
#' [tf_aligned()], [tf_inv_warps()], and [tf_template()] to extract components.
#'
#' For a lower-level interface that returns only warping functions (without
#' performing alignment), see [tf_estimate_warps()].
#'
#' @param x a `tf` vector of functions to register.
#' @param ... additional method-specific arguments passed to backend routines
#'   (for example `crit` for `method = "fda"`). See [tf_estimate_warps()] for
#'   method-specific argument documentation.
#' @param template an optional `tf` vector of length 1 to use as the template.
#'   If `NULL`, a default template is computed (method-dependent).
#'   Not used for `method = "landmark"`.
#' @param method the registration method to use:
#'   * `"srvf"`: Square Root Velocity Framework (elastic registration).
#'   * `"fda"`: continuous-criterion registration via a tf-native dense-grid
#'     optimizer with monotone spline warps.
#'   * `"affine"`: affine (linear) registration.
#'   * `"landmark"`: piecewise-linear warps aligning user-specified landmarks.
#' @param max_iter integer: maximum Procrustes-style template refinement
#'   iterations. Default `3L`.
#' @param tol numeric: convergence tolerance for template refinement.
#'   Default `1e-2`.
#' @param store_x logical: store original data in the result object?
#'   Default `TRUE`. Set to `FALSE` to save memory.
#' @inheritSection tf_estimate_warps Important method-specific arguments (passed via `...`)
#' @returns A [tf_registration] object. Access components with
#'   [tf_aligned()], [tf_inv_warps()], [tf_template()].
#'
#' @references
#' Ramsay JO, Hooker G, Graves S (2009).
#' *Functional Data Analysis with R and MATLAB.* Springer, New York.
#' \doi{10.1007/978-0-387-98185-7}.
#'
#' Srivastava A, Wu W, Kurtek S, Klassen E, Marron JS (2011).
#' "Registration of Functional Data Using Fisher-Rao Metric."
#' *arXiv:1103.3817.*
#'
#' Tucker JD, Wu W, Srivastava A (2013).
#' "Generative models for functional data using phase and amplitude separation."
#' *Computational Statistics & Data Analysis*, **61**, 50--66.
#' \doi{10.1016/j.csda.2012.12.001}.
#' @export
#' @author Maximilian Muecke, Fabian Scheipl, Claude Opus 4.6
#' @family registration functions
#'
#' @examplesIf rlang::is_installed(c("fdasrvf", "fda"))
#' # Elastic registration (SRVF method)
#' height_female <- subset(growth, gender == "female", select = height, drop = TRUE)
#' growth_female <- tf_derive(height_female) |> tfd(arg = seq(1.125, 17.8), l = 101)
#' reg <- tf_register(growth_female)
#' layout(t(1:3))
#' plot(growth_female, xlab = "Chronological Age", ylab = "Growth Rate (cm/year)")
#' plot(tf_inv_warps(reg), xlab = "Chronological Age", ylab = "Biological Age")
#' plot(tf_aligned(reg), xlab = "Biological Age", ylab = "Growth Rate (cm/year)")
#'
#' @examples
#' # Affine registration (shift only)
#' t <- seq(0, 2 * pi, length.out = 101)
#' x <- tfd(t(sapply(c(-0.3, 0, 0.3), function(s) sin(t + s))), arg = t)
#' reg <- tf_register(x, method = "affine", type = "shift")
#' plot(tf_aligned(reg), col = 1:3)
#'
#' # Landmark registration
#' peaks <- tf_landmarks_extrema(x, "max")
#' reg <- tf_register(x, method = "landmark", landmarks = peaks)
#' plot(tf_aligned(reg), col = 1:3)
#'
tf_register <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2,
  store_x = TRUE
) {
  cl <- match.call()
  method <- match.arg(method)
  assert_tf(x)
  assert_flag(store_x)
  warps <- tf_estimate_warps(
    x,
    ...,
    template = template,
    method = method,
    max_iter = max_iter,
    tol = tol
  )
  registered <- tf_align(x, warps)
  tmpl <- attr(warps, "template") %||%
    (if (method != "landmark") template) %||%
    suppressWarnings(mean(registered))
  new_tf_registration(
    registered = registered,
    inv_warps = tf_invert(warps),
    template = tmpl,
    x = if (store_x) x else NULL,
    call = cl
  )
}

#-------------------------------------------------------------------------------

registration_objective_fda <- function(aligned, template, crit = 2L) {
  aligned_mat <- as.matrix(aligned)
  template_mat <- as.matrix(template)
  if (nrow(template_mat) == 1L && nrow(aligned_mat) > 1L) {
    template_mat <- template_mat[rep(1L, nrow(aligned_mat)), , drop = FALSE]
  }

  total <- 0
  for (i in seq_len(nrow(aligned_mat))) {
    y0 <- template_mat[i, ]
    y1 <- aligned_mat[i, ]
    aa <- mean(y0^2)
    bb <- mean(y0 * y1)
    cc <- mean(y1^2)
    total <- total +
      if (crit == 1L) {
        aa - 2 * bb + cc
      } else {
        aa + cc - sqrt((aa - cc)^2 + 4 * bb^2)
      }
  }
  total
}

outer_registration_objective <- function(method, aligned, template, arg, dots) {
  if (method == "fda") {
    return(registration_objective_fda(
      aligned = aligned,
      template = template,
      crit = dots$crit %||% 2L
    ))
  }

  domain_length <- diff(tf_domain(aligned))
  suppressWarnings(mean(
    tf_integrate((aligned - template)^2, arg = arg) / domain_length,
    na.rm = TRUE
  ))
}

#-------------------------------------------------------------------------------

#' Estimate warping functions for registration
#'
#' `tf_estimate_warps()` is the low-level workhorse for functional data
#' registration. It estimates warping functions that align a set of functions to
#' a template, but does *not* apply them. For a one-shot interface that also
#' aligns the data, see [tf_register()].
#'
#' For `method = "fda"`, `tf` uses a tf-native dense-grid optimizer with
#' monotone spline warps. Each warp is represented as the normalized cumulative
#' integral of `exp(eta(t))`, where `eta(t)` is a spline with `nbasis`
#' coefficients. Registration is then carried out curve-by-curve by minimizing
#' either an integrated squared-error criterion (`crit = 1`) or the
#' first-eigenfunction variance criterion (`crit = 2`) plus an optional spline
#' roughness penalty (`lambda`). The outer `max_iter` loop, when `template =
#' NULL`, still performs the same Procrustes-style template refinement as the
#' other methods.
#'
#' @param x a `tf` vector of functions to register.
#' @param ... additional method-specific arguments passed to backend routines
#'   (for example `crit` for `method = "fda"`).
#' @param template an optional `tf` vector of length 1 to use as the template.
#'   If `NULL`, a default template is computed (method-dependent).
#'   Not used for `method = "landmark"`.
#' @param method the registration method to use:
#'   * `"srvf"`: Square Root Velocity Framework (elastic registration).
#'     For details, see [fdasrvf::time_warping()]. Default template is the Karcher mean.
#'   * `"fda"`: continuous-criterion registration via a tf-native dense-grid
#'     optimizer with monotone spline warps. Default template is the arithmetic
#'     mean.
#'   * `"affine"`: affine (linear) registration with warps of the form
#'     \eqn{h(t) = a \cdot t + b}. Simpler than elastic registration, appropriate
#'     when phase variability consists only of shifts and/or uniform speed-up/slow-down.
#'     Default template is the arithmetic mean.
#'   * `"landmark"`: piecewise-linear warps that align user-specified landmark
#'     features. Requires `landmarks` argument.
#' @param max_iter integer: maximum number of Procrustes-style template
#'   refinement iterations when `template = NULL`. The iteration cycle is:
#'   (1) estimate template as mean of (aligned) curves, (2) register all curves
#'   to current template, (3) update template as mean of newly aligned curves,
#'   (4) repeat until convergence or `max_iter` reached.
#'   Ignored when `template` is provided (no refinement needed) or for
#'   `method = "landmark"` (template not used).
#'   For `method = "srvf"` with `template = NULL`, the outer Procrustes loop
#'   is skipped regardless of `max_iter` because [fdasrvf::time_warping()]
#'   already computes the Karcher mean internally.
#'   Default is `3L`.
#' @param tol numeric: convergence tolerance for template refinement. For
#'   `method = "fda"`, iteration stops when the relative improvement in the
#'   registration criterion becomes negligible; for the other iterative methods,
#'   iteration stops when the relative change in the template (L2 norm) falls
#'   below `tol`. Default is `1e-2`.
#'
#' @section Important method-specific arguments (passed via `...`):
#'
#' **For `method = "srvf"`:**
#' \describe{
#' \item{`lambda`}{non-negative number: penalty controlling the flexibility of
#' warpings (default is `0` for unrestricted warps).}
#' \item{`penalty_method`}{cost function used to penalize warping functions.
#' Defaults to `"roughness"` (norm of their second derivative),
#' `"geodesic"` uses the geodesic distance to the identity and `"norm"` uses
#' Euclidean distance to the identity.} }
#'
#' **For `method = "fda"`:**
#' \describe{
#'   \item{`nbasis`}{integer: number of B-spline basis functions for the monotone
#'     warp basis (default `6L`, minimum 2).}
#'   \item{`lambda`}{non-negative number: roughness penalty for the warp basis
#'     (default `0` for unpenalized warping).}
#'   \item{`crit`}{registration criterion. Defaults to `2` for the
#'     first-eigenfunction variance criterion; alternative is `1` for
#'     integrated squared error.}
#'   \item{`conv`}{non-negative convergence tolerance for the inner optimizer.
#'     Default is `1e-4`.}
#'   \item{`iterlim`}{maximum number of inner optimization iterations per curve.
#'     Default is `20L`.}
#' }
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
#' @returns `tfd` vector of (forward) warping functions \eqn{h_i(s) = t}
#'   with the same length as `x`.
#'   Apply with [tf_align()] to obtain registered functions, or use
#'   [tf_invert()] to obtain inverse warps \eqn{h_i^{-1}(t) = s}.
#'   The returned warps carry an `attr(, "template")` with the template used
#'   (`NULL` for landmark registration, which has no template).
#'
#' @export
#' @author Maximilian Muecke, Fabian Scheipl, Claude Opus 4.6
#' @family registration functions
tf_estimate_warps <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2
) {
  UseMethod("tf_estimate_warps")
}

#' @export
tf_estimate_warps.tfd_reg <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2
) {
  dots <- list(...)
  assert_tfd(x)
  method <- match.arg(method)
  assert_count(max_iter, positive = TRUE)
  assert_number(tol, lower = 0)

  # Landmark method doesn't use template, uses landmarks instead
  if (method == "landmark") {
    warps <- do.call(tf_register_landmark, c(list(x = x), dots))
    attr(warps, "template") <- NULL
    return(warps)
  }

  # SRVF with no template: Karcher mean handles iteration internally
  # (skip outer Procrustes loop regardless of max_iter)
  if (method == "srvf" && is.null(template)) {
    return(do.call(tf_register_srvf, c(list(x = x, template = NULL), dots)))
  }

  # Initial template
  if (is.null(template)) {
    current_template <- mean(x)
  } else {
    current_template <- template
    max_iter <- 1L # no refinement when template is given
  }

  # Validate user-supplied template for SRVF/FDA
  if (!is.null(template) && method %in% c("srvf", "fda")) {
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

  # Iterative registration (single pass when max_iter=1 or template given)
  arg <- tf_arg(x)
  best_warps <- NULL
  best_template <- current_template
  best_obj <- Inf
  prev_obj <- NA_real_
  fda_min_iter <- 3L
  for (iter in seq_len(max_iter)) {
    warps <- switch(
      method,
      srvf = do.call(
        tf_register_srvf,
        c(list(x = x, template = current_template), dots)
      ),
      fda = do.call(
        tf_register_fda,
        c(list(x = x, template = current_template), dots)
      ),
      affine = do.call(
        tf_register_affine,
        c(list(x = x, template = current_template), dots)
      )
    )

    aligned <- tf_align(x, warps)
    aligned_on_arg <- suppressWarnings(tf_interpolate(aligned, arg = arg))
    template_on_arg <- suppressWarnings(tfd(current_template, arg = arg))
    template_vec <- tf_evaluate(template_on_arg, arg = arg)[[1]]

    obj <- outer_registration_objective(
      method = method,
      aligned = aligned_on_arg,
      template = template_on_arg,
      arg = arg,
      dots = dots
    )
    if (is.finite(obj)) {
      if (
        is.finite(best_obj) &&
          obj > best_obj * (1 + sqrt(.Machine$double.eps))
      ) {
        cli::cli_inform(
          "Iterative registration stopped after {iter - 1} of {max_iter} iteration{?s}: alignment worsened (objective {round(obj, 4)} > {round(best_obj, 4)})."
        )
        warps <- best_warps
        break
      }
      best_warps <- warps
      best_template <- current_template
      best_obj <- obj
    } else if (is.null(best_warps)) {
      best_warps <- warps
    }

    if (
      method == "fda" &&
        iter >= fda_min_iter &&
        is.finite(prev_obj) &&
        is.finite(obj)
    ) {
      rel_improvement <- (prev_obj - obj) /
        max(abs(prev_obj), .Machine$double.eps)
      if (rel_improvement >= 0 && rel_improvement < tol^2) {
        break
      }
    }
    prev_obj <- obj

    if (iter == max_iter) {
      if (max_iter > 1L) {
        cli::cli_inform(
          "Iterative registration reached {.arg max_iter} = {max_iter} without convergence (tol = {tol})."
        )
      }
      break
    }

    new_template <- suppressWarnings(mean(aligned_on_arg, na.rm = TRUE))
    new_tmpl_vec <- tf_evaluate(new_template, arg = arg)[[1]]
    old_vec <- template_vec
    # Preserve previous template where all aligned curves are undefined.
    missing_tmpl <- !is.finite(new_tmpl_vec)
    if (any(missing_tmpl)) {
      new_tmpl_vec[missing_tmpl] <- old_vec[missing_tmpl]
    }
    new_template <- tfd(matrix(new_tmpl_vec, nrow = 1), arg = arg)
    if (method != "fda") {
      domain_length <- diff(tf_domain(x))
      delta <- suppressWarnings(
        tf_integrate((new_template - template_on_arg)^2, arg = arg)
      )
      norm_sq <- suppressWarnings(tf_integrate(template_on_arg^2, arg = arg))
      delta <- as.numeric(delta / domain_length)
      norm_sq <- as.numeric(norm_sq / domain_length)
      if (
        is.finite(delta) &&
          delta / max(norm_sq, .Machine$double.eps) < tol^2
      ) {
        break
      }
    }
    current_template <- new_template
  }
  result <- best_warps %||% warps
  attr(result, "template") <- if (!is.null(best_warps)) best_template else
    current_template
  result
}

#' @export
tf_estimate_warps.tfb <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2
) {
  x |>
    as.tfd() |>
    tf_estimate_warps(
      template = template,
      method = method,
      max_iter = max_iter,
      tol = tol,
      ...
    )
}

#' @export
tf_estimate_warps.tfd_irreg <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "fda", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2
) {
  assert_tfd(x)
  method <- match.arg(method)
  assert_count(max_iter, positive = TRUE)
  assert_number(tol, lower = 0)

  if (method %in% c("srvf", "fda")) {
    cli::cli_abort(
      c(
        "For {.cls tfd_irreg}, only `affine` and `landmark` registration are currently supported.",
        "i" = "Convert to {.cls tfd_reg} first to use {.val {method}}."
      )
    )
  }
  if (method == "landmark") {
    warps <- tf_register_landmark(x, ...)
    attr(warps, "template") <- NULL
    return(warps)
  }

  arg_list <- ensure_list(tf_arg(x))
  arg_common <- sort_unique(arg_list, simplify = TRUE)
  x_common <- suppressWarnings(tfd(x, arg = arg_common))
  template_common <- if (is.null(template)) {
    NULL
  } else {
    suppressWarnings(tfd(template, arg = arg_common))
  }

  warp_common <- tf_estimate_warps.tfd_reg(
    x = x_common,
    ...,
    template = template_common,
    method = "affine",
    max_iter = max_iter,
    tol = tol
  )
  tmpl <- attr(warp_common, "template")
  warp_values <- map(
    seq_along(x),
    \(i) tf_evaluate(warp_common[i], arg = arg_list[[i]])[[1]]
  )
  result <- tfd(warp_values, arg = arg_list, domain = tf_domain(x))
  attr(result, "template") <- tmpl
  result
}

#-------------------------------------------------------------------------------

tf_register_srvf <- function(x, template, ...) {
  rlang::check_installed("fdasrvf")

  arg <- tf_arg(x)
  domain <- tf_domain(x)
  lwr <- domain[1]
  upr <- domain[2]

  # Karcher mean
  x_mat <- as.matrix(x)
  if (is.null(template)) {
    ret <- suppressMessages(fdasrvf::time_warping(
      f = t(x_mat),
      time = arg,
      ...
    ))
    warp <- t(ret$warping_functions)
    # Extract Karcher mean as template
    tmpl <- tfd(matrix(ret$fmean, nrow = 1), arg = arg)
  } else {
    is_single_template <- length(template) == 1
    template_mat <- as.matrix(template)
    warp <- matrix(nrow = nrow(x_mat), ncol = ncol(x_mat))
    for (i in seq_len(nrow(x_mat))) {
      warp[i, ] <- fdasrvf::pair_align_functions(
        f1 = if (is_single_template) template_mat[1, ] else template_mat[i, ],
        f2 = x_mat[i, ],
        time = arg,
        ...
      )$gam
    }
    tmpl <- template
  }
  warp <- lwr + (upr - lwr) * warp
  # avoid numerical over/underflow issue:
  warp[, 1] <- arg[1]
  warp[, length(arg)] <- arg[length(arg)]
  result <- tfd(warp, arg = arg)
  attr(result, "template") <- tmpl
  result
}

# Internal: Landmark registration implementation
# Called by tf_estimate_warps() with method = "landmark"
tf_register_landmark <- function(x, landmarks, template_landmarks = NULL) {
  assert_tf(x)
  assert_matrix(landmarks, mode = "numeric", nrows = length(x), min.cols = 1)

  domain <- tf_domain(x)
  arg <- tf_arg(x)
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
  warp_on_arg <- \(landmark_row, x_arg) {
    valid <- !is.na(landmark_row)
    t_arg <- c(domain[1], template_landmarks[valid], domain[2])
    w_vals <- c(domain[1], landmark_row[valid], domain[2])
    approx(t_arg, w_vals, xout = x_arg, rule = 2)$y
  }

  if (is.list(arg)) {
    warp_list <- lapply(seq_len(n), \(i) warp_on_arg(landmarks[i, ], arg[[i]]))
    return(tfd(warp_list, arg = arg, domain = domain))
  }

  if (!has_na) {
    # Fast path: all landmarks present, vectorized construction
    template_arg <- c(domain[1], template_landmarks, domain[2])
    warp_values <- cbind(domain[1], landmarks, domain[2])
    return(tfd(warp_values, arg = template_arg))
  }

  # NA-aware path: build per-curve warps using only available landmarks
  arg <- as.numeric(arg)
  warp_list <- lapply(seq_len(n), \(i) {
    warp_on_arg(landmarks[i, ], arg)
  })
  tfd(do.call(rbind, warp_list), arg = arg)
}

#-------------------------------------------------------------------------------

# Internal: Affine registration implementation
# Called by tf_estimate_warps() with method = "affine"
#' @importFrom stats approx optim
tf_register_affine <- function(
  x,
  template = NULL,
  type = c("shift", "scale", "shift_scale"),
  shift_range = NULL,
  scale_range = NULL
) {
  assert_tf(x)
  type <- match.arg(type)

  # Validate range parameters
  assert_numeric(
    shift_range,
    len = 2,
    sorted = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_numeric(
    scale_range,
    len = 2,
    lower = 1e-10,
    sorted = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )

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

  result <- tfd(warp_mat, arg = arg)
  attr(result, "template") <- template
  result
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
  # Final tf_align will use rule=1 (NA) for proper incomplete functions
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
