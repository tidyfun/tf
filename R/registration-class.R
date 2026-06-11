#' Registration Result Object
#'
#' @description
#' `tf_registration` objects store the result of [tf_register()], including
#' the aligned (registered) curves, estimated inverse warping functions
#' \eqn{h_i^{-1}} (observed \eqn{\to} aligned time), and the template used.
#' Use accessors [tf_aligned()], [tf_inv_warps()], and [tf_template()] to extract
#' components.
#' `tf_shape_registration` objects, returned by [tf_register_shape()], extend
#' this structure with shape-space rotations and scale factors. Use
#' [tf_rotations()] and [tf_scales()] to extract those components.
#'
#' @section Summary diagnostics:
#' `summary()` computes per-curve diagnostics for assessing registration
#' quality and prints their averages and/or deciles.
#' The printed output contains four sections:
#'
#' **Amplitude variance reduction** (only if `store_x = TRUE`): the proportion
#' of pointwise variance removed by registration, computed as
#' \eqn{1 - \bar{V}_{\mathrm{registered}} / \bar{V}_{\mathrm{original}}}
#' where \eqn{\bar{V}} is the mean (across the domain) of the pointwise
#' variance (across curves). Values near 1 indicate that registration
#' removed most of the original variability; values near 0 indicate little
#' change; negative values indicate that registration *increased* variability
#' (a sign that something went wrong).
#'
#' **Warp deviation from identity** (deciles across curves): each curve's
#' inverse warping function \eqn{h_i^{-1}} is compared to the identity via the
#' normalized integral \eqn{ 2/L^2  \int |h_i^{-1}(t) - t|\, dt }, where \eqn{L} is the domain
#' length. The normalizing constant \eqn{L^2/2} is the theoretical upper limit
#' deviation for a monotone, domain-preserving warp that maps all timepoints to
#' the first or last timepoint, so values range from 0 (identity warp, no time
#' deformation) to 1 (maximal crazy warping). Values above \eqn{\approx 0.3} may
#' suggest aggressive warping that could warrant inspection.
#'
#' **Warp slopes** (deciles of per-curve min and max \eqn{dh^{-1}/dt}): a slope of 1
#' of the warp corresponds to no local time deformation (identity).
#' Slopes \eqn{> 1} indicate local time dilation (the warped curve is
#' "stretched" relative to the template), slopes \eqn{< 1} indicate local time
#' compression, so slopes near 0 or very large slopes indicate extreme local
#' deformation. For affine shift warps, all slopes are exactly 1.
#'
#' **Domain coverage loss** (only printed if any loss occurs): the fraction of
#' the original domain range that is lost per curve after alignment, computed
#' as `1 - range(aligned_arg) / range(original_arg)`. This is only relevant
#' for affine (non-domain-preserving) warps where alignment can shift parts of
#' curves outside the original domain. Domain-preserving methods (`srvf`,
#' `cc`, `landmark`) always have zero domain loss.
#'
#' @section Accessors:
#' - `tf_aligned(x)`: extract the registered/aligned curves (`tfd` vector).
#' - `tf_inv_warps(x)`: extract the estimated inverse warping functions
#'   \eqn{h_i^{-1}(t)} that map observed time to aligned time (`tfd` vector).
#'   Use [tf_invert()] on the result to obtain forward warps if needed.
#' - `tf_template(x)`: extract the template function (`tf` vector of length 1).
#' - `tf_rotations(x)`: extract the per-curve rotation matrices from a
#'   `tf_shape_registration` object.
#' - `tf_scales(x)`: extract the per-curve scale factors from a
#'   `tf_shape_registration` object. Each factor is relative to the template
#'   (template SRVF norm divided by the curve's SRVF norm); see
#'   [tf_register_shape()].
#'
#' @param x a `tf_registration` or `tf_shape_registration` object
#' @param i index for subsetting (integer, logical, or character)
#' @param object a `tf_registration` object
#' @param ... additional arguments (currently unused)
#' @returns For `tf_registration` objects: a list with entries `registered`
#'   (`tf`-vector of aligned/registered functions from `x`), `inv_warps`
#'   (inverse warping functions aligning `x` to the template function), the
#'   `template` function, the original data `x` (if `store_x = TRUE` was used
#'   in [tf_register()]), and the `call` to [tf_register()] that created the
#'   object. `tf_shape_registration` objects additionally contain the forward
#'   `warps`, `rotations`, and `scales`. Accessors return the respective
#'   component.
#' @examples
#' reg <- tf_register(pinch[1:5], method = "affine", type = "shift_scale")
#' reg
#' summary(reg)
#' plot(reg)
#' @name tf_registration
#' @author Fabian Scheipl, Claude Opus 4.6
#' @family registration functions
NULL

# Constructor (not exported)
new_tf_registration <- function(registered, inv_warps, template, x, call) {
  structure(
    list(
      registered = registered,
      inv_warps = inv_warps,
      template = template,
      x = x,
      call = call
    ),
    class = "tf_registration"
  )
}

new_tf_shape_registration <- function(
  registered,
  warps,
  inv_warps,
  template,
  x,
  rotations,
  scales,
  call
) {
  structure(
    list(
      registered = registered,
      warps = warps,
      inv_warps = inv_warps,
      template = template,
      x = x,
      rotations = rotations,
      scales = scales,
      call = call
    ),
    class = c("tf_shape_registration", "tf_registration")
  )
}

#' @rdname tf_registration
#' @export
tf_aligned <- function(x) {
  assert_class(x, "tf_registration")
  x$registered
}

#' @rdname tf_registration
#' @export
tf_inv_warps <- function(x) {
  assert_class(x, "tf_registration")
  x$inv_warps
}

#' @rdname tf_registration
#' @export
tf_template <- function(x) {
  assert_class(x, "tf_registration")
  x$template
}

#' @rdname tf_registration
#' @export
tf_rotations <- function(x) {
  assert_class(x, "tf_shape_registration")
  x$rotations
}

#' @rdname tf_registration
#' @export
tf_scales <- function(x) {
  assert_class(x, "tf_shape_registration")
  x$scales
}

# Shared body of the registration print methods: prints the class header, the
# call, a "<n> curve(s)[ with <d> component(s)] on [a, b]" line, and the list of
# present (non-NULL) slots. `count_line` is a cli/glue template evaluated here
# (so it may reference `x` and the local `domain`); `slots` is a named logical.
print_registration <- function(x, cls, count_line, slots) {
  domain <- tf_domain(x$registered)
  cli::cli_text("{.cls {cls}}")
  cat("Call: ")
  print(x$call)
  cli::cli_text(count_line, .envir = environment())
  cli::cli_text(
    "Components: {paste(names(slots)[slots], collapse = ', ')}"
  )
  invisible(x)
}

#' @rdname tf_registration
#' @export
print.tf_registration <- function(x, ...) {
  print_registration(
    x, "tf_registration",
    "{length(x$registered)} curve{?s} on [{domain[1]}, {domain[2]}]",
    c(
      "aligned" = !is.null(x$registered),
      "inv_warps" = !is.null(x$inv_warps),
      "template" = !is.null(x$template),
      "original data" = !is.null(x$x)
    )
  )
}

#' @rdname tf_registration
#' @export
print.tf_shape_registration <- function(x, ...) {
  print_registration(
    x, "tf_shape_registration",
    "{length(x$registered)} curve{?s} with {tf_ncomp(x$registered)} component{?s} on [{domain[1]}, {domain[2]}]",
    c(
      "aligned" = !is.null(x$registered),
      "inv_warps" = !is.null(x$inv_warps),
      "template" = !is.null(x$template),
      "rotations" = !is.null(x$rotations),
      "scales" = !is.null(x$scales),
      "original data" = !is.null(x$x)
    )
  )
}

# Mean (over the domain) of the pointwise variance (across curves) of a
# tf-vector. For tf_mv inputs the per-component scalars are averaged, giving a
# single scalar comparable to the univariate case (#249). Returns NA_real_ on
# unexpected failures (e.g. degenerate inputs where `var()` or
# `tf_evaluations()` error) rather than letting `summary()` itself error.
mean_pointwise_variance <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  vx <- tryCatch(
    suppressWarnings(var(x)),
    error = function(e) NULL
  )
  if (is.null(vx)) {
    return(NA_real_)
  }
  if (is_tf_mv(vx)) {
    per_comp <- tryCatch(
      vapply(
        tf_components(vx),
        \(comp) suppressWarnings(mean(tf_evaluations(comp)[[1]], na.rm = TRUE)),
        numeric(1)
      ),
      error = function(e) NA_real_
    )
    return(suppressWarnings(mean(per_comp, na.rm = TRUE)))
  }
  ev <- tryCatch(tf_evaluations(vx), error = function(e) NULL)
  if (!length(ev)) {
    return(NA_real_)
  }
  suppressWarnings(mean(ev[[1]], na.rm = TRUE))
}

#' @rdname tf_registration
#' @export
summary.tf_registration <- function(object, ...) {
  domain <- tf_domain(object$registered)
  domain_length <- diff(domain)
  n <- length(object$registered)
  probs <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)

  # Amplitude variance reduction: 1 - mean(pointwise_var(reg)) / mean(pointwise_var(orig))
  # `var(tf_mv)` is component-wise (returns a tf_mv), so handle multivariate
  # input by averaging per-component scalar mean variances (#249).
  amp_var_reduction <- NA_real_
  if (!is.null(object$x)) {
    var_orig <- mean_pointwise_variance(object$x)
    var_reg <- mean_pointwise_variance(object$registered)
    if (is.finite(var_orig) && var_orig > 0 && is.finite(var_reg)) {
      amp_var_reduction <- 1 - var_reg / var_orig
    }
  }

  # Per-curve integrated |h(t) - t| / (domain_length^2 / 2)
  # max possible for monotone domain-preserving warp is domain_length^2 / 2,
  # so this ranges from 0 (identity) to 1 (maximal warp)
  # Compute directly from evaluations to handle tfd_irreg (inverse warps)
  arg <- tf_arg(object$inv_warps)
  args_list <- if (is.list(arg)) arg else rep(list(arg), n)
  inv_warp_evals <- tf_evaluations(object$inv_warps)
  inv_warp_dev_per_curve <- vapply(
    seq_len(n),
    \(i) {
      a <- args_list[[i]]
      dev <- abs(inv_warp_evals[[i]] - a)
      dt <- diff(a)
      sum((dev[-length(dev)] + dev[-1]) / 2 * dt)
    },
    numeric(1)
  ) /
    (domain_length^2 / 2)
  inv_warp_dev_quantiles <- stats::quantile(
    inv_warp_dev_per_curve,
    probs = probs
  )

  # Per-curve domain loss for affine (non-domain-preserving) warps:
  # fraction of original domain range lost after alignment
  aligned_args <- tf_arg(object$registered)
  if (is.list(aligned_args)) {
    domain_loss_per_curve <- vapply(
      aligned_args,
      \(a) 1 - diff(range(a)) / domain_length,
      numeric(1)
    )
  } else {
    domain_loss_per_curve <- rep(0, n)
  }
  domain_loss_quantiles <- stats::quantile(domain_loss_per_curve, probs = probs)

  # Warp slope statistics (local time dilation/compression)
  inv_warp_evals <- tf_evaluations(object$inv_warps)
  if (is.list(arg)) {
    slope_per_curve <- vapply(
      seq_len(n),
      \(i) {
        dt <- diff(arg[[i]])
        dh <- diff(inv_warp_evals[[i]])
        slopes <- dh / dt
        slopes <- slopes[is.finite(slopes)]
        c(min = min(slopes), max = max(slopes))
      },
      numeric(2)
    )
  } else {
    dt <- diff(arg)
    slope_per_curve <- vapply(
      seq_len(n),
      \(i) {
        dh <- diff(inv_warp_evals[[i]])
        slopes <- dh / dt
        slopes <- slopes[is.finite(slopes)]
        c(min = min(slopes), max = max(slopes))
      },
      numeric(2)
    )
  }
  inv_warp_slope_range <- c(
    min = min(slope_per_curve["min", ]),
    max = max(slope_per_curve["max", ])
  )
  inv_warp_min_slopes <- stats::quantile(
    slope_per_curve["min", ],
    probs = probs
  )
  inv_warp_max_slopes <- stats::quantile(
    slope_per_curve["max", ],
    probs = probs
  )

  ret <- list(
    call = object$call,
    n = n,
    domain = domain,
    amp_var_reduction = amp_var_reduction,
    inv_warp_dev_quantiles = inv_warp_dev_quantiles,
    domain_loss_quantiles = domain_loss_quantiles,
    inv_warp_slope_range = inv_warp_slope_range,
    inv_warp_min_slopes = inv_warp_min_slopes,
    inv_warp_max_slopes = inv_warp_max_slopes,
    has_original = !is.null(object$x)
  )
  class(ret) <- "summary.tf_registration"
  ret
}

#' @rdname tf_registration
#' @export
print.summary.tf_registration <- function(x, ...) {
  print(x$call)
  cat(
    "\n",
    x$n,
    " curve(s) on [",
    x$domain[1],
    ", ",
    x$domain[2],
    "]\n",
    sep = ""
  )

  if (x$has_original) {
    if (is.finite(x$amp_var_reduction)) {
      cat(
        "\nAmplitude variance reduction: ",
        round(x$amp_var_reduction * 100, 1),
        "%\n",
        sep = ""
      )
    } else {
      cat("\nAmplitude variance reduction: not computable\n")
    }
  } else {
    cat("\nAmplitude variance reduction: no original data (store_x = FALSE)\n")
  }

  cat(
    "\nInverse warp deviations from identity (relative to domain length):\n"
  )
  print(round(x$inv_warp_dev_quantiles, 4))

  constant_slopes <- max(abs(x$inv_warp_min_slopes - x$inv_warp_max_slopes)) <
    1e-10
  cat("\nInverse warp slopes (1 = identity):\n")
  cat(
    "  overall range: [",
    round(x$inv_warp_slope_range["min"], 3),
    ", ",
    round(x$inv_warp_slope_range["max"], 3),
    "]\n",
    sep = ""
  )
  if (constant_slopes) {
    cat("  per-curve slopes:\n")
    print(round(x$inv_warp_min_slopes, 3))
  } else {
    cat("  per-curve min slopes:\n")
    print(round(x$inv_warp_min_slopes, 3))
    cat("  per-curve max slopes:\n")
    print(round(x$inv_warp_max_slopes, 3))
  }

  if (any(x$domain_loss_quantiles > 0)) {
    cat(
      "\nDomain coverage loss after alignment (fraction of original range):\n"
    )
    print(round(x$domain_loss_quantiles, 4))
  }

  invisible(x)
}

# Extract rotation angle (radians) from each per-curve rotation matrix in the
# shape registration. Supports 2D (signed angle in [-pi, pi]) and generic d-D
# via the standard arccos((tr(R) - 1) / 2) for d > 2.
shape_rotation_angles <- function(rotations) {
  if (is.null(rotations)) {
    return(numeric(0))
  }
  d <- dim(rotations)[1L]
  n <- dim(rotations)[3L]
  if (!length(n) || n == 0L) {
    return(numeric(0))
  }
  if (d == 2L) {
    return(vapply(
      seq_len(n),
      \(i) atan2(rotations[2, 1, i], rotations[1, 1, i]),
      numeric(1)
    ))
  }
  vapply(
    seq_len(n),
    \(i) {
      tr <- sum(diag(rotations[,, i]))
      acos(max(-1, min(1, (tr - 1) / 2)))
    },
    numeric(1)
  )
}

#' @rdname tf_registration
#' @export
summary.tf_shape_registration <- function(object, ...) {
  base <- NextMethod()
  probs <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
  angles <- shape_rotation_angles(object$rotations)
  scales <- object$scales %||% numeric(0)
  base$rotation_angles_deg <- if (length(angles)) {
    stats::quantile(angles * 180 / pi, probs = probs)
  } else {
    NULL
  }
  base$scale_quantiles <- if (length(scales)) {
    stats::quantile(scales, probs = probs)
  } else {
    NULL
  }
  class(base) <- c("summary.tf_shape_registration", class(base))
  base
}

#' @rdname tf_registration
#' @export
print.summary.tf_shape_registration <- function(x, ...) {
  NextMethod()
  if (!is.null(x$rotation_angles_deg)) {
    cat("\nRotation angles (degrees), per-curve deciles:\n")
    print(round(x$rotation_angles_deg, 2))
  }
  if (!is.null(x$scale_quantiles)) {
    cat("\nScale factors (template / curve SRVF norm), per-curve deciles:\n")
    print(round(x$scale_quantiles, 4))
  }
  invisible(x)
}

#' @rdname tf_registration
#' @export
plot.tf_registration <- function(x, ...) {
  has_orig <- !is.null(x$x)
  if (has_orig) {
    old_par <- graphics::par(mfrow = c(1, 3))
  } else {
    old_par <- graphics::par(mfrow = c(1, 2))
  }
  on.exit(graphics::par(old_par))

  if (has_orig) {
    plot(x$x, main = "Original", ylab = "x(t)", xlab = "t", ...)
    if (!is.null(x$template)) {
      graphics::lines(x$template, lwd = 2, lty = 2)
    }
  }

  plot(
    x$inv_warps,
    main = "Inverse warps",
    xlab = "Observed time t",
    ylab = "Aligned time s",
    points = FALSE,
    ...
  )
  graphics::abline(
    a = 0,
    b = 1,
    lty = 2,
    col = "grey40"
  )

  plot(
    x$registered,
    main = "Aligned",
    xlab = "Aligned time s",
    ylab = "x(s)",
    points = FALSE,
    ...
  )
  if (!is.null(x$template)) {
    graphics::lines(x$template, lwd = 2, lty = 2)
  }
  invisible(x)
}

#' @rdname tf_registration
#' @export
`[.tf_registration` <- function(x, i) {
  new_tf_registration(
    registered = x$registered[i],
    inv_warps = x$inv_warps[i],
    template = x$template,
    x = if (!is.null(x$x)) x$x[i] else NULL,
    call = x$call
  )
}

#' @rdname tf_registration
#' @export
length.tf_registration <- function(x) {
  length(x$registered)
}

#' @rdname tf_registration
#' @export
`[.tf_shape_registration` <- function(x, i) {
  new_tf_shape_registration(
    registered = x$registered[i],
    warps = x$warps[i],
    inv_warps = x$inv_warps[i],
    template = x$template,
    x = if (!is.null(x$x)) x$x[i] else NULL,
    rotations = x$rotations[,, i, drop = FALSE],
    scales = x$scales[i],
    call = x$call
  )
}
