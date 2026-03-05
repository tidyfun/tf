# sim-metrics.R -- Performance Measures for Registration Benchmark
#
# Provides:
#   extract_metrics()      - main entry: all metrics for one (data, method_result) pair
#   warp_mise()            - integrated squared warp error
#   warp_mise_interior()   - warp MISE restricted to [0.1, 0.9]
#   aligned_mise()         - integrated squared alignment error
#   alignment_cc()         - cross-correlation between aligned and template
#   amplitude_rmse()       - RMSE of amplitude coefficient recovery
#   landmark_detection_error() - mean abs landmark detection error
#   warp_roughness()       - mean integrated squared second derivative of warp
#   boundary_na_rate()     - fraction of NA at boundaries
#
# All metrics returned with NA on failure (never errors out).

devtools::load_all()

# --- Main Entry Point ---------------------------------------------------------

#' Extract all performance metrics for one method result
#'
#' @param data list from generate_data()
#' @param result list from fit_method() with components: warp_est, aligned,
#'   time, error, converged, detected_landmarks
#' @return named list of metric values
extract_metrics <- function(data, result) {
  if (!is.null(result$error)) {
    return(failure_metrics(result))
  }

  arg <- data$arg
  template <- data$template
  warps_true <- data$warps
  warp_est <- result$warp_est
  aligned <- result$aligned
  spec <- data$spec

  metrics <- list(
    warp_mise = safe_metric(warp_mise, warp_est, warps_true),
    warp_mise_interior = safe_metric(
      warp_mise_interior,
      warp_est,
      warps_true,
      lower = 0.1,
      upper = 0.9
    ),
    aligned_mise = safe_metric(aligned_mise, aligned, template),
    alignment_cc = safe_metric(alignment_cc, aligned, template),
    warp_roughness = safe_metric(
      compute_warp_roughness,
      warp_est,
      data$spec$phase
    ),
    boundary_na_rate = safe_metric(boundary_na_rate, warp_est, arg),
    time = result$time,
    converged = result$converged %||% TRUE,
    failure = FALSE
  )

  # Amplitude recovery (only for DGPs with amplitude variation)
  metrics$amplitude_rmse <- safe_metric(
    compute_amplitude_rmse,
    aligned,
    template,
    data$amp_coefs,
    arg
  )

  # Landmark detection error (only for landmark_auto)
  if (!is.null(result$detected_landmarks) && !is.null(data$true_landmarks)) {
    metrics$landmark_error <- safe_metric(
      landmark_detection_error,
      result$detected_landmarks,
      data$true_landmarks
    )
  } else {
    metrics$landmark_error <- NA_real_
  }

  metrics
}

#' Return all-NA metrics for failed runs
#' @keywords internal
failure_metrics <- function(result) {
  list(
    warp_mise = NA_real_,
    warp_mise_interior = NA_real_,
    aligned_mise = NA_real_,
    alignment_cc = NA_real_,
    warp_roughness = NA_real_,
    boundary_na_rate = NA_real_,
    amplitude_rmse = NA_real_,
    landmark_error = NA_real_,
    time = result$time %||% NA_real_,
    converged = FALSE,
    failure = TRUE
  )
}

#' Safely evaluate a metric function (returns NA on error)
#' @keywords internal
safe_metric <- function(f, ...) {
  tryCatch(f(...), error = function(e) NA_real_)
}

# --- Warp MISE ----------------------------------------------------------------

#' Warp Mean Integrated Squared Error
#'
#' mean(tf_integrate((w_est - w_true)^2, na.rm = TRUE))
#'
#' @param warp_est tfd of estimated warps
#' @param warp_true tfd of true warps
#' @return scalar
warp_mise <- function(warp_est, warp_true) {
  # Ensure same arg
  arg <- tf_arg(warp_true)
  if (is.list(arg)) arg <- arg[[1]]
  warp_est <- tfd(warp_est, arg = arg)
  warp_true <- tfd(warp_true, arg = arg)

  sq_diff <- (warp_est - warp_true)^2
  ise_vals <- sapply(tf_evaluations(sq_diff), function(v) {
    valid <- !is.na(v)
    if (sum(valid) < 2) return(NA_real_)
    trapez_int(arg[valid], v[valid])
  })
  mean(ise_vals, na.rm = TRUE)
}

#' Interior Warp MISE (restricted to [lower, upper])
#'
#' @param warp_est tfd of estimated warps
#' @param warp_true tfd of true warps
#' @param lower lower bound of interior region
#' @param upper upper bound of interior region
#' @return scalar
warp_mise_interior <- function(warp_est, warp_true, lower = 0.1, upper = 0.9) {
  arg <- tf_arg(warp_true)
  if (is.list(arg)) arg <- arg[[1]]
  warp_est <- tfd(warp_est, arg = arg)
  warp_true <- tfd(warp_true, arg = arg)

  interior <- arg >= lower & arg <= upper
  if (sum(interior) < 2) return(NA_real_)

  arg_int <- arg[interior]
  sq_diff <- (warp_est - warp_true)^2
  ise_vals <- sapply(tf_evaluations(sq_diff), function(v) {
    v_int <- v[interior]
    valid <- !is.na(v_int)
    if (sum(valid) < 2) return(NA_real_)
    trapez_int(arg_int[valid], v_int[valid])
  })
  mean(ise_vals, na.rm = TRUE)
}

# --- Aligned MISE -------------------------------------------------------------

#' Aligned Mean Integrated Squared Error
#'
#' mean(tf_integrate((aligned - template)^2))
#'
#' @param aligned tfd of aligned curves
#' @param template tfd of length 1
#' @return scalar
aligned_mise <- function(aligned, template) {
  template_rep <- rep(template, length(aligned))
  sq_diff <- (aligned - template_rep)^2
  # Extract arg from the result of subtraction (may differ from template's arg)
  arg <- tf_arg(sq_diff)
  if (is.list(arg)) arg <- arg[[1]]
  ise_vals <- sapply(tf_evaluations(sq_diff), function(v) {
    valid <- !is.na(v)
    if (sum(valid) < 2) return(NA_real_)
    trapez_int(arg[valid], v[valid])
  })
  mean(ise_vals, na.rm = TRUE)
}

# --- Alignment Cross-Correlation ---------------------------------------------

#' Alignment cross-correlation
#'
#' mean(tf_crosscor(aligned, template))
#'
#' @param aligned tfd of aligned curves
#' @param template tfd of length 1
#' @return scalar
alignment_cc <- function(aligned, template) {
  template_rep <- rep(template, length(aligned))
  cc_vals <- tf_crosscor(aligned, template_rep)
  mean(cc_vals, na.rm = TRUE)
}

# --- Amplitude RMSE -----------------------------------------------------------

#' Compute amplitude coefficient recovery RMSE
#'
#' Method depends on amplitude type:
#' - mult: a_est = <aligned, template> / <template, template>
#' - rank2: lm(aligned ~ template) -> (a_est, c_est)
#' - additive: b_est = integral(aligned - template) / domain_length
#' - corr_deriv/corr_orth: c_est = <aligned - template, phi> / <phi, phi>
#'
#' @param aligned tfd of aligned curves
#' @param template tfd of length 1
#' @param amp_coefs list from generate_amplitude()
#' @param arg evaluation grid
#' @return scalar RMSE (or NA if not applicable)
compute_amplitude_rmse <- function(aligned, template, amp_coefs, arg) {
  if (amp_coefs$type == "none") return(NA_real_)

  template_vals <- tf_evaluations(template)[[1]]
  domain_length <- diff(range(arg))
  n <- length(aligned)

  switch(
    amp_coefs$type,
    mult = {
      # a_est = <aligned, template> / <template, template>
      denom <- trapez_int(arg, template_vals^2)
      a_est <- sapply(tf_evaluations(aligned), function(v) {
        valid <- !is.na(v) & !is.na(template_vals)
        if (sum(valid) < 2) return(NA_real_)
        trapez_int(arg[valid], v[valid] * template_vals[valid]) / denom
      })
      sqrt(mean((a_est - amp_coefs$a)^2, na.rm = TRUE))
    },

    rank2 = {
      # lm(aligned_i ~ template) -> slope = a_est, intercept = c_est
      a_est <- numeric(n)
      c_est <- numeric(n)
      for (i in seq_len(n)) {
        v <- tf_evaluations(aligned)[[i]]
        valid <- !is.na(v)
        if (sum(valid) < 3) {
          a_est[i] <- c_est[i] <- NA_real_
          next
        }
        fit <- lm(v[valid] ~ template_vals[valid])
        a_est[i] <- coef(fit)[2]
        c_est[i] <- coef(fit)[1]
      }
      rmse_a <- sqrt(mean((a_est - amp_coefs$a)^2, na.rm = TRUE))
      rmse_c <- sqrt(mean((c_est - amp_coefs$c)^2, na.rm = TRUE))
      # Return combined RMSE (Euclidean norm of per-parameter RMSEs)
      sqrt(rmse_a^2 + rmse_c^2)
    },

    additive = {
      # b_est = integral(aligned - template) / domain_length
      b_est <- sapply(tf_evaluations(aligned), function(v) {
        diff_v <- v - template_vals
        valid <- !is.na(diff_v)
        if (sum(valid) < 2) return(NA_real_)
        trapez_int(arg[valid], diff_v[valid]) / domain_length
      })
      sqrt(mean((b_est - amp_coefs$b)^2, na.rm = TRUE))
    },

    corr_deriv = {
      phi <- compute_phi_deriv(
        tfd(matrix(template_vals, nrow = 1), arg = arg),
        arg
      )
      denom <- trapez_int(arg, phi^2)
      c_est <- sapply(tf_evaluations(aligned), function(v) {
        diff_v <- v - template_vals
        valid <- !is.na(diff_v)
        if (sum(valid) < 2) return(NA_real_)
        trapez_int(arg[valid], diff_v[valid] * phi[valid]) / denom
      })
      sqrt(mean((c_est - amp_coefs$c)^2, na.rm = TRUE))
    },

    corr_orth = {
      phi <- compute_phi_orth(
        tfd(matrix(template_vals, nrow = 1), arg = arg),
        arg
      )
      denom <- trapez_int(arg, phi^2)
      c_est <- sapply(tf_evaluations(aligned), function(v) {
        diff_v <- v - template_vals
        valid <- !is.na(diff_v)
        if (sum(valid) < 2) return(NA_real_)
        trapez_int(arg[valid], diff_v[valid] * phi[valid]) / denom
      })
      sqrt(mean((c_est - amp_coefs$c)^2, na.rm = TRUE))
    }
  )
}

# --- Warp Roughness -----------------------------------------------------------

#' Warp roughness: mean integrated squared second derivative
#'
#' Only meaningful for smooth-warp methods (SRVF, FDA). Skip for landmark/affine.
#'
#' @param warp_est tfd of estimated warps
#' @param phase_type character indicating phase type
#' @return scalar (or NA if not applicable)
compute_warp_roughness <- function(warp_est, phase_type) {
  # Only meaningful for smooth-warp methods; piecewise-linear (landmark/affine)
  # warps have degenerate second derivatives
  if (phase_type %in% c("shift", "affine", "landmark")) return(NA_real_)
  warp_d2 <- tf_derive(tf_derive(warp_est))
  d2_arg <- tf_arg(warp_d2)
  if (is.list(d2_arg)) d2_arg <- d2_arg[[1]]

  roughness <- sapply(tf_evaluations(warp_d2), function(v) {
    valid <- !is.na(v)
    if (sum(valid) < 2) return(NA_real_)
    trapez_int(d2_arg[valid], v[valid]^2)
  })
  mean(roughness, na.rm = TRUE)
}

# --- Boundary NA Rate ---------------------------------------------------------

#' Boundary NA rate
#'
#' Fraction of NA in warp estimates at boundaries (arg < 0.1 or arg > 0.9)
#'
#' @param warp_est tfd of estimated warps
#' @param arg evaluation grid
#' @return scalar
boundary_na_rate <- function(warp_est, arg) {
  boundary_idx <- arg < 0.1 | arg > 0.9
  if (!any(boundary_idx)) return(0)

  na_count <- sapply(tf_evaluations(warp_est), function(v) {
    sum(is.na(v[boundary_idx]))
  })
  sum(na_count) / (length(na_count) * sum(boundary_idx))
}

# --- Landmark Detection Error -------------------------------------------------

#' Mean absolute landmark detection error
#'
#' For each curve, matches each true landmark to the nearest detected landmark
#' (nearest-neighbor), then averages absolute errors. Handles dimension
#' mismatches when detected and true landmark matrices have different numbers
#' of columns.
#'
#' @param detected matrix of detected landmark positions (n x k_detected)
#' @param true_landmarks matrix of true landmark positions (n x k_true)
#' @return scalar
landmark_detection_error <- function(detected, true_landmarks) {
  if (is.null(detected) || is.null(true_landmarks)) return(NA_real_)
  if (!is.matrix(detected)) detected <- matrix(detected, ncol = 1)
  if (!is.matrix(true_landmarks))
    true_landmarks <- matrix(true_landmarks, ncol = 1)

  n <- nrow(true_landmarks)
  if (nrow(detected) != n) return(NA_real_)

  # Same dimensions: direct comparison (fast path)
  if (ncol(detected) == ncol(true_landmarks)) {
    return(mean(abs(detected - true_landmarks), na.rm = TRUE))
  }

  # Different dimensions: nearest-neighbor matching per curve
  errors <- numeric(0)
  for (i in seq_len(n)) {
    true_i <- true_landmarks[i, !is.na(true_landmarks[i, ])]
    det_i <- detected[i, !is.na(detected[i, ])]
    if (length(true_i) == 0 || length(det_i) == 0) next
    for (tl in true_i) {
      errors <- c(errors, min(abs(det_i - tl)))
    }
  }
  if (length(errors) == 0) return(NA_real_)
  mean(errors)
}

# --- Trapezoidal integration (duplicated from sim-dgp.R for independence) -----

trapez_int <- function(arg, vals) {
  sum(diff(arg) * (head(vals, -1) + vals[-1]) / 2)
}
