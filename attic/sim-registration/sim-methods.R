# sim-methods.R -- Method Wrappers for Registration Benchmark
#
# Provides:
#   fit_method()           - main entry: run one method config on one dataset
#   method_configs()       - table of all 7 method configurations
#
# Each method wrapper uses tryCatch and records timing + convergence.

devtools::load_all()

# --- Method Configuration Table -----------------------------------------------

#' Get method configurations
#' @return named list of lists, each with $method and $args
method_configs <- function() {
  configs <- list(
    srvf = list(
      method = "srvf",
      args = list()
    ),
    fda_default = list(
      method = "fda",
      args = list()
    ),
    fda_crit1 = list(
      method = "fda",
      args = list(crit = 1)
    ),
    affine_shift = list(
      method = "affine",
      args = list(type = "shift")
    ),
    affine_ss = list(
      method = "affine",
      args = list(type = "shift_scale")
    ),
    landmark_auto = list(
      method = "landmark",
      args = list() # landmarks computed at fit time
    ),
    landmark_oracle = list(
      method = "landmark",
      args = list() # landmarks from data$true_landmarks
    )
  )
  configs
}

# --- Main Method Wrapper ------------------------------------------------------

#' Fit one method configuration on one dataset
#'
#' @param data list from generate_data()
#' @param config_name character: name from method_configs()
#' @param use_true_template logical: if TRUE, pass true template to method
#' @return list with:
#'   warp_est: tfd of estimated warps (or NULL on failure)
#'   aligned: tfd of aligned curves (or NULL on failure)
#'   time: elapsed time in seconds
#'   error: error message (or NULL on success)
#'   converged: logical
#'   detected_landmarks: matrix (for landmark_auto) or NULL
fit_method <- function(data, config_name, use_true_template = TRUE) {
  configs <- method_configs()
  if (!config_name %in% names(configs)) {
    cli::cli_abort(
      "Unknown config: {config_name}. Must be one of: {paste(names(configs), collapse = ', ')}"
    )
  }
  config <- configs[[config_name]]

  x <- data$x
  template <- if (use_true_template) data$template else NULL
  arg <- data$arg

  # Build call arguments
  call_args <- list(x = x, method = config$method)
  extra <- config$args

  # Method-specific setup
  detected_landmarks <- NULL

  if (config_name == "landmark_auto") {
    # Auto-detect landmarks
    lm_detected <- tryCatch(
      tf_landmarks_extrema(x, "both"),
      error = function(e) NULL
    )
    if (is.null(lm_detected)) {
      return(list(
        warp_est = NULL,
        aligned = NULL,
        time = NA_real_,
        error = "Landmark detection failed",
        converged = FALSE,
        detected_landmarks = NULL
      ))
    }
    detected_landmarks <- lm_detected
    extra$landmarks <- lm_detected
    # Don't pass template for landmark method
    call_args$template <- NULL
  } else if (config_name == "landmark_oracle") {
    # Use true landmarks
    if (is.null(data$true_landmarks)) {
      return(list(
        warp_est = NULL,
        aligned = NULL,
        time = NA_real_,
        error = "No true landmarks for this DGP",
        converged = FALSE,
        detected_landmarks = NULL
      ))
    }
    extra$landmarks <- data$true_landmarks
    if (!is.null(data$template_landmarks)) {
      extra$template_landmarks <- data$template_landmarks
    }
    call_args$template <- NULL
  } else {
    # Template-based methods
    if (!is.null(template)) {
      call_args$template <- template
    }
    # Procrustes iteration when estimating template (Study 2)
    if (!use_true_template && config$method %in% c("fda", "affine")) {
      call_args$max_iter <- 20L
    }
  }

  # Merge extra args
  call_args <- c(call_args, extra)

  # Run with timing and error handling
  t0 <- proc.time()
  result <- tryCatch(
    {
      warp_est <- do.call(tf_register, call_args)
      aligned <- tf_unwarp(x, warp_est)
      list(
        warp_est = warp_est,
        aligned = aligned,
        error = NULL,
        converged = TRUE
      )
    },
    error = function(e) {
      list(
        warp_est = NULL,
        aligned = NULL,
        error = conditionMessage(e),
        converged = FALSE
      )
    }
  )
  elapsed <- (proc.time() - t0)["elapsed"]

  # Post-hoc centering for affine methods (identifiability constraint for

  # benchmark). Re-centers (a, b) parameters so mean warp ≈ identity, then
  # re-computes aligned curves. Does not change per-curve optimization quality
  # since we only shift parameters, not re-optimize.
  if (result$converged && config$method == "affine" && length(x) > 1) {
    result <- center_affine_warps(result, x, arg)
  }

  list(
    warp_est = result$warp_est,
    aligned = result$aligned,
    time = as.numeric(elapsed),
    error = result$error,
    converged = result$converged,
    detected_landmarks = detected_landmarks
  )
}

# --- Post-hoc Affine Centering ------------------------------------------------

#' Re-center affine warps for identifiability: enforce mean(a)=1, mean(b)=0.
#'
#' Extracts per-curve (a_i, b_i) via linear regression on the evaluation grid,
#' shifts parameters so mean(a)=1 and mean(b)=0, then rebuilds warps and
#' re-computes aligned curves.
#'
#' @param result list with $warp_est (tfd), $aligned (tfd), $error, $converged
#' @param x original tfd curves
#' @param arg evaluation grid
#' @return updated result list
center_affine_warps <- function(result, x, arg) {
  warp_mat <- as.matrix(result$warp_est)
  n <- nrow(warp_mat)

  # Extract per-curve (a_i, b_i) via lm.fit: h_i(s) = b_i + a_i * s
  design <- cbind(1, arg)
  ab <- t(apply(warp_mat, 1, function(h) {
    stats::lm.fit(design, h)$coefficients
  }))
  a_vec <- ab[, 2]
  b_vec <- ab[, 1]

  # Re-center: shift a_i so mean = 1, shift b_i so mean = 0
  a_centered <- a_vec - mean(a_vec) + 1
  b_centered <- b_vec - mean(b_vec)

  # Rebuild warp matrix from centered parameters
  for (i in seq_len(n)) {
    warp_mat[i, ] <- a_centered[i] * arg + b_centered[i]
  }

  warp_centered <- tfd(warp_mat, arg = arg)
  aligned_centered <- tf_unwarp(x, warp_centered)

  result$warp_est <- warp_centered
  result$aligned <- aligned_centered
  result
}

# --- Applicable Methods per DGP -----------------------------------------------

#' Which method configs are applicable for a given DGP?
#'
#' landmark_oracle only applies to S8, S9 (harmonic template with known landmarks).
#' All other methods apply to all DGPs.
#'
#' @param dgp character DGP identifier
#' @return character vector of applicable config names
applicable_methods <- function(dgp) {
  all_methods <- names(method_configs())
  if (dgp %in% c("S8", "S9", "R3b")) {
    return(all_methods)
  }
  setdiff(all_methods, "landmark_oracle")
}
