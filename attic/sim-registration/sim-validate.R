# sim-validate.R -- Known-Answer Tests & Pilot Calibration
#
# Provides:
#   run_known_answer_tests()  - 5 sanity checks with known correct answers
#   run_pilot_calibration()   - pilot checks on 10 reps of core grid
#
# Run this before the full benchmark to catch bugs early.

base_dir <- here::here("attic", "sim-registration")
source(file.path(base_dir, "sim-dgp.R"))
source(file.path(base_dir, "sim-methods.R"))
source(file.path(base_dir, "sim-metrics.R"))
source(file.path(base_dir, "sim-config.R"))

# ==============================================================================
# Known-Answer Tests (Section 6.1)
# ==============================================================================

run_known_answer_tests <- function(verbose = TRUE) {
  results <- list()
  n_pass <- 0
  n_fail <- 0

  report <- function(name, passed, detail = "") {
    status <- if (passed) "PASS" else "FAIL"
    if (verbose) cat(sprintf("  [%s] %s %s\n", status, name, detail))
    if (passed) n_pass <<- n_pass + 1 else n_fail <<- n_fail + 1
  }

  cat("=== Known-Answer Tests ===\n\n")

  # --- Test 1: Identity warp -> near-identity recovery, MISE ~ 0 -------------
  cat("Test 1: Identity warp recovery\n")
  tryCatch(
    {
      arg <- seq(0, 1, length.out = 101)
      template <- make_template("harmonic", arg)
      # Create unwarped data (identity warp)
      x <- rep(template, 10) +
        tfd(matrix(rnorm(10 * 101, 0, 0.001), nrow = 10), arg = arg)
      identity_warps <- tfd(matrix(rep(arg, each = 10), nrow = 10), arg = arg)

      for (method in c("srvf", "affine_shift")) {
        result <- fit_method(
          list(
            x = x,
            template = template,
            warps = identity_warps,
            arg = arg,
            amp_coefs = list(type = "none"),
            true_landmarks = NULL,
            template_landmarks = NULL,
            spec = list(phase = "smooth")
          ),
          method
        )
        if (!is.null(result$error)) {
          report(
            paste("identity_warp", method),
            FALSE,
            paste("Error:", result$error)
          )
        } else {
          mise <- warp_mise(result$warp_est, identity_warps)
          report(
            paste("identity_warp", method),
            mise < 0.01,
            sprintf("MISE = %.6f", mise)
          )
        }
      }
    },
    error = function(e) report("identity_warp", FALSE, e$message)
  )

  # --- Test 2: Affine on affine DGP -> exact recovery -------------------------
  cat("\nTest 2: Affine method on affine DGP\n")
  tryCatch(
    {
      set.seed(42)
      data <- generate_data(
        "S1",
        n = 20,
        n_grid = 101,
        severity = 0.5,
        noise_sd = 0,
        seed = 42
      )
      result <- fit_method(data, "affine_shift")
      if (!is.null(result$error)) {
        report("affine_on_shift", FALSE, paste("Error:", result$error))
      } else {
        mise <- warp_mise(result$warp_est, data$warps)
        # Threshold relaxed: boundary extrapolation (rule=2) in non-domain-preserving
        # DGPs introduces small errors even for the well-specified method
        report("affine_on_shift", mise < 0.05, sprintf("MISE = %.6f", mise))
      }
    },
    error = function(e) report("affine_on_shift", FALSE, e$message)
  )

  # --- Test 3: Landmark oracle on landmark DGP -> near-exact alignment --------
  cat("\nTest 3: Landmark oracle on landmark DGP\n")
  tryCatch(
    {
      set.seed(123)
      data <- generate_data(
        "S8",
        n = 15,
        n_grid = 101,
        severity = 0.5,
        noise_sd = 0,
        seed = 123
      )
      result <- fit_method(data, "landmark_oracle")
      if (!is.null(result$error)) {
        report("landmark_oracle", FALSE, paste("Error:", result$error))
      } else {
        mise <- warp_mise(result$warp_est, data$warps)
        a_mise <- aligned_mise(result$aligned, data$template)
        report(
          "landmark_oracle_warp",
          mise < 0.01,
          sprintf("Warp MISE = %.6f", mise)
        )
        report(
          "landmark_oracle_aligned",
          a_mise < 0.01,
          sprintf("Aligned MISE = %.6f", a_mise)
        )
      }
    },
    error = function(e) report("landmark_oracle", FALSE, e$message)
  )

  # --- Test 4: Round-trip warp/unwarp consistency ----------------------------
  cat("\nTest 4: Round-trip warp/unwarp\n")
  tryCatch(
    {
      set.seed(456)
      arg <- seq(0, 1, length.out = 101)
      template <- make_template("harmonic", arg)
      warps <- generate_smooth_warps(5, arg, severity = 0.5)

      warped <- tf_warp(rep(template, 5), warps)
      roundtrip <- tf_unwarp(warped, warps)

      # roundtrip should approximately equal template
      rt_mise <- aligned_mise(roundtrip, template)
      report(
        "roundtrip_warp_unwarp",
        rt_mise < 0.001,
        sprintf("Round-trip MISE = %.6f", rt_mise)
      )
    },
    error = function(e) report("roundtrip", FALSE, e$message)
  )

  # --- Test 5: Metric sanity checks ------------------------------------------
  cat("\nTest 5: Metric sanity\n")
  tryCatch(
    {
      arg <- seq(0, 1, length.out = 101)
      template <- make_template("harmonic", arg)
      perfect_warps <- tfd(matrix(rep(arg, each = 5), nrow = 5), arg = arg)

      # MISE = 0 when est = true
      mise_zero <- warp_mise(perfect_warps, perfect_warps)
      report(
        "mise_zero_when_identical",
        abs(mise_zero) < 1e-10,
        sprintf("MISE = %.2e", mise_zero)
      )

      # CC = 1 when aligned = template
      aligned_perfect <- rep(template, 5)
      cc_one <- alignment_cc(aligned_perfect, template)
      report(
        "cc_one_when_aligned",
        abs(cc_one - 1) < 0.01,
        sprintf("CC = %.4f", cc_one)
      )
    },
    error = function(e) report("metric_sanity", FALSE, e$message)
  )

  # --- Summary ----------------------------------------------------------------
  cat(sprintf("\n=== Results: %d passed, %d failed ===\n", n_pass, n_fail))
  invisible(list(passed = n_pass, failed = n_fail))
}

# ==============================================================================
# Pilot Calibration (Section 6.2)
# ==============================================================================

#' Run pilot calibration with 10 reps on core grid subset
#'
#' Checks:
#' 1. No systematic failures (error rate < 50%)
#' 2. Severity calibration: comparable ||h_i - id|| across phase types
#' 3. Mean warp ≈ identity for smooth warps
#' 4. Sigbump stress check (SRVF failure rate)
#' 5. Affine overlap check for S11
#' 6. Truncation mean check for affine warps
#' 7. fda_crit1 decision
#'
#' @param n_pilot_reps number of pilot repetitions (default 10)
#' @param verbose print progress
run_pilot_calibration <- function(n_pilot_reps = 10, verbose = TRUE) {
  cat("=== Pilot Calibration ===\n\n")

  # --- Check 1: Failure rates -------------------------------------------------
  cat("1. Checking failure rates across core DGPs...\n")
  core_dgps <- c("S1", "S4", "S5", "S8", "S11")
  methods <- c(
    "srvf",
    "fda_default",
    "affine_shift",
    "affine_ss",
    "landmark_auto"
  )

  failure_table <- matrix(
    0,
    nrow = length(core_dgps),
    ncol = length(methods),
    dimnames = list(core_dgps, methods)
  )

  for (dgp in core_dgps) {
    if (verbose) cat("  DGP:", dgp, "\n")
    for (rep in seq_len(n_pilot_reps)) {
      data <- generate_data(
        dgp,
        n = 50,
        n_grid = 101,
        severity = 0.5,
        noise_sd = 0,
        seed = make_seed(dgp, rep)
      )
      for (method in methods) {
        result <- fit_method(data, method)
        if (!is.null(result$error) || !isTRUE(result$converged)) {
          failure_table[dgp, method] <- failure_table[dgp, method] + 1
        }
      }
    }
  }
  failure_rates <- failure_table / n_pilot_reps
  cat("\nFailure rates (should be < 0.5):\n")
  print(round(failure_rates, 2))

  high_failure <- which(failure_rates >= 0.5, arr.ind = TRUE)
  if (nrow(high_failure) > 0) {
    cat("\n  WARNING: High failure rates detected:\n")
    for (i in seq_len(nrow(high_failure))) {
      cat(sprintf(
        "    %s / %s: %.0f%%\n",
        rownames(failure_rates)[high_failure[i, 1]],
        colnames(failure_rates)[high_failure[i, 2]],
        failure_rates[high_failure[i, 1], high_failure[i, 2]] * 100
      ))
    }
  }

  # --- Check 2: Severity calibration -----------------------------------------
  cat("\n2. Severity calibration: mean ||h - id||_2 by phase type...\n")
  arg <- seq(0, 1, length.out = 101)
  phase_dists <- list()
  for (sev in c(0.5, 1.0)) {
    set.seed(999)
    shift_w <- generate_shift_warps(50, arg, severity = sev)
    smooth_w <- generate_smooth_warps(50, arg, severity = sev)
    lm_res <- generate_landmark_warps(
      50,
      arg,
      find_template_landmarks(make_template("harmonic", arg), arg),
      severity = sev
    )

    dists <- c(
      shift = mean(compute_warp_distances(shift_w, arg)),
      smooth = mean(compute_warp_distances(smooth_w, arg)),
      landmark = mean(compute_warp_distances(lm_res$warps, arg))
    )

    if (sev == 1.0) {
      affine_w <- generate_affine_warps(50, arg, severity = sev)
      dists <- c(dists, affine = mean(compute_warp_distances(affine_w, arg)))
    }

    cat(sprintf(
      "  severity=%.1f: %s\n",
      sev,
      paste(names(dists), "=", round(dists, 4), collapse = ", ")
    ))
    phase_dists[[as.character(sev)]] <- dists
  }

  # --- Check 3: Mean warp ≈ identity for smooth warps -----------------------
  cat("\n3. Mean warp ≈ identity check (smooth warps)...\n")
  set.seed(777)
  for (sev in c(0.5, 1.0)) {
    warps <- generate_smooth_warps(100, arg, severity = sev)
    mean_warp <- colMeans(as.matrix(warps))
    max_dev <- max(abs(mean_warp - arg))
    cat(sprintf(
      "  severity=%.1f: max|mean_warp - identity| = %.6f %s\n",
      sev,
      max_dev,
      if (max_dev < 0.02) "OK" else "WARNING"
    ))
  }

  # --- Check 4: Sigbump SRVF stress check ------------------------------------
  cat("\n4. Sigbump SRVF stress check...\n")
  srvf_failures <- 0
  for (rep in seq_len(n_pilot_reps)) {
    data <- generate_data(
      "S4",
      n = 20,
      n_grid = 101,
      severity = 0.5,
      noise_sd = 0,
      seed = make_seed("S4_stress", rep)
    )
    result <- fit_method(data, "srvf")
    if (!is.null(result$error)) srvf_failures <- srvf_failures + 1
  }
  srvf_rate <- srvf_failures / n_pilot_reps
  cat(sprintf(
    "  SRVF failure rate on sigbump: %.0f%% %s\n",
    srvf_rate * 100,
    if (srvf_rate < 0.8) "OK" else "WARNING: >80%"
  ))

  # --- Check 5: Affine overlap check for S11 ---------------------------------
  cat("\n5. Affine overlap check for S11...\n")
  set.seed(555)
  for (sev in c(0.5, 1.0)) {
    affine_warps <- generate_affine_warps(100, arg, severity = sev)
    warp_evals <- tf_evaluations(affine_warps)
    overlaps <- sapply(warp_evals, function(w) {
      w_min <- min(w, na.rm = TRUE)
      w_max <- max(w, na.rm = TRUE)
      valid_lower <- max(0, w_min)
      valid_upper <- min(1, w_max)
      max(0, valid_upper - valid_lower)
    })
    med_overlap <- median(overlaps)
    cat(sprintf(
      "  severity=%.1f: median overlap = %.4f %s\n",
      sev,
      med_overlap,
      if (med_overlap >= 0.90) "OK" else "WARNING: <0.90"
    ))
  }

  # --- Check 6: Truncation mean check for affine warps -----------------------
  cat("\n6. Truncation mean check for affine warps...\n")
  set.seed(666)
  for (sev in c(0.5, 1.0)) {
    # Generate many times to check mean stability
    a_means <- numeric(20)
    b_means <- numeric(20)
    for (i in 1:20) {
      n_test <- 100
      sd_a <- 0.05 * sev
      sd_b <- 0.03 * sev
      a_draws <- rtruncnorm(n_test, 1, sd_a, 1 - 0.15 * sev, 1 + 0.15 * sev)
      b_draws <- rtruncnorm(n_test, 0, sd_b, -0.07 * sev, 0.07 * sev)
      a_draws <- a_draws - mean(a_draws) + 1
      b_draws <- b_draws - mean(b_draws)
      a_means[i] <- mean(a_draws)
      b_means[i] <- mean(b_draws)
    }
    cat(sprintf(
      "  severity=%.1f: mean(a) = %.4f ± %.4f, mean(b) = %.4f ± %.4f\n",
      sev,
      mean(a_means),
      sd(a_means),
      mean(b_means),
      sd(b_means)
    ))
  }

  # --- Check 7: fda_crit1 decision -------------------------------------------
  cat(
    "\n7. fda_crit1 vs fda_default comparison (S4, noise=0, severity=0.5)...\n"
  )
  fda_default_mises <- numeric(n_pilot_reps)
  fda_crit1_mises <- numeric(n_pilot_reps)

  for (rep in seq_len(n_pilot_reps)) {
    data <- generate_data(
      "S4",
      n = 50,
      n_grid = 101,
      severity = 0.5,
      noise_sd = 0,
      seed = make_seed("S4_fda", rep)
    )

    r_default <- fit_method(data, "fda_default")
    r_crit1 <- fit_method(data, "fda_crit1")

    fda_default_mises[rep] <- if (is.null(r_default$error)) {
      warp_mise(r_default$warp_est, data$warps)
    } else NA_real_

    fda_crit1_mises[rep] <- if (is.null(r_crit1$error)) {
      warp_mise(r_crit1$warp_est, data$warps)
    } else NA_real_
  }

  mean_default <- mean(fda_default_mises, na.rm = TRUE)
  mean_crit1 <- mean(fda_crit1_mises, na.rm = TRUE)

  cat(sprintf("  fda_default mean MISE: %.6f\n", mean_default))
  cat(sprintf("  fda_crit1 mean MISE:   %.6f\n", mean_crit1))
  if (is.nan(mean_default) || mean_default == 0) {
    cat(
      "  WARNING: Cannot compute relative difference (fda_default all failed or MISE=0)\n"
    )
  } else {
    pct_diff <- abs(mean_default - mean_crit1) / mean_default * 100
    cat(sprintf("  Relative difference:   %.1f%%\n", pct_diff))
    if (pct_diff < 10) {
      cat("  RECOMMENDATION: Drop fda_crit1 (< 10% difference)\n")
    } else {
      cat("  RECOMMENDATION: Keep fda_crit1 (>= 10% difference)\n")
    }
  }

  # --- Check 8: Estimate wall-clock time per cell ----------------------------
  cat("\n8. Timing estimate...\n")
  set.seed(888)
  data <- generate_data(
    "S4",
    n = 50,
    n_grid = 101,
    severity = 0.5,
    noise_sd = 0.1,
    seed = 888
  )
  times <- sapply(
    c("srvf", "fda_default", "affine_shift", "affine_ss", "landmark_auto"),
    function(m) {
      r <- fit_method(data, m)
      r$time
    }
  )
  cat("  Time per cell (seconds):\n")
  print(round(times, 3))
  n_methods_timed <- length(times)
  n_cores <- 4
  design <- full_design(include_fda_crit1 = FALSE)
  # Each design row is one (dgp, params, method) cell; mean time * n_cells / cores
  est_total <- (sum(times) / n_methods_timed) * nrow(design) / n_cores / 3600
  cat(sprintf(
    "  Estimated total time (%d cores): %.1f hours\n",
    n_cores,
    est_total
  ))

  cat("\n=== Pilot Calibration Complete ===\n")
}

# ==============================================================================
# Run if sourced directly
# ==============================================================================

if (sys.nframe() == 0) {
  run_known_answer_tests()
  cat("\n\n")
  run_pilot_calibration()
}
