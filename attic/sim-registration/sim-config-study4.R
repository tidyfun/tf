# sim-config-study4.R -- Study 4: Real-Data Templates
#
# Design for benchmark using semi-synthetic data: templates extracted from
# tf's bundled datasets (pinch, growth, gait), warps/noise/amplitude synthetic.
# Truth is still known, allowing rigorous evaluation.
#
# Provides:
#   realdata_design()  - full design table for Study 4
#
# DGPs (6):
#   R1a: pinch / smooth / rank2 (amp_sd=0.5)
#   R1b: pinch / shift / mult (amp_sd=0.5)
#   R2a: growth_velocity / smooth / rank2 (amp_sd=0.5)
#   R2b: growth_velocity / smooth / corr_orth (amp_sd=0.5)
#   R3a: gait_hip / smooth / rank2 (amp_sd=0.5)
#   R3b: gait_hip / landmark / mult (amp_sd=0.5)
#
# Main arm: 6 DGPs x 2 noise levels x methods
# Severity arm: R1a x noise=0 x severity=1.0
#
# Total: 80 cells x 100 reps = 8,000 runs

# --- Applicable Methods (inline) ---------------------------------------------

applicable_methods_realdata <- function(dgp) {
  all_methods <- c(
    "srvf",
    "fda_default",
    "fda_crit1",
    "affine_shift",
    "affine_ss",
    "landmark_auto",
    "landmark_oracle"
  )
  if (dgp == "R3b") {
    return(all_methods)
  }
  setdiff(all_methods, "landmark_oracle")
}

# --- Design Builder -----------------------------------------------------------

#' Study 4 design: Real-data templates
#'
#' @return data.frame with columns: dgp, n_curves, n_grid, noise_sd, severity,
#'   method, reps, grid_name
realdata_design <- function() {
  # Main arm: 6 DGPs x 2 noise levels x severity 0.5
  main <- expand.grid(
    dgp = c("R1a", "R1b", "R2a", "R2b", "R3a", "R3b"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = c(0, 0.1),
    severity = 0.5,
    stringsAsFactors = FALSE
  )

  # Extra severity arm: R1a at severity 1.0, noise = 0
  extra <- data.frame(
    dgp = "R1a",
    n_curves = 50,
    n_grid = 101,
    noise_sd = 0,
    severity = 1.0,
    stringsAsFactors = FALSE
  )

  grid <- rbind(main, extra)

  # Expand to applicable methods
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    methods <- applicable_methods_realdata(grid$dgp[i])
    cbind(
      grid[rep(i, length(methods)), , drop = FALSE],
      method = methods,
      stringsAsFactors = FALSE
    )
  })
  design <- do.call(rbind, rows)

  # Add metadata
  design$reps <- 100
  design$grid_name <- "realdata"
  design$use_true_template <- TRUE
  rownames(design) <- NULL
  design$cell_id <- seq_len(nrow(design))

  message(sprintf(
    "Study 4 (real-data) design: %d cells, %d total runs",
    nrow(design),
    sum(design$reps)
  ))
  design
}

# --- Summary ------------------------------------------------------------------

#' Print summary of Study 4 design
summarize_realdata_design <- function(design = NULL) {
  if (is.null(design)) design <- realdata_design()

  cat("=== Study 4: Real-Data Templates ===\n\n")
  cat("DGPs:", sort(unique(design$dgp)), "\n")
  cat("Methods:", sort(unique(design$method)), "\n")
  cat("Noise levels:", sort(unique(design$noise_sd)), "\n")
  cat("Severity levels:", sort(unique(design$severity)), "\n")
  cat("Total cells:", nrow(design), "\n")
  cat("Total runs:", sum(design$reps), "\n")
}
