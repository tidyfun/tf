# sim-config-study3.R -- Study 3: Outlier Contamination
#
# Design for benchmark with contaminated data (outliers injected after
# data generation). Tests which methods degrade gracefully under outliers.
#
# Provides:
#   outlier_design()  - full design table for Study 3
#
# Outlier types (3):
#   amplitude: curves scaled by Uniform(3,5) * random sign
#   phase:     warps replaced with severity=3.0 extreme warps
#   shape:     template replaced with opposite type (sigbump<->harmonic)
#
# Main arm: 4 DGPs x 3 outlier types x methods @ contam_frac=0.10
# Dose-response arm: S4 x amplitude x contam_frac={0.05, 0.20}
#
# Total: 87 cells x 50 reps = 4,350 runs

# --- Applicable Methods (inline) ---------------------------------------------

applicable_methods_outlier <- function(dgp) {
  all_methods <- c(
    "srvf",
    "fda_default",
    "fda_crit1",
    "affine_shift",
    "affine_ss",
    "landmark_auto",
    "landmark_oracle"
  )
  if (dgp %in% c("S8", "S9")) {
    return(all_methods)
  }
  setdiff(all_methods, "landmark_oracle")
}

# --- Design Builder -----------------------------------------------------------

#' Study 3 design: Outlier contamination
#'
#' @return data.frame with columns: dgp, n_curves, n_grid, noise_sd, severity,
#'   method, reps, grid_name, contam_frac, outlier_type
outlier_design <- function() {
  # Main arm: 4 representative DGPs x 3 outlier types @ 10% contamination
  main <- expand.grid(
    dgp = c("S1", "S4", "S7", "S8"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = 0.1,
    severity = 0.5,
    contam_frac = 0.10,
    outlier_type = c("amplitude", "phase", "shape"),
    stringsAsFactors = FALSE
  )

  # Dose-response arm: S4 x amplitude outlier, vary contam_frac
  dose <- expand.grid(
    dgp = "S4",
    n_curves = 50,
    n_grid = 101,
    noise_sd = 0.1,
    severity = 0.5,
    contam_frac = c(0.05, 0.20),
    outlier_type = "amplitude",
    stringsAsFactors = FALSE
  )

  grid <- rbind(main, dose)

  # Expand to applicable methods
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    methods <- applicable_methods_outlier(grid$dgp[i])
    cbind(
      grid[rep(i, length(methods)), , drop = FALSE],
      method = methods,
      stringsAsFactors = FALSE
    )
  })
  design <- do.call(rbind, rows)

  # Add metadata
  design$reps <- 50
  design$grid_name <- "outlier"
  design$use_true_template <- TRUE
  rownames(design) <- NULL
  design$cell_id <- seq_len(nrow(design))

  message(sprintf(
    "Study 3 (outlier) design: %d cells, %d total runs",
    nrow(design),
    sum(design$reps)
  ))
  design
}

# --- Summary ------------------------------------------------------------------

#' Print summary of Study 3 design
summarize_outlier_design <- function(design = NULL) {
  if (is.null(design)) design <- outlier_design()

  cat("=== Study 3: Outlier Contamination ===\n\n")
  cat("DGPs:", sort(unique(design$dgp)), "\n")
  cat("Methods:", sort(unique(design$method)), "\n")
  cat("Outlier types:", sort(unique(design$outlier_type)), "\n")
  cat("Contamination fractions:", sort(unique(design$contam_frac)), "\n")
  cat("Total cells:", nrow(design), "\n")
  cat("Total runs:", sum(design$reps), "\n")
}
