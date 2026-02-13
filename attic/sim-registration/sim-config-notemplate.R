# sim-config-notemplate.R -- Study 2: No Oracle Template
#
# Design for benchmark where methods estimate their own template
# (use_true_template = FALSE). Paired with Study 1 for comparison.
#
# Provides:
#   notemplate_design()  - full design table for Study 2
#
# DGP selection (10 of 12):
#   S1 (shift), S2 (shift+mult), S4 (smooth/none), S5 (smooth/mult),
#   S6a (corr-deriv stress), S6b (corr-orth), S7 (additive),
#   S8 (landmark), S10 (rank-2), S11 (affine)
# Excluded: S3 (redundant with S4), S9 (redundant with S8)
#
# Methods: 6 (all except landmark_oracle)
# Total: 132 cells x 100 reps = 13,200 runs

# --- Helper (inline to avoid circular sourcing) ------------------------------

applicable_methods_notemplate <- function(dgp) {
  # All methods except landmark_oracle (which needs true landmarks,
  # not template — but we only include it in Study 1 for S8/S9)
  c(
    "srvf",
    "fda_default",
    "fda_crit1",
    "affine_shift",
    "affine_ss",
    "landmark_auto"
  )
}

# --- Design Builder ----------------------------------------------------------

#' Study 2 design: No oracle template
#'
#' 10 DGPs x noise x severity x 6 methods, use_true_template = FALSE
#'
#' @return data.frame with columns: dgp, n_curves, n_grid, noise_sd, severity,
#'   method, reps, grid_name, use_true_template
notemplate_design <- function() {
  # Main grid: 10 DGPs x 2 noise levels x severity 0.5
  main <- expand.grid(
    dgp = c("S1", "S2", "S4", "S5", "S6a", "S6b", "S7", "S8", "S10", "S11"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = c(0, 0.1),
    severity = 0.5,
    stringsAsFactors = FALSE
  )

  # Extra arm: S4 at severity 1.0 (to test severity effect on template estimation)
  extra <- expand.grid(
    dgp = "S4",
    n_curves = 50,
    n_grid = 101,
    noise_sd = c(0, 0.1),
    severity = 1.0,
    stringsAsFactors = FALSE
  )

  grid <- rbind(main, extra)

  # Expand to all applicable methods
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    methods <- applicable_methods_notemplate(grid$dgp[i])
    cbind(
      grid[rep(i, length(methods)), , drop = FALSE],
      method = methods,
      stringsAsFactors = FALSE
    )
  })
  design <- do.call(rbind, rows)

  # Add metadata
  design$reps <- 100
  design$grid_name <- "notemplate"
  design$use_true_template <- FALSE
  rownames(design) <- NULL

  # Add cell ID
  design$cell_id <- seq_len(nrow(design))

  message(sprintf(
    "Study 2 (no template) design: %d cells, %d total runs",
    nrow(design),
    sum(design$reps)
  ))
  design
}

# --- Summary -----------------------------------------------------------------

#' Print summary of Study 2 design
summarize_notemplate_design <- function(design = NULL) {
  if (is.null(design)) design <- notemplate_design()

  cat("=== Study 2: No Oracle Template ===\n\n")
  cat("DGPs:", sort(unique(design$dgp)), "\n")
  cat("Methods:", sort(unique(design$method)), "\n")
  cat("Noise levels:", sort(unique(design$noise_sd)), "\n")
  cat("Severity levels:", sort(unique(design$severity)), "\n")
  cat("Total cells:", nrow(design), "\n")
  cat("Total runs:", sum(design$reps), "\n")
  cat("use_true_template:", unique(design$use_true_template), "\n")
}
