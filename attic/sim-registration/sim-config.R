# sim-config.R -- Experimental Design Tables for Registration Benchmark
#
# Provides:
#   core_grid()         - 4.1: main grid (5 DGPs x noise x severity x 7 methods)
#   interaction_grid()  - 4.2: noise x amplitude interaction
#   sensitivity_grid()  - 4.3: sensitivity arms
#   supplementary_grid()- 4.4: remaining DGPs (lower reps)
#   full_design()       - all grids combined
#
# Each grid returns a data.frame with columns:
#   dgp, n_curves, n_grid, noise_sd, severity, method, reps, grid_name

# --- Grid Builders -----------------------------------------------------------

#' 4.1 Core Grid (fully crossed, 100 reps)
#'
#' 5 DGPs x 2 noise x 2 severity x 6-7 methods = 124 cells x 100 reps = 12,400 runs
#' (S8 gets 7 methods incl. landmark_oracle; S1/S4/S5/S11 get 6 each)
core_grid <- function() {
  expand.grid(
    dgp = c("S1", "S4", "S5", "S8", "S11"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = c(0, 0.1),
    severity = c(0.5, 1.0),
    stringsAsFactors = FALSE
  ) |>
    add_methods_column() |>
    add_grid_info(reps = 100, grid_name = "core")
}

#' 4.2 Noise x Amplitude Interaction Grid (100 reps)
#'
#' 4 DGPs x 3 noise x 6 methods = 72 cells pre-dedup x 100 reps
#' After dedup from core: ~48 cells x 100 reps = 4,800 runs
interaction_grid <- function() {
  expand.grid(
    dgp = c("S4", "S5", "S6b", "S7"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = c(0, 0.1, 0.2),
    severity = 0.5,
    stringsAsFactors = FALSE
  ) |>
    add_methods_column(exclude = "landmark_oracle") |>
    add_grid_info(reps = 100, grid_name = "interaction") |>
    deduplicate_from_core()
}

#' 4.3 Sensitivity Arms (100 reps, S4 only)
#'
#' Grid density, severity, noise, n_curves variations.
#' Deduplicated: excludes levels already in core grid.
sensitivity_grid <- function() {
  base <- data.frame(
    dgp = "S4",
    n_curves = 50,
    n_grid = 101,
    noise_sd = 0,
    severity = 0.5,
    stringsAsFactors = FALSE
  )

  # Grid density: n_grid in {51, 201} (101 is in core)
  grid_density <- rbind(
    transform(base, n_grid = 51),
    transform(base, n_grid = 201)
  )

  # Severity: {0.2} (0.5, 1.0 are in core)
  severity_arm <- transform(base, severity = 0.2)

  # Noise: {0.05, 0.2} (0, 0.1 are in core)
  noise_arm <- rbind(
    transform(base, noise_sd = 0.05),
    transform(base, noise_sd = 0.2)
  )

  # n_curves: {20} (50 is in core)
  n_curves_arm <- transform(base, n_curves = 20)

  all_arms <- rbind(grid_density, severity_arm, noise_arm, n_curves_arm)
  all_arms |>
    add_methods_column(exclude = "landmark_oracle") |>
    add_grid_info(reps = 100, grid_name = "sensitivity")
}

#' 4.4 Supplementary DGPs (50 reps)
#'
#' Remaining DGPs not in core: S2, S3, S6a, S6b, S7, S9, S10
#' 6 DGPs x 6 methods + S9 x 7 methods = 43 cells x 50 reps = 2,150 runs
supplementary_grid <- function() {
  expand.grid(
    dgp = c("S2", "S3", "S6a", "S6b", "S7", "S9", "S10"),
    n_curves = 50,
    n_grid = 101,
    noise_sd = 0.05,
    severity = 0.5,
    stringsAsFactors = FALSE
  ) |>
    add_methods_column() |>
    add_grid_info(reps = 50, grid_name = "supplementary")
}

# --- Full Design --------------------------------------------------------------

#' Combine all grids into one design table
#'
#' @param include_fda_crit1 logical: include fda_crit1 config? (pilot-decide)
#' @return data.frame with all design rows
full_design <- function(include_fda_crit1 = TRUE) {
  design <- rbind(
    core_grid(),
    interaction_grid(),
    sensitivity_grid(),
    supplementary_grid()
  )

  if (!include_fda_crit1) {
    design <- design[design$method != "fda_crit1", ]
  }

  # Remove exact duplicates
  design <- unique(design)

  # Add row ID
  design$cell_id <- seq_len(nrow(design))

  message(sprintf(
    "Full design: %d cells, %d total runs",
    nrow(design),
    sum(design$reps)
  ))
  design
}

# --- Helpers ------------------------------------------------------------------

#' Add method column by expanding each row to all applicable methods
#' @keywords internal
add_methods_column <- function(grid, exclude = character(0)) {
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    methods <- applicable_methods(grid$dgp[i])
    methods <- setdiff(methods, exclude)
    cbind(
      grid[rep(i, length(methods)), , drop = FALSE],
      method = methods,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Add reps and grid_name columns
#' @keywords internal
add_grid_info <- function(grid, reps, grid_name) {
  grid$reps <- reps
  grid$grid_name <- grid_name
  rownames(grid) <- NULL
  grid
}

#' Remove rows from interaction grid that duplicate core grid cells
#' @keywords internal
deduplicate_from_core <- function(grid) {
  core <- core_grid()
  # Key: dgp + n_curves + n_grid + noise_sd + severity + method
  make_key <- function(df) {
    paste(
      df$dgp,
      df$n_curves,
      df$n_grid,
      df$noise_sd,
      df$severity,
      df$method,
      sep = "|"
    )
  }
  core_keys <- make_key(core)
  grid_keys <- make_key(grid)
  grid[!grid_keys %in% core_keys, ]
}

# --- Seed Generation ----------------------------------------------------------

#' Generate deterministic seed for a (dgp, rep) combination
#'
#' @param dgp character DGP name
#' @param rep integer repetition number
#' @return integer seed
make_seed <- function(dgp, rep) {
  # Deterministic hash-based seed
  digest::digest2int(paste(dgp, rep, sep = "_"))
}

# --- Applicable Methods (imported from sim-methods.R context) -----------------

# Inline version to avoid circular sourcing
applicable_methods <- function(dgp) {
  all_methods <- c(
    "srvf",
    "fda_default",
    "fda_crit1",
    "affine_shift",
    "affine_ss",
    "landmark_auto",
    "landmark_oracle"
  )
  if (dgp %in% c("S8", "S9", "R3b")) {
    return(all_methods)
  }
  setdiff(all_methods, "landmark_oracle")
}

# --- Design Summary -----------------------------------------------------------

#' Print summary of design
summarize_design <- function(design = NULL) {
  if (is.null(design)) design <- full_design()

  cat("=== Registration Benchmark Design ===\n\n")

  cat("Grid breakdown:\n")
  grid_summary <- aggregate(reps ~ grid_name, data = design, FUN = function(x) {
    c(cells = length(x), total_runs = sum(x))
  })
  print(grid_summary)

  cat("\nDGPs:", sort(unique(design$dgp)), "\n")
  cat("Methods:", sort(unique(design$method)), "\n")
  cat("Total cells:", nrow(design), "\n")
  cat("Total runs:", sum(design$reps), "\n")
}
