# sim-analyze.R -- Analysis, Tables, and Figures for Registration Benchmark
#
# Usage:
#   source("sim-analyze.R")
#   results <- load_results()
#   make_tables(results)
#   make_figures(results)
#   run_statistical_analysis(results)

library(ggplot2)

base_dir <- here::here("attic", "sim-registration")

results_dir <- file.path(base_dir, "results")
figures_dir <- file.path(results_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Load and Prepare Results
# ==============================================================================

#' Load all results from RDS files
#'
#' @param grid_name "all", "core", etc. or NULL for auto-detect
#' @return data.frame of all results
load_results <- function(grid_name = NULL) {
  if (!is.null(grid_name)) {
    f <- file.path(results_dir, sprintf("results_combined_%s.rds", grid_name))
    if (file.exists(f)) return(readRDS(f))
  }

  # Auto-detect: load individual per-DGP result files (exclude combined files)
  rds_files <- list.files(
    results_dir,
    pattern = "^results_.*\\.rds$",
    full.names = TRUE
  )
  rds_files <- rds_files[!grepl("results_combined_", rds_files)]
  if (length(rds_files) == 0) {
    cli::cli_abort("No result files found in {results_dir}")
  }

  all_dfs <- lapply(rds_files, readRDS)
  results <- do.call(rbind, all_dfs)
  # Remove duplicates (from incremental + combined saves)
  results <- unique(results)
  rownames(results) <- NULL

  results$not_applicable <- FALSE

  cat(sprintf(
    "Loaded %d rows from %d files\n",
    nrow(results),
    length(rds_files)
  ))
  cat(sprintf("DGPs: %s\n", paste(sort(unique(results$dgp)), collapse = ", ")))
  cat(sprintf(
    "Methods: %s\n",
    paste(sort(unique(results$method)), collapse = ", ")
  ))
  cat(sprintf("Failure rate: %.1f%%\n", 100 * mean(results$failure)))

  results
}

#' Compute summary statistics with Monte Carlo SE
#'
#' @param results data.frame from load_results()
#' @param group_vars character vector of grouping variables
#' @param metric character: which metric to summarize
#' @return data.frame with mean, sd, se, n
summarize_metric <- function(results, group_vars, metric) {
  # Remove failures, N/A (landmark_auto on sigbump), and NAs in the target metric
  df <- results[!results$failure & !results$not_applicable, ]
  df <- df[!is.na(df[[metric]]), ]

  if (nrow(df) == 0) {
    return(data.frame(
      mean = numeric(0),
      sd = numeric(0),
      se = numeric(0),
      n = integer(0)
    ))
  }

  # Use aggregate to avoid the dot-in-rownames parsing bug
  formula <- as.formula(paste(metric, "~", paste(group_vars, collapse = " + ")))
  agg_mean <- aggregate(formula, data = df, FUN = mean)
  agg_sd <- aggregate(formula, data = df, FUN = function(x) {
    if (length(x) > 1) sd(x) else NA_real_
  })
  agg_n <- aggregate(formula, data = df, FUN = length)

  summary_df <- agg_mean
  names(summary_df)[names(summary_df) == metric] <- "mean"
  summary_df$sd <- agg_sd[[metric]]
  summary_df$n <- agg_n[[metric]]
  summary_df$se <- summary_df$sd / sqrt(summary_df$n)
  summary_df
}

# ==============================================================================
# Tables (Section 7)
# ==============================================================================

make_tables <- function(results) {
  cat("=== Generating Tables ===\n\n")

  # Table 1: Method x DGP heatmap of mean warp MISE
  cat("Table 1: Method x DGP warp MISE (core grid)\n")
  core_res <- results[results$grid_name == "core", ]
  t1 <- summarize_metric(core_res, c("dgp", "method"), "warp_mise")
  t1_wide <- reshape(
    t1[, c("dgp", "method", "mean")],
    idvar = "dgp",
    timevar = "method",
    direction = "wide"
  )
  names(t1_wide) <- gsub("mean\\.", "", names(t1_wide))
  print(t1_wide, digits = 4)
  cat("\n")

  # Table 2: Noise robustness (S4, all methods)
  cat("Table 2: Noise robustness for S4\n")
  s4_res <- results[results$dgp == "S4", ]
  t2 <- summarize_metric(s4_res, c("noise_sd", "method"), "warp_mise")
  t2_wide <- reshape(
    t2[, c("noise_sd", "method", "mean")],
    idvar = "noise_sd",
    timevar = "method",
    direction = "wide"
  )
  names(t2_wide) <- gsub("mean\\.", "", names(t2_wide))
  print(t2_wide, digits = 4)
  cat("\n")

  # Table 3: Severity response (S4, all methods)
  cat("Table 3: Severity response for S4\n")
  t3 <- summarize_metric(s4_res, c("severity", "method"), "warp_mise")
  t3_wide <- reshape(
    t3[, c("severity", "method", "mean")],
    idvar = "severity",
    timevar = "method",
    direction = "wide"
  )
  names(t3_wide) <- gsub("mean\\.", "", names(t3_wide))
  print(t3_wide, digits = 4)
  cat("\n")

  # Table 4: Amplitude interaction (S4-S7 x noise)
  cat("Table 4: Amplitude interaction\n")
  amp_dgps <- c("S4", "S5", "S6b", "S7")
  amp_res <- results[results$dgp %in% amp_dgps, ]
  t4 <- summarize_metric(amp_res, c("dgp", "noise_sd", "method"), "warp_mise")
  print(head(t4, 30), digits = 4)
  cat("...\n\n")

  # Save tables
  saveRDS(
    list(t1 = t1, t2 = t2, t3 = t3, t4 = t4),
    file.path(results_dir, "tables.rds")
  )
  cat("Tables saved to results/tables.rds\n")
}

# ==============================================================================
# Figures (Section 7)
# ==============================================================================

make_figures <- function(results) {
  cat("=== Generating Figures ===\n\n")

  core_res <- results[results$grid_name == "core" & !results$failure, ]

  # Figure 1: Heatmap - DGP x method, fill = log(warp MISE)
  cat("Figure 1: Warp MISE heatmap\n")
  heatmap_data <- summarize_metric(core_res, c("dgp", "method"), "warp_mise")
  heatmap_data$log_mise <- log10(heatmap_data$mean)

  p1 <- ggplot(heatmap_data, aes(x = method, y = dgp, fill = log_mise)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", mean)), size = 3) +
    scale_fill_viridis_c(name = "log10(MISE)", option = "plasma") +
    labs(title = "Warp MISE by Method and DGP", x = "Method", y = "DGP") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(figures_dir, "fig1_heatmap.pdf"), p1, width = 10, height = 6)

  # Figure 2: Boxplot array - warp MISE distributions per method
  cat("Figure 2: Warp MISE boxplots\n")
  p2 <- ggplot(core_res, aes(x = method, y = warp_mise, fill = method)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~dgp, scales = "free_y", ncol = 3) +
    scale_y_log10() +
    labs(
      title = "Warp MISE Distributions by Method and DGP",
      x = "Method",
      y = "Warp MISE (log scale)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ggsave(
    file.path(figures_dir, "fig2_boxplots.pdf"),
    p2,
    width = 12,
    height = 8
  )

  # Figure 3: Noise robustness curves
  cat("Figure 3: Noise robustness curves\n")
  s4_res <- results[results$dgp == "S4" & !results$failure, ]
  noise_data <- summarize_metric(s4_res, c("noise_sd", "method"), "warp_mise")
  noise_data$noise_sd <- as.numeric(noise_data$noise_sd)

  p3 <- ggplot(noise_data, aes(x = noise_sd, y = mean, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.01) +
    scale_y_log10() +
    labs(
      title = "Warp MISE vs Noise Level (DGP S4)",
      x = "Noise SD",
      y = "Warp MISE ± MC SE (log scale)"
    ) +
    theme_minimal()

  ggsave(
    file.path(figures_dir, "fig3_noise_robustness.pdf"),
    p3,
    width = 8,
    height = 5
  )

  # Figure 4: Severity response curves
  cat("Figure 4: Severity response curves\n")
  severity_data <- summarize_metric(
    s4_res,
    c("severity", "method"),
    "warp_mise"
  )
  severity_data$severity <- as.numeric(severity_data$severity)

  p4 <- ggplot(severity_data, aes(x = severity, y = mean, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.02) +
    scale_y_log10() +
    labs(
      title = "Warp MISE vs Severity (DGP S4)",
      x = "Severity",
      y = "Warp MISE ± MC SE (log scale)"
    ) +
    theme_minimal()

  ggsave(
    file.path(figures_dir, "fig4_severity_response.pdf"),
    p4,
    width = 8,
    height = 5
  )

  # Figure 5: Aligned MISE vs Warp MISE scatter
  cat("Figure 5: Aligned MISE vs Warp MISE scatter\n")
  scatter_data <- core_res[
    !is.na(core_res$aligned_mise) &
      !is.na(core_res$warp_mise),
  ]

  p5 <- ggplot(
    scatter_data,
    aes(x = warp_mise, y = aligned_mise, color = method)
  ) +
    geom_point(alpha = 0.3, size = 1) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Aligned MISE vs Warp MISE",
      x = "Warp MISE (log scale)",
      y = "Aligned MISE (log scale)"
    ) +
    theme_minimal()

  ggsave(file.path(figures_dir, "fig5_scatter.pdf"), p5, width = 8, height = 6)

  # Figure 6: Computation time bars
  cat("Figure 6: Computation time\n")
  time_data <- summarize_metric(core_res, c("method", "n_grid"), "time")
  time_data$time_per_curve <- time_data$mean / 50 # n_curves = 50 in core

  p6 <- ggplot(time_data, aes(x = method, y = time_per_curve, fill = method)) +
    geom_col() +
    facet_wrap(~n_grid, labeller = label_both) +
    labs(
      title = "Mean Time per Curve by Method",
      x = "Method",
      y = "Time per curve (seconds)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ggsave(file.path(figures_dir, "fig6_timing.pdf"), p6, width = 8, height = 5)

  # Figure 7: Landmark detection accuracy (if applicable)
  cat("Figure 7: Landmark detection accuracy\n")
  lm_data <- results[
    results$method == "landmark_auto" &
      !is.na(results$landmark_error),
  ]
  if (nrow(lm_data) > 0) {
    p7 <- ggplot(lm_data, aes(x = dgp, y = landmark_error, fill = dgp)) +
      geom_boxplot() +
      labs(
        title = "Landmark Detection Error by DGP",
        x = "DGP",
        y = "Mean Absolute Landmark Error"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggsave(
      file.path(figures_dir, "fig7_landmarks.pdf"),
      p7,
      width = 6,
      height = 4
    )
  }

  cat(sprintf("Figures saved to %s/\n", figures_dir))
}

# ==============================================================================
# Statistical Analysis (Section 7)
# ==============================================================================

#' Fit mixed-effects model and report results
run_statistical_analysis <- function(results) {
  cat("=== Statistical Analysis ===\n\n")

  # Core grid only for main analysis
  core_res <- results[results$grid_name == "core" & !results$failure, ]
  core_res$log_warp_mise <- log(core_res$warp_mise)

  # Remove infinite/NA values
  core_res <- core_res[is.finite(core_res$log_warp_mise), ]

  # Factorize
  core_res$method <- factor(core_res$method)
  core_res$dgp <- factor(core_res$dgp)
  core_res$noise_sd <- factor(core_res$noise_sd)
  core_res$severity <- factor(core_res$severity)
  core_res$rep <- factor(core_res$rep)

  # ANOVA: log(warp_MISE) ~ method * dgp * noise_sd + severity + (1|rep)
  cat("Model: log(warp_MISE) ~ method * dgp * noise_sd + severity\n\n")

  # Use linear model (lme4 may not be available)
  fit <- tryCatch(
    {
      if (requireNamespace("lme4", quietly = TRUE)) {
        lme4::lmer(
          log_warp_mise ~ method * dgp * noise_sd + severity + (1 | rep),
          data = core_res
        )
      } else {
        lm(log_warp_mise ~ method * dgp * noise_sd + severity, data = core_res)
      }
    },
    error = function(e) {
      message("Model fitting failed: ", e$message)
      lm(log_warp_mise ~ method + dgp + noise_sd + severity, data = core_res)
    }
  )

  cat("ANOVA table:\n")
  print(anova(fit))

  # Pairwise method comparisons (within each DGP)
  cat("\n\nMethod rankings by DGP (mean log warp MISE, lower = better):\n")
  rankings <- aggregate(log_warp_mise ~ dgp + method, data = core_res, mean)
  rankings <- rankings[order(rankings$dgp, rankings$log_warp_mise), ]
  for (dgp in unique(rankings$dgp)) {
    cat(sprintf("\n  %s:\n", dgp))
    dgp_ranks <- rankings[rankings$dgp == dgp, ]
    for (i in seq_len(nrow(dgp_ranks))) {
      cat(sprintf(
        "    %d. %s (%.3f)\n",
        i,
        dgp_ranks$method[i],
        dgp_ranks$log_warp_mise[i]
      ))
    }
  }

  # Save model
  saveRDS(fit, file.path(results_dir, "statistical_model.rds"))
  cat("\n\nModel saved to results/statistical_model.rds\n")

  invisible(fit)
}

# ==============================================================================
# Full Analysis Pipeline
# ==============================================================================

run_full_analysis <- function(grid_name = NULL) {
  results <- load_results(grid_name)
  make_tables(results)
  make_figures(results)
  run_statistical_analysis(results)

  # Session info
  cat("\n\n=== Session Info ===\n")
  print(sessionInfo())
}

# --- CLI Entry Point ----------------------------------------------------------

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  grid_name <- if (length(args) >= 1) args[1] else NULL
  run_full_analysis(grid_name)
}
