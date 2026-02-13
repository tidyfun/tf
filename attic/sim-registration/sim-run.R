# sim-run.R -- Main Runner for Registration Benchmark
#
# Usage:
#   Rscript sim-run.R [grid_name] [n_cores]
#
# Arguments:
#   grid_name: "core", "interaction", "sensitivity", "supplementary", "pilot", "all"
#   n_cores:   number of parallel cores (default: 4)
#
# Results saved incrementally per DGP to attic/sim-registration/results/

library(parallel)

`%||%` <- function(x, y) if (is.null(x)) y else x

base_dir <- here::here("attic", "sim-registration")

results_dir <- file.path(base_dir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

source(file.path(base_dir, "sim-dgp.R"))
source(file.path(base_dir, "sim-methods.R"))
source(file.path(base_dir, "sim-metrics.R"))
source(file.path(base_dir, "sim-config.R"))

# --- Task Definition ----------------------------------------------------------

#' Create task list from design table
#'
#' Each task = one (design_row, rep) combination.
#'
#' @param design data.frame from full_design() or a grid function
#' @return list of task specifications
create_tasks <- function(design) {
  tasks <- list()
  for (i in seq_len(nrow(design))) {
    row <- design[i, ]
    for (rep in seq_len(row$reps)) {
      task <- list(
        dgp = row$dgp,
        n_curves = row$n_curves,
        n_grid = row$n_grid,
        noise_sd = row$noise_sd,
        severity = row$severity,
        method = row$method,
        rep = rep,
        grid_name = row$grid_name,
        seed = make_seed(row$dgp, rep),
        use_true_template = if (!is.null(row$use_true_template)) {
          row$use_true_template
        } else {
          TRUE
        }
      )
      # Contamination fields (Study 3)
      if (!is.null(row$contam_frac)) {
        task$contam_frac <- row$contam_frac
        task$outlier_type <- row$outlier_type
      }
      tasks[[length(tasks) + 1]] <- task
    }
  }
  tasks
}

# --- Single Task Runner -------------------------------------------------------

#' Run one (data_generation + method_fit + metric_extraction) task
#'
#' @param task list with dgp, n_curves, n_grid, noise_sd, severity,
#'   method, rep, seed. Optional: contam_frac, outlier_type (for Study 3).
#' @return data.frame (1 row) with design info + all metrics
run_one_task <- function(task) {
  # Generate data
  data <- tryCatch(
    generate_data(
      dgp = task$dgp,
      n = task$n_curves,
      n_grid = task$n_grid,
      severity = task$severity,
      noise_sd = task$noise_sd,
      seed = task$seed
    ),
    error = function(e) {
      return(list(error = conditionMessage(e)))
    }
  )

  # Apply contamination if requested (Study 3)
  if (!is.null(data$x) && !is.null(task$contam_frac) && task$contam_frac > 0) {
    data <- tryCatch(
      contaminate_data(
        data,
        contam_frac = task$contam_frac,
        outlier_type = task$outlier_type,
        seed = task$seed
      ),
      error = function(e) {
        data$error <- conditionMessage(e)
        data
      }
    )
  }

  if (!is.null(data$error)) {
    return(data.frame(
      dgp = task$dgp,
      n_curves = task$n_curves,
      n_grid = task$n_grid,
      noise_sd = task$noise_sd,
      severity = task$severity,
      method = task$method,
      rep = task$rep,
      seed = task$seed,
      grid_name = task$grid_name,
      use_true_template = task$use_true_template %||% TRUE,
      contam_frac = task$contam_frac %||% NA_real_,
      outlier_type = task$outlier_type %||% NA_character_,
      warp_mise = NA,
      warp_mise_interior = NA,
      aligned_mise = NA,
      alignment_cc = NA,
      warp_roughness = NA,
      boundary_na_rate = NA,
      amplitude_rmse = NA,
      landmark_error = NA,
      time = NA,
      converged = FALSE,
      failure = TRUE,
      error_msg = data$error,
      stringsAsFactors = FALSE
    ))
  }

  # Fit method + extract metrics (wrapped to prevent worker crashes)
  metrics <- tryCatch(
    {
      result <- fit_method(
        data,
        task$method,
        use_true_template = task$use_true_template %||% TRUE
      )
      m <- extract_metrics(data, result)
      m$error_msg <- if (is.null(result$error)) "" else result$error
      m
    },
    error = function(e) {
      list(
        warp_mise = NA_real_,
        warp_mise_interior = NA_real_,
        aligned_mise = NA_real_,
        alignment_cc = NA_real_,
        warp_roughness = NA_real_,
        boundary_na_rate = NA_real_,
        amplitude_rmse = NA_real_,
        landmark_error = NA_real_,
        time = NA_real_,
        converged = FALSE,
        failure = TRUE,
        error_msg = conditionMessage(e)
      )
    }
  )

  # Assemble result row
  data.frame(
    dgp = task$dgp,
    n_curves = task$n_curves,
    n_grid = task$n_grid,
    noise_sd = task$noise_sd,
    severity = task$severity,
    method = task$method,
    rep = task$rep,
    seed = task$seed,
    grid_name = task$grid_name,
    use_true_template = task$use_true_template %||% TRUE,
    contam_frac = task$contam_frac %||% NA_real_,
    outlier_type = task$outlier_type %||% NA_character_,
    warp_mise = metrics$warp_mise,
    warp_mise_interior = metrics$warp_mise_interior,
    aligned_mise = metrics$aligned_mise,
    alignment_cc = metrics$alignment_cc,
    warp_roughness = metrics$warp_roughness,
    boundary_na_rate = metrics$boundary_na_rate,
    amplitude_rmse = metrics$amplitude_rmse,
    landmark_error = metrics$landmark_error,
    time = metrics$time,
    converged = metrics$converged,
    failure = metrics$failure,
    error_msg = metrics$error_msg,
    stringsAsFactors = FALSE
  )
}

# --- Batch Runner with Incremental Saves --------------------------------------

#' Run all tasks for one DGP, save results
#'
#' @param tasks list of task specs (all same DGP)
#' @param n_cores number of parallel cores
#' @param results_dir output directory
run_dgp_batch <- function(tasks, n_cores = 4, results_dir = results_dir) {
  dgp <- tasks[[1]]$dgp
  grid_name <- tasks[[1]]$grid_name
  use_tmpl <- tasks[[1]]$use_true_template %||% TRUE
  tmpl_tag <- if (!use_tmpl && !grepl("notemplate", grid_name))
    "_notemplate" else ""
  out_file <- file.path(
    results_dir,
    sprintf("results_%s_%s%s.rds", dgp, grid_name, tmpl_tag)
  )

  cat(sprintf(
    "[%s] Running %d tasks for %s/%s...\n",
    format(Sys.time(), "%H:%M:%S"),
    length(tasks),
    dgp,
    grid_name
  ))

  if (n_cores > 1) {
    results <- mclapply(
      tasks,
      run_one_task,
      mc.cores = n_cores,
      mc.preschedule = TRUE
    )
  } else {
    results <- lapply(tasks, run_one_task)
  }

  # Handle mclapply worker crashes (try-error or NULL elements)
  failed <- vapply(
    results,
    function(r) {
      inherits(r, "try-error") || is.null(r) || !is.data.frame(r)
    },
    logical(1)
  )

  if (any(failed)) {
    warning(sprintf(
      "%d/%d tasks crashed in %s/%s",
      sum(failed),
      length(tasks),
      dgp,
      grid_name
    ))
    for (i in which(failed)) {
      err_msg <- if (inherits(results[[i]], "try-error")) {
        as.character(results[[i]])
      } else {
        "Worker returned NULL or non-data.frame"
      }
      t <- tasks[[i]]
      results[[i]] <- data.frame(
        dgp = t$dgp,
        n_curves = t$n_curves,
        n_grid = t$n_grid,
        noise_sd = t$noise_sd,
        severity = t$severity,
        method = t$method,
        rep = t$rep,
        seed = t$seed,
        grid_name = t$grid_name,
        use_true_template = t$use_true_template %||% TRUE,
        contam_frac = t$contam_frac %||% NA_real_,
        outlier_type = t$outlier_type %||% NA_character_,
        warp_mise = NA,
        warp_mise_interior = NA,
        aligned_mise = NA,
        alignment_cc = NA,
        warp_roughness = NA,
        boundary_na_rate = NA,
        amplitude_rmse = NA,
        landmark_error = NA,
        time = NA,
        converged = FALSE,
        failure = TRUE,
        error_msg = err_msg,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine into data.frame
  results_df <- do.call(rbind, results)

  # Save incrementally
  saveRDS(results_df, out_file)
  cat(sprintf(
    "[%s] Saved %d rows to %s\n",
    format(Sys.time(), "%H:%M:%S"),
    nrow(results_df),
    basename(out_file)
  ))

  results_df
}

# --- Main Runner --------------------------------------------------------------

#' Run the full benchmark (or a subset)
#'
#' @param grid_name "core", "interaction", "sensitivity", "supplementary",
#'   "pilot", "all"
#' @param n_cores number of parallel cores
#' @param include_fda_crit1 include fda_crit1 config?
run_benchmark <- function(
  grid_name = "all",
  n_cores = 4,
  include_fda_crit1 = TRUE
) {
  cat(sprintf("=== Registration Benchmark ===\n"))
  cat(sprintf(
    "Grid: %s | Cores: %d | fda_crit1: %s\n",
    grid_name,
    n_cores,
    include_fda_crit1
  ))
  cat(sprintf("Start: %s\n\n", format(Sys.time())))

  # Get design
  design <- switch(
    grid_name,
    core = core_grid(),
    interaction = interaction_grid(),
    sensitivity = sensitivity_grid(),
    supplementary = supplementary_grid(),
    pilot = {
      # Pilot: 10 reps of core grid
      d <- core_grid()
      d$reps <- 10
      d
    },
    all = full_design(include_fda_crit1 = include_fda_crit1),
    notemplate = {
      source(file.path(base_dir, "sim-config-notemplate.R"), local = TRUE)
      notemplate_design()
    },
    notemplate_pilot = {
      source(file.path(base_dir, "sim-config-notemplate.R"), local = TRUE)
      d <- notemplate_design()
      d$reps <- 10
      d
    },
    outlier = {
      source(file.path(base_dir, "sim-config-study3.R"), local = TRUE)
      outlier_design()
    },
    outlier_pilot = {
      source(file.path(base_dir, "sim-config-study3.R"), local = TRUE)
      d <- outlier_design()
      d$reps <- 10
      d
    },
    realdata = {
      source(file.path(base_dir, "sim-config-study4.R"), local = TRUE)
      realdata_design()
    },
    realdata_pilot = {
      source(file.path(base_dir, "sim-config-study4.R"), local = TRUE)
      d <- realdata_design()
      d$reps <- 10
      d
    },
    cli::cli_abort("Unknown grid: {grid_name}")
  )

  if (!include_fda_crit1 && grid_name != "pilot") {
    design <- design[design$method != "fda_crit1", ]
  }

  # Create all tasks
  all_tasks <- create_tasks(design)
  cat(sprintf("Total tasks: %d\n\n", length(all_tasks)))

  # Group tasks by DGP + grid_name (+ outlier_type if present) for incremental saves
  task_groups <- split(
    all_tasks,
    sapply(all_tasks, function(t) {
      key <- paste(t$dgp, t$grid_name, sep = "_")
      if (!is.null(t$outlier_type)) {
        key <- paste(key, t$outlier_type, t$contam_frac, sep = "_")
      }
      key
    })
  )

  all_results <- list()
  t0 <- proc.time()

  for (group_name in names(task_groups)) {
    group_tasks <- task_groups[[group_name]]
    result_df <- run_dgp_batch(
      group_tasks,
      n_cores = n_cores,
      results_dir = results_dir
    )
    all_results[[group_name]] <- result_df
  }

  # Combine all results
  final_results <- do.call(rbind, all_results)
  rownames(final_results) <- NULL

  # Save combined results
  combined_file <- file.path(
    results_dir,
    sprintf("results_combined_%s.rds", grid_name)
  )
  saveRDS(final_results, combined_file)

  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n=== Benchmark Complete ===\n"))
  cat(sprintf(
    "Total time: %.1f minutes (%.1f hours)\n",
    elapsed / 60,
    elapsed / 3600
  ))
  cat(sprintf("Total rows: %d\n", nrow(final_results)))
  cat(sprintf("Failure rate: %.1f%%\n", 100 * mean(final_results$failure)))
  cat(sprintf("Results saved to: %s\n", combined_file))

  # Save session info
  writeLines(
    capture.output(sessionInfo()),
    file.path(results_dir, "sessionInfo.txt")
  )

  invisible(final_results)
}

# --- CLI Entry Point ----------------------------------------------------------

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  grid_name <- if (length(args) >= 1) args[1] else "pilot"
  n_cores <- if (length(args) >= 2) as.integer(args[2]) else 4

  cat(sprintf("Running benchmark: grid=%s, cores=%d\n", grid_name, n_cores))
  run_benchmark(grid_name = grid_name, n_cores = n_cores)
}
