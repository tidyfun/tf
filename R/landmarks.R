#' Find Extrema Locations in Functional Data
#'
#' Find landmark locations for registration
#'
#' Detects local maxima, minima, and/or zero crossings in each function and
#' returns a landmark matrix suitable for [tf_register()] with
#' `method = "landmark"`. Uses position-based clustering across curves to
#' establish feature correspondence and majority-count filtering to discard
#' unstable landmarks.
#'
#' @param x a `tf` vector.
#' @param which character: which features to detect. Either `"all"` (maxima,
#'   minima, and zero crossings), `"both"` (maxima and minima), or any subset
#'   of `c("max", "min", "zero")`.
#' @param threshold numeric in (0, 1]: minimum proportion of curves that must
#'   contain a feature for it to be retained as a landmark. Defaults to `0.5`.
#' @param boundary_tol numeric: features within this distance of the domain
#'   boundary are dropped (they are redundant with the boundary anchors in
#'   landmark registration). Defaults to 2x the grid spacing. Set to `0` to
#'   keep all features.
#' @returns A numeric matrix with one row per function and one column per
#'   landmark, sorted left-to-right on the domain. Has attribute
#'   `"feature_types"` (character vector of `"max"`, `"min"`, or `"zero"` for
#'   each column). Contains `NA` where a curve is missing a landmark.
#' @seealso [tf_register()] with `method = "landmark"`
#' @export
#' @rdname landmarks
#' @family registration functions
#' @examples
#' t <- seq(0, 1, length.out = 101)
#' x <- tfd(t(sapply(c(0.3, 0.5, 0.7), function(s) dnorm(t, s, 0.1))), arg = t)
#' tf_landmarks_extrema(x, "max")
#' tf_landmarks_extrema(x, "both")
tf_landmarks_extrema <- function(
  x,
  which = "all",
  threshold = 0.5,
  boundary_tol = NULL
) {
  assert_tf(x)
  assert_number(threshold, lower = 0, upper = 1)
  if (!is.null(boundary_tol)) assert_number(boundary_tol, lower = 0)
  n <- length(x)

  # Parse `which`
  if (identical(which, "all")) {
    which <- c("max", "min", "zero")
  } else if (identical(which, "both")) {
    which <- c("max", "min")
  }
  assert_subset(which, c("max", "min", "zero"))

  domain <- tf_domain(x)
  # Get per-curve arg grids for feature detection
  arg_list <- tf_arg(x)
  if (!is.list(arg_list)) {
    # Regular tfd: single shared grid → replicate for uniform interface
    arg_list <- rep(list(as.numeric(arg_list)), n)
  }
  # Representative grid spacing (for bandwidth/boundary defaults)
  grid_spacing <- median(vapply(arg_list, \(a) median(diff(a)), numeric(1)))

  if (is.null(boundary_tol)) {
    boundary_tol <- 2 * grid_spacing
  }

  # --- Detect features per curve (each on its own grid) ---
  features <- detect_landmarks(x, arg_list, which)

  # --- Drop boundary features ---
  features <- lapply(features, \(f) {
    if (nrow(f) == 0) return(f)
    interior <- f$position > (domain[1] + boundary_tol) &
      f$position < (domain[2] - boundary_tol)
    f[interior, , drop = FALSE]
  })

  # --- Cluster features across curves ---
  # Bandwidth controls how far apart features (across different curves) can be
  # and still be considered the same landmark. Default: 15% of domain range,
  # which handles moderate warping. Increase for heavily warped data.
  bandwidth <- max(5 * grid_spacing, 0.15 * diff(domain))
  clusters <- cluster_landmarks(features, n, bandwidth, threshold)

  if (nrow(clusters) == 0) {
    cli::cli_warn(c(
      "No stable landmarks detected across curves.",
      "i" = "Pre-smoothing with {.fun tf_smooth} can help suppress spurious features."
    ))
    lm_mat <- matrix(NA_real_, nrow = n, ncol = 0)
    attr(lm_mat, "feature_types") <- character(0)
    return(lm_mat)
  }

  # --- Build landmark matrix ---
  lm_mat <- build_landmark_matrix(features, clusters, n, bandwidth)

  # --- Check for NAs ---
  n_na <- sum(is.na(lm_mat))
  if (n_na > 0) {
    na_per_col <- colSums(is.na(lm_mat))
    cols_with_na <- which(na_per_col > 0)
    cli::cli_warn(c(
      "{n_na} missing landmark{?s} across {length(cols_with_na)} column{?s}.",
      "i" = "Some curves lack features near {round(clusters$center[cols_with_na], 3)}.",
      "i" = "Pre-smoothing with {.fun tf_smooth} can help suppress spurious features."
    ))
  }

  lm_mat
}

# --- Landmark Detection Helpers ------------------------------------------------

#' @details
#' - `detect_landmarks` detects local extrema and zero crossings per curve.
#' @param x tf object (already smoothed if needed)
#' @param arg_list list of numeric vectors: per-curve evaluation grids
#' @param which character vector: subset of c("max", "min", "zero")
#' @returns `detect_landmarks`: list of n data.frames with columns (position, type)
#' @keywords internal
#' @rdname landmarks
detect_landmarks <- function(x, arg_list, which) {
  x_evals <- tf_evaluations(x)
  n <- length(x)
  need_extrema <- any(c("max", "min") %in% which)
  need_zero <- "zero" %in% which

  lapply(seq_len(n), \(i) {
    vals <- x_evals[[i]]
    arg_i <- arg_list[[i]]
    positions <- numeric(0)
    types <- character(0)

    if (need_extrema) {
      dv <- diff(vals)
      # Compare sign of consecutive differences to find local extrema.
      # Local max: slope positive then negative (dv[j] > 0, dv[j+1] < 0).
      # Local min: slope negative then positive (dv[j] < 0, dv[j+1] > 0).
      # Extremum is at arg_i[j + 1] (the middle point of the 3-point window).
      for (j in seq_len(length(dv) - 1)) {
        if ("max" %in% which && dv[j] > 0 && dv[j + 1] < 0) {
          positions <- c(positions, arg_i[j + 1])
          types <- c(types, "max")
        }
        if ("min" %in% which && dv[j] < 0 && dv[j + 1] > 0) {
          positions <- c(positions, arg_i[j + 1])
          types <- c(types, "min")
        }
      }
    }

    if (need_zero) {
      for (j in seq_len(length(vals) - 1)) {
        if (vals[j] * vals[j + 1] < 0) {
          # Strict sign change: interpolate zero position
          pos <- arg_i[j] -
            vals[j] * (arg_i[j + 1] - arg_i[j]) / (vals[j + 1] - vals[j])
          positions <- c(positions, pos)
          types <- c(types, "zero")
        } else if (
          vals[j] == 0 &&
            j > 1 &&
            j < length(vals) - 1 &&
            sign(vals[j - 1]) != sign(vals[j + 1]) &&
            vals[j - 1] != 0 &&
            vals[j + 1] != 0
        ) {
          # Exact zero at grid point with sign change around it
          positions <- c(positions, arg_i[j])
          types <- c(types, "zero")
        }
      }
    }

    ord <- order(positions)
    data.frame(
      position = positions[ord],
      type = types[ord],
      stringsAsFactors = FALSE
    )
  })
}


#' @details
#' - `cluster_landmarks` clusters within each feature type separately
#' (max with max, min with min, etc.) to avoid merging adjacent features of
#' different types. Then combines and sorts by position.
#' @param features list of per-curve data.frames from detect_landmarks()
#' @param n number of curves
#' @param bandwidth merge distance for clustering
#' @param threshold minimum proportion of curves for a cluster to be retained
#' @returns `cluster_landmarks`: data.frame with columns: center, type, count, proportion
#' @keywords internal
#' @rdname landmarks
cluster_landmarks <- function(features, n, bandwidth, threshold) {
  # Pool all features with curve ID
  all_f <- do.call(
    rbind,
    lapply(seq_along(features), \(i) {
      f <- features[[i]]
      if (nrow(f) == 0) return(NULL)
      data.frame(
        position = f$position,
        type = f$type,
        curve = i,
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(all_f) || nrow(all_f) == 0) {
    return(data.frame(
      center = numeric(0),
      type = character(0),
      count = integer(0),
      proportion = numeric(0)
    ))
  }

  # Cluster within each feature type separately, then combine
  feature_types <- unique(all_f$type)
  all_clusters <- list()

  for (ftype in feature_types) {
    sub <- all_f[all_f$type == ftype, , drop = FALSE]
    sub <- sub[order(sub$position), ]
    if (nrow(sub) == 0) next

    # Greedy clustering: merge features within bandwidth of cluster center
    cur <- list(positions = sub$position[1], curves = sub$curve[1])

    for (i in seq_len(nrow(sub))[-1]) {
      if (sub$position[i] - mean(cur$positions) <= bandwidth) {
        cur$positions <- c(cur$positions, sub$position[i])
        cur$curves <- c(cur$curves, sub$curve[i])
      } else {
        all_clusters[[length(all_clusters) + 1]] <- list(
          center = mean(cur$positions),
          type = ftype,
          count = length(unique(cur$curves))
        )
        cur <- list(positions = sub$position[i], curves = sub$curve[i])
      }
    }
    all_clusters[[length(all_clusters) + 1]] <- list(
      center = mean(cur$positions),
      type = ftype,
      count = length(unique(cur$curves))
    )
  }

  result <- data.frame(
    center = vapply(all_clusters, `[[`, numeric(1), "center"),
    type = vapply(all_clusters, `[[`, character(1), "type"),
    count = vapply(all_clusters, `[[`, integer(1), "count"),
    stringsAsFactors = FALSE
  )
  result$proportion <- result$count / n

  # Filter by threshold, then sort by position
  result <- result[result$proportion >= threshold, , drop = FALSE]
  result[order(result$center), , drop = FALSE]
}

#' @details
#' - `build_landmark_matrix` creates a landmark matrix by matching per-curve
#'  features to clusters.
#' @param features list of per-curve data.frames
#' @param clusters data.frame from cluster_landmarks()
#' @param n number of curves
#' @param bandwidth matching distance
#' @returns `build_landmark_matrix`: n x k matrix with feature_types attribute
#' @keywords internal
#' @rdname landmarks
build_landmark_matrix <- function(features, clusters, n, bandwidth) {
  k <- nrow(clusters)
  lm_mat <- matrix(NA_real_, nrow = n, ncol = k)

  for (i in seq_len(n)) {
    f <- features[[i]]
    if (nrow(f) == 0) next
    used <- logical(nrow(f))
    prev_pos <- -Inf
    for (j in seq_len(k)) {
      # Find features of matching type within bandwidth, not yet assigned,
      # and strictly after the previous matched position (ensures monotonicity)
      matches <- which(
        abs(f$position - clusters$center[j]) <= bandwidth &
          f$type == clusters$type[j] &
          !used &
          f$position > prev_pos
      )
      if (length(matches) > 0) {
        best <- matches[which.min(abs(
          f$position[matches] - clusters$center[j]
        ))]
        lm_mat[i, j] <- f$position[best]
        used[best] <- TRUE
        prev_pos <- f$position[best]
      }
    }
  }

  attr(lm_mat, "feature_types") <- clusters$type
  lm_mat
}
