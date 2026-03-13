#' Functional Data Depth
#'
#' Data depths for functional data. All depths are scaled so that 1 means most
#' central and 0 means most extreme. Available methods:
#'
#' - `"MBD"`: Modified Band-2 Depth (default). Scale of 0 (most extreme) to
#'   1 (most central).
#' - `"MHI"`: Modified Hypograph Index. **Ranks functions from lowest (0) to
#'   highest (1)** instead of most extreme to most central! For functions that
#'   never cross: \eqn{MBD = -2(MHI - 0.5)^2 + .5}.
#' - `"FM"`: Fraiman-Muniz depth. Integrates pointwise univariate halfspace
#'   depths over the domain. Scale of 0 (most extreme) to 1 (most central).
#' - `"FSD"`: Functional Spatial Depth. Based on spatial signs; robust to
#'   outliers. Scale of 0 (most extreme) to 1 (most central).
#' - `"RPD"`: Regularized Projection Depth. Projects curves onto random
#'   directions and computes outlyingness. Especially useful for detecting
#'   shape outliers. Scale of 0 (most extreme) to 1 (most central).
#'   **Note:** results depend on the RNG state; set a seed
#'   (e.g. `set.seed(...)`) before calling for reproducibility. Accepts
#'   additional arguments via `...`: `u` (quantile level for regularization,
#'   default 0.01), `n_projections` (M, number of projection directions,
#'   default 5000), `n_projections_beta` (L, directions for estimating
#'   regularization parameter, default 500).
#'
#' @param x `tf` (or a matrix of evaluations).
#' @param depth one of `"MBD"`, `"MHI"`, `"FM"`, `"FSD"`, or `"RPD"`.
#' @param arg grid of evaluation points.
#' @param na.rm remove missing observations? Defaults to `TRUE`.
#' @param ... for `"RPD"`: `u` (regularization quantile, default 0.01),
#'   `n_projections` (M, default 5000), `n_projections_beta` (L, default
#'   500).
#' @returns vector of depth values
#' @references `r format_bib("sun2012exact", "lopez2009concept", "lopez2011half", "fraiman2001trimmed", "chakraborty2014spatial", "bocinec2026rpd")`
#' @examples
#' x <- tf_rgp(3)/3 + 1:3
#' tf_depth(x, depth = "MBD")
#' tf_depth(x, depth = "MHI")
#' tf_depth(x, depth = "FM")
#' tf_depth(x, depth = "FSD")
#' @export
#' @rdname tf_depth
#' @family tidyfun ordering and ranking functions
tf_depth <- function(x, arg, depth = "MBD", na.rm = TRUE, ...) {
  UseMethod("tf_depth")
}

#' @export
#' @rdname tf_depth
tf_depth.matrix <- function(
  x,
  arg,
  depth = c("MBD", "MHI", "FM", "FSD", "RPD"),
  na.rm = TRUE,
  ...
) {
  if (missing(arg)) arg <- unlist(find_arg(x, arg = NULL), use.names = FALSE)
  assert_numeric(
    arg,
    finite = TRUE,
    any.missing = FALSE,
    len = ncol(x),
    unique = TRUE,
    sorted = TRUE
  )

  if (na.rm) {
    x <- x[stats::complete.cases(x), , drop = FALSE]
  }

  depth <- match.arg(depth)
  switch(
    depth,
    MBD = 2 * mbd(x, arg),
    MHI = mhi(x, arg),
    FM = fm(x, arg),
    FSD = fsd(x, arg),
    RPD = rpd(x, arg, ...)
  )
}

#' @export
#' @rdname tf_depth
tf_depth.tf <- function(x, arg, depth = "MBD", na.rm = TRUE, ...) {
  if (!missing(arg)) assert_arg_vector(arg, x)
  # TODO: warn if irreg?
  if (na.rm) x <- x[!is.na(x)]
  tf_depth(
    as.matrix(x, arg = arg, interpolate = TRUE),
    depth = depth,
    na.rm = na.rm,
    ...
  )
}

#-------------------------------------------------------------------------------

# trapezoidal integration weights normalized to sum to 1
trap_weights <- function(arg) {
  lengths <- diff(arg) / 2
  (c(lengths, 0) + c(0, lengths)) / diff(range(arg))
}

# modified band-2 depth:
mbd <- function(x, arg = seq_len(ncol(x))) {
  if (nrow(x) == 1) return(0.5)
  if (nrow(x) == 2) return(c(0.5, 0.5))

  # algorithm of Sun/Genton/Nychka (2012)
  # TODO: does this need "ties.method = min" or max instead?
  ranks <- apply(x, 2, rank, na.last = "keep", ties.method = "average")
  weights <- trap_weights(arg)
  n <- nrow(ranks)
  tmp <- colSums(t((n - ranks) * (ranks - 1)) * weights, na.rm = TRUE)
  (tmp + n - 1) / choose(n, 2)
}

# modified hypograph index
# adapted from roahd:::MHI.default
mhi <- function(x, arg = seq_len(ncol(x))) {
  if (nrow(x) == 1) return(0.5)
  n <- nrow(x)
  weights <- trap_weights(arg)
  ranks <- apply(x, 2, rank, na.last = "keep", ties.method = "max")
  colSums(t(ranks) * weights) / n
}


# Fraiman-Muniz depth (integrated univariate halfspace depth):
# Fraiman, R. and Muniz, G. (2001)
fm <- function(x, arg = seq_len(ncol(x))) {
  n <- nrow(x)
  if (n == 1) return(1)
  # Use the upper empirical CDF F_n(x) = P(X <= x), which preserves
  # permutation invariance under ties and matches reference implementations.
  ranks <- apply(x, 2, rank, na.last = "keep", ties.method = "max")
  weights <- trap_weights(arg)
  # pointwise univariate halfspace depth: min(F_n, 1-F_n) where F_n = rank/n
  pw_depth <- pmin(ranks / n, 1 - ranks / n)
  # integrate over domain with trapezoidal weights, scale to [0, 1]
  2 * colSums(t(pw_depth) * weights, na.rm = TRUE)
}

# Functional spatial depth:
# Chakraborty, A. and Chaudhuri, P. (2014)
fsd <- function(x, arg = seq_len(ncol(x))) {
  n <- nrow(x)
  if (n == 1) return(1)
  weights <- trap_weights(arg)
  w_norm <- function(v) sqrt(sum(v^2 * weights))

  depths <- numeric(n)
  p <- ncol(x)
  for (i in seq_len(n)) {
    # accumulate average spatial sign directly (avoid n x p matrix allocation)
    avg_sign <- numeric(p)
    for (j in seq_len(n)) {
      if (j == i) next
      diff_ij <- x[i, ] - x[j, ]
      norm_ij <- w_norm(diff_ij)
      if (norm_ij > 0) avg_sign <- avg_sign + diff_ij / norm_ij
    }
    avg_sign <- avg_sign / n
    depths[i] <- 1 - w_norm(avg_sign)
  }
  names(depths) <- rownames(x)
  depths
}

# Regularized projection depth:
# Bočinec, F., Nagy, S., and Yeon, H. (2026)
# Note: uses random projections -- set RNG seed before calling for
# reproducible results.
rpd <- function(
  x,
  arg = seq_len(ncol(x)),
  u = 0.01,
  n_projections = 5000L,
  n_projections_beta = 500L
) {
  assert_number(u, lower = 0, upper = 1)
  assert_count(n_projections, positive = TRUE)
  assert_count(n_projections_beta, positive = TRUE)

  n <- nrow(x)
  p <- ncol(x)
  if (n == 1) return(1)

  sqrt_weights <- sqrt(trap_weights(arg))

  # work in weighted L2 space: multiply columns by sqrt(weights)
  xw <- t(t(x) * sqrt_weights)

  # sample L random directions on unit sphere (in weighted space)
  dirs_beta <- matrix(
    stats::rnorm(n_projections_beta * p),
    n_projections_beta,
    p
  )
  dirs_beta <- dirs_beta / sqrt(rowSums(dirs_beta^2))

  # project data onto each direction
  projs_beta <- dirs_beta %*% t(xw) # L x n

  # compute MAD of projections for each direction
  mads <- apply(projs_beta, 1, stats::mad, constant = 1)

  # beta = u-quantile of MADs
  beta <- stats::quantile(mads, probs = u, names = FALSE)

  # sample M directions from V_beta (directions with MAD >= beta)
  dirs <- matrix(nrow = 0, ncol = p)
  attempts <- 0
  while (nrow(dirs) < n_projections && attempts < 100) {
    batch_size <- max(n_projections * 2, 1000)
    candidates <- matrix(stats::rnorm(batch_size * p), batch_size, p)
    candidates <- candidates / sqrt(rowSums(candidates^2))
    projs_c <- candidates %*% t(xw)
    mads_c <- apply(projs_c, 1, stats::mad, constant = 1)
    keep <- mads_c >= beta
    if (any(keep)) dirs <- rbind(dirs, candidates[keep, , drop = FALSE])
    attempts <- attempts + 1
  }
  if (nrow(dirs) == 0) {
    cli::cli_warn(
      "No directions satisfied MAD threshold; using unregularized directions."
    )
    dirs <- dirs_beta
  } else {
    if (nrow(dirs) < n_projections) {
      cli::cli_warn(
        "Only {nrow(dirs)}/{n_projections} directions found after {attempts} attempts."
      )
    }
    dirs <- dirs[seq_len(min(nrow(dirs), n_projections)), , drop = FALSE]
  }

  # compute projections
  projs <- dirs %*% t(xw) # M x n

  # for each direction, compute outlyingness of each observation
  meds <- apply(projs, 1, stats::median)
  mads_dir <- apply(projs, 1, stats::mad, constant = 1)

  # outlyingness: |<x,v> - med| / MAD
  outlyingness <- abs(projs - meds) / pmax(mads_dir, .Machine$double.eps)

  # depth = min over directions of (1 + O_v)^{-1}
  depth_per_dir <- 1 / (1 + outlyingness) # M x n
  apply(depth_per_dir, 2, min)
}

#------------------------------------------------------------------------------

#' @importFrom stats quantile
#' @inheritParams stats::quantile
#' @family tidyfun ordering and ranking functions
#' @export
quantile.tf <- function(
  x,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  names = TRUE,
  type = 7,
  ...
) {
  # TODO: functional quantiles will need (a lot) more thought,
  # cf. Serfling, R., & Wijesuriya, U. (2017).
  # Depth-based nonparametric description of functional data,
  #   with emphasis on use of spatial depth.
  cli::cli_inform(c(
    i = "Only pointwise, non-functional quantiles implemented for {.cls tf}s."
  ))
  summarize_tf(
    x,
    probs = probs,
    na.rm = na.rm,
    names = names,
    type = type,
    op = "quantile",
    eval = is_tfd(x),
    ...
  )
}
