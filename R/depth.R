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
  # TODO: Implement depths for partially observed functions instead of
  # interpolating them onto a common grid; see Elías et al. (2022),
  # "Integrated Depths for Partially Observed Functional Data",
  # doi:10.1080/10618600.2022.2070171.
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

# helper: compute depth values from a depth specification (string or function)
compute_depth <- function(x, depth, na.rm = TRUE, ...) {
  validate_depth(depth)
  depth_values <- if (is.function(depth)) {
    depth(x, ...)
  } else {
    tf_depth(x, depth = depth, na.rm = na.rm, ...)
  }

  if (!is.numeric(depth_values)) {
    cli::cli_abort(
      "{.arg depth} must return a numeric vector, not an object of class {.cls {class(depth_values)}}."
    )
  }

  n_x <- length(x)
  if (length(depth_values) == n_x) {
    return(depth_values)
  }

  complete <- !is.na(x)
  if (length(depth_values) == sum(complete)) {
    ret <- rep(NA_real_, n_x)
    ret[complete] <- depth_values
    return(ret)
  }

  cli::cli_abort(
    "{.arg depth} must return a numeric vector of length {n_x} or {sum(complete)}."
  )
}

# helper: validate depth argument -- either a known string or a function
validate_depth <- function(depth) {
  known <- c("MBD", "MHI", "FM", "FSD", "RPD")
  if (is.function(depth)) return(invisible(depth))
  if (is.character(depth) && length(depth) == 1 && depth %in% known) {
    return(invisible(depth))
  }
  cli::cli_abort(
    "{.arg depth} must be one of {.or {.val {known}}} or a function, not {.val {depth}}."
  )
}

depth_data <- function(x, depth, na.rm = FALSE, ...) {
  validate_depth(depth)
  if (!na.rm && anyNA(x))
    return(list(x = 1 * NA * x[which(is.na(x))[1]], d = NULL))

  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(x = x, d = NULL))

  list(x = x, d = compute_depth(x, depth, na.rm = TRUE, ...))
}

depth_extreme <- function(
  x,
  depth,
  which = c("min", "max"),
  na.rm = FALSE,
  ...
) {
  which <- match.arg(which)
  prepared <- depth_data(x, depth, na.rm = na.rm, ...)
  if (is.null(prepared$d)) return(prepared$x)

  idx <- switch(which, min = which.min(prepared$d), max = which.max(prepared$d))
  unname(prepared$x[idx])
}

#' Rank, order and sort `tf` vectors
#'
#' These methods use [tf_depth()] to rank, order, and sort functional data. By
#' default they use the modified hypograph index (`"MHI"`) which provides an
#' up-down ordering (lowest to highest). You can also use any of the other depth
#' methods available via [tf_depth()], or supply a custom depth function.
#'
#' @details
#' `rank` assigns ranks based on depth values: lower depth values get lower
#' ranks. For `"MHI"` this gives an ordering from lowest to highest function.
#' For centrality-based depths (`"MBD"`, `"FM"`, `"FSD"`, `"RPD"`), the most
#' extreme function gets rank 1 and the most central gets the highest rank.
#'
#' `order` returns the permutation which rearranges `x` into ascending
#' order according to depth.
#'
#' `sort.tf` returns the sorted `tf` vector.
#'
#' `xtfrm.tf` returns a numeric vector of MHI depth values, enabling
#' `base::order` and `base::rank` to work on `tf` vectors.
#'
#' @param x a `tf` vector.
#' @param depth the depth function to use for ranking. One of the depths
#'   available via [tf_depth()] (default: `"MHI"`) or a function that takes a
#'   `tf` vector and returns a numeric vector of depth values.
#' @param na.last for handling of `NA`s; see [base::rank()] and [base::sort()].
#' @param ties.method a character string for handling ties; see [base::rank()].
#' @param decreasing logical. Should the sort be decreasing?
#' @param ... passed to [tf_depth()] (e.g. `arg`).
#' @returns `rank`: a numeric vector of ranks.\cr
#'   `order`: an integer vector of indices.\cr
#'   `sort.tf`: a sorted `tf` vector.\cr
#'   `xtfrm.tf`: a numeric vector of depth values.
#' @examples
#' x <- tf_rgp(5) + 1:5
#' rank(x)
#' order(x)
#' sort(x)
#' # use a centrality-based depth instead:
#' rank(x, depth = "MBD")
#' @seealso [tf_depth()], [min.tf()], [max.tf()]
#' @family tidyfun ordering and ranking functions
#' @name tf_order
NULL

# Make rank generic so we can dispatch on tf objects
#' @rdname tf_order
#' @export
rank <- function(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  ...
) {
  UseMethod("rank")
}

#' @export
#' @rdname tf_order
rank.default <- function(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  ...
) {
  base::rank(x, na.last = na.last, ties.method = match.arg(ties.method), ...)
}

#' @rdname tf_order
#' @export
rank.tf <- function(
  x,
  na.last = TRUE,
  ties.method = c("average", "first", "last", "random", "max", "min"),
  depth = "MHI",
  ...
) {
  ties.method <- match.arg(ties.method)
  d <- compute_depth(x, depth, na.rm = FALSE, ...)
  base::rank(d, na.last = na.last, ties.method = ties.method)
}

#' @rdname tf_order
#' @export
xtfrm.tf <- function(x) {
  compute_depth(x, "MHI", na.rm = FALSE)
}

#' @rdname tf_order
#' @export
sort.tf <- function(x, decreasing = FALSE, na.last = NA, depth = "MHI", ...) {
  d <- compute_depth(x, depth, na.rm = FALSE, ...)
  o <- base::order(d, na.last = na.last, decreasing = decreasing)
  x[o]
}

#' Depth-based minimum, maximum and range for `tf` vectors
#'
#' By default, `min`, `max`, and `range` compute **pointwise** extremes (the
#' existing behaviour). When a `depth` argument is supplied, they instead return
#' the most extreme / most central observation according to the chosen depth.
#' For the default `"MHI"` depth this gives the lowest / highest function in an
#' up-down sense.
#'
#' @param ... `tf` objects (and `na.rm` for the pointwise default).
#' @param na.rm logical; passed on to the pointwise summary or used to filter
#'   `NA`s before computing depth.
#' @param depth depth method to use. `NULL` (default) gives the pointwise
#'   min/max/range. Supply a depth name (e.g. `"MHI"`) or a custom depth
#'   function for depth-based selection.
#' @returns a `tf` object.
#' @examples
#' x <- tf_rgp(5) + 1:5
#' # pointwise (default):
#' min(x)
#' max(x)
#' # depth-based:
#' min(x, depth = "MHI")
#' max(x, depth = "MHI")
#' @seealso [tf_depth()], [rank.tf()]
#' @family tidyfun ordering and ranking functions
#' @export
#' @name tf_minmax
min.tf <- function(..., na.rm = FALSE, depth = NULL) {
  if (is.null(depth)) {
    return(summarize_tf(
      ...,
      na.rm = na.rm,
      op = "min",
      eval = is_tfd(list(...)[[1]])
    ))
  }
  x <- vctrs::vec_c(...)
  depth_extreme(x, depth, which = "min", na.rm = na.rm)
}

#' @rdname tf_minmax
#' @export
max.tf <- function(..., na.rm = FALSE, depth = NULL) {
  if (is.null(depth)) {
    return(summarize_tf(
      ...,
      na.rm = na.rm,
      op = "max",
      eval = is_tfd(list(...)[[1]])
    ))
  }
  x <- vctrs::vec_c(...)
  depth_extreme(x, depth, which = "max", na.rm = na.rm)
}

#' @rdname tf_minmax
#' @export
range.tf <- function(..., na.rm = FALSE, depth = NULL) {
  c(
    min(..., na.rm = na.rm, depth = depth),
    max(..., na.rm = na.rm, depth = depth)
  )
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
