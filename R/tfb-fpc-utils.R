#' Eigenfunctions via weighted, regularized SVD
#'
#' Compute (truncated) orthonormal eigenfunctions and scores
#' for (partially missing) data on a common (potentially non-equidistant) grid.
#'
#' Performs a weighted SVD with trapezoidal quadrature weights s.t. returned
#' vectors represent (evaluations of)
#' orthonormal eigen*functions* \eqn{\phi_j(t)}, not eigen*vectors*
#' \eqn{\phi_j = (\phi_j(t_1), \dots, \phi_j(t_n))}, specifically:\cr
#' \eqn{\int_T \phi_j(t)^2 dt \approx \sum_i \Delta_i \phi_j(t_i)^2 = 1}
#' given quadrature weights \eqn{\Delta_i}, not
#' \eqn{\phi_j'\phi_j = \sum_i \phi_j(t_i)^2 = 1};\cr
#' \eqn{\int_T \phi_j(t) \phi_k(t) dt = 0} not
#' \eqn{\phi_j'\phi_k = \sum_i \phi_j(t_i)\phi_k(t_i) = 0},
#' c.f. `mogsa::wsvd()`.\cr
#' For incomplete data, this uses an adaptation of `softImpute::softImpute()`,
#' see references. Note that will not work well for data on a common grid if more
#' than a few percent of data points are missing, and it breaks down completely
#' for truly irregular data with no/few common timepoints, even if observed very
#' densely. For such data, either re-evaluate on a common grid first or use more
#' advanced FPCA approaches like `refund::fpca_sc()`,
#' see last example for [tfb_fpc()]
#'
#' @param data numeric matrix of function evaluations
#'   (each row is one curve, no NAs)
#' @param arg numeric vector of argument values
#' @param pve percentage of variance explained
#' @returns a list with entries
#' - `mu` estimated mean function (numeric vector)
#' - `efunctions` estimated FPCs (numeric matrix, columns represent FPCs)
#' - `scores` estimated FPC scores (one row per observed curve)
#' - `npc` how many FPCs were returned for the given `pve` (integer)
#' - `scoring_function` a function that returns FPC scores for new data
#'    and given eigenfunctions, see `tf:::.fpc_wsvd_scores` for an example.
#' @author Trevor Hastie, Rahul Mazumder, Cheng Meng, Fabian Scheipl
#' @references code adapted from / inspired by `mogsa::wsvd()` by Cheng Meng
#'   and `softImpute::softImpute()` by Trevor Hastie and Rahul Mazumder.\cr
#' `r format_bib("meng2023mogsa")`\cr
#' `r format_bib("mazumder2010")`\cr
#' `r format_bib("softimpute")`
#' @family tfb-class
#' @family tfb_fpc-class
fpc_wsvd <- function(data, arg, pve = 0.995) {
  UseMethod("fpc_wsvd")
}

#' @rdname fpc_wsvd
#' @importFrom utils head tail
#' @importFrom stats lowess
#' @export
fpc_wsvd.matrix <- function(data, arg, pve = 0.995) {
  assert_matrix(data, mode = "numeric", min.cols = 2, min.rows = 1)
  assert_numeric(arg, any.missing = FALSE, sorted = TRUE, len = ncol(data))
  assert_number(pve, lower = 0, upper = 1)

  delta <- c(0, diff(arg))
  # trapezoid integration weights:
  weights <- 0.5 * c(delta[-1] + head(delta, -1), tail(delta, 1))
  mean <- colMeans(data, na.rm = TRUE)

  data_wc <- t((t(data) - mean) * sqrt(weights))

  nas <- is.na(data)
  pc <- if (!any(nas)) {
    svd(data_wc, nu = 0, nv = min(dim(data)))
  } else {
    message("Using softImpute SVD on ", round(mean(nas)*100, 1), "% missing data")
    if (pve + mean(nas) > 1) {
      warning("High <pve> with many missings likely to yield bad FPC estimates.",
              call. = FALSE)
    }
    simpute_svd(data_wc)
  }

  pve_observed <- cumsum(pc$d^2) / sum(pc$d^2)
  use <- min(which(pve_observed >= pve))

  efunctions <- pc$v[, 1:use] / sqrt(weights)

  if (any(nas)) {
    # slightly smooth efunctions from incomplete data to reduce artefacts
    efunctions <- apply(efunctions, 2,
                        \(ef) stats::lowess(x = arg, y = ef,  f = .15)$y)
  }
  evalues <- (pc$d[1:use])^2
  scores <- .fpc_wsvd_scores(data, efunctions, mean, weights) #!!

  list(
    mu = mean, efunctions = efunctions,
    scores = scores, npc = use, evalues = evalues,
    error_variance = cumsum((pc$d^2)[-(1:use)]),
    scoring_function = .fpc_wsvd_scores
  )
}

# extract scoring function for reuse in tf_rebase
# performs simple (weighted) LS fit of data onto the eigenfunctions.
.fpc_wsvd_scores <- function(data_matrix, efunctions, mean, weights) {
  w_mat <- matrix(weights, ncol = length(weights), nrow = nrow(data_matrix),
                  byrow = TRUE)
  w_mat[is.na(data_matrix)] <- 0
  data_matrix[is.na(data_matrix)] <- 0
  data_wc <- t((t(data_matrix) - mean) * sqrt(t(w_mat)))
  t(qr.coef(qr(efunctions), t(data_wc) / sqrt(weights)))
}


#' @rdname fpc_wsvd
#' @export
fpc_wsvd.data.frame <- function(data, arg, pve = 0.995) {
  data_mat <- df_2_mat(data)
  fpc_wsvd.matrix(data_mat, arg = attr(data_mat, "arg"), pve = pve)
}

#---------------------------------------------------------------------------

fpc_wrapper <- function(efunctions) {
  function(arg) {
    t(efunctions[, arg, interpolate = TRUE, matrix = TRUE])
  }
}
