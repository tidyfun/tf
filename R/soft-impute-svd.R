# Soft-impute SVD: iteratively SVD a matrix with NAs, replacing NAs by their
# rank-J reconstruction (with optionally soft-thresholded singular values) until
# the Frobenius-distance of consecutive rank-J factors converges. Used by
# `fpc_wsvd()` to do FPCA on partially missing data on a common grid.

# Squared Frobenius distance between two rank-J factorizations
# (U1 diag(d1) V1') and (U2 diag(d2) V2'), normalized by ||U1 diag(d1) V1'||_F^2.
# Computed via the inner-product identity to avoid forming the full products.
.simpute_frob_ratio <- function(u1, d1, v1, u2, d2, v2) {
  denom <- sum(d1^2)
  # cross = tr( diag(d2) (u2' u1) diag(d1) (v1' v2) ), computed as sum(A * t(B))
  cross <- sum((d2 * crossprod(u2, u1)) * t(d1 * crossprod(v1, v2)))
  (denom + sum(d2^2) - 2 * cross) / max(denom, 1e-9)
}

# Soft-impute SVD as used in fpc_wsvd() for matrices with NAs.
#
# inputs:
#   x       : numeric matrix of (weighted, centered) function evaluations,
#             entries may be NA
#   J       : truncation rank used for the imputation reconstruction
#   thresh  : convergence threshold on the relative Frobenius change
#   lambda  : soft-threshold applied to singular values (0 = hard rank-J)
#   maxit   : iteration cap
# output: list(u, d, v) with non-zero singular values (and a final +1 buffer),
#         analogous to base::svd() but operating on an NA-filled matrix.
simpute_svd <- function(x,
                        J = min(dim(x)) - 1,
                        thresh = 1e-5,
                        lambda = 0,
                        maxit = 100,
                        ...) {
  J <- as.integer(J)
  nas <- is.na(x)
  if (!any(nas)) {
    s <- svd(x)
    keep <- seq_len(min(J, length(s$d)))
    return(list(u = s$u[, keep, drop = FALSE],
                d = pmax(s$d[keep] - lambda, 0),
                v = s$v[, keep, drop = FALSE]))
  }

  # Initial fill: zeros. Callers pass column-centered data, so zero is the
  # column mean. This is a standard warm start for soft-impute.
  filled <- x
  filled[nas] <- 0

  s_prev <- svd(filled)
  idx <- seq_len(J)

  for (iter in seq_len(maxit)) {
    d_thr <- pmax(s_prev$d - lambda, 0)
    # rank-J reconstruction; impute the missing cells with it
    xhat <- s_prev$u[, idx, drop = FALSE] %*%
      (d_thr[idx] * t(s_prev$v[, idx, drop = FALSE]))
    filled[nas] <- xhat[nas]

    s_new <- svd(filled)
    d_new_thr <- pmax(s_new$d - lambda, 0)

    ratio <- .simpute_frob_ratio(
      s_prev$u[, idx, drop = FALSE], d_thr[idx], s_prev$v[, idx, drop = FALSE],
      s_new$u[, idx, drop = FALSE], d_new_thr[idx], s_new$v[, idx, drop = FALSE]
    )

    s_prev <- s_new
    if (ratio < thresh) break
  }

  if (iter == maxit && ratio >= thresh) {
    cli::cli_warn(
      "Convergence not achieved in {maxit} iterations for incomplete-data SVD."
    )
  }

  d_final <- pmax(s_prev$d[idx] - lambda, 0)
  keep <- min(sum(d_final > 0) + 1, J)
  keep_idx <- seq_len(keep)
  list(u = s_prev$u[, keep_idx, drop = FALSE],
       d = d_final[keep_idx],
       v = s_prev$v[, keep_idx, drop = FALSE])
}
