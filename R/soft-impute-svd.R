# copied from softImpute::Frob (v 1.4-1) under GPL-2
# original authors: Trevor Hastie <hastie@stanford.edu> and Rahul Mazumder <rahul.mazumder@gmail.com>
frob <- function(Uold, Dsqold, Vold, U, Dsq, V) {
  denom = sum(Dsqold^2)
  utu = Dsq * (t(U) %*% Uold)
  vtv = Dsqold * (t(Vold) %*% V)
  uvprod = sum(diag(utu %*% vtv))
  num = denom + sum(Dsq^2) - 2 * uvprod
  num/max(denom, 1e-09)
}

# code slightly adapted and shortened from softImpute::simpute.svd.R (v 1.4-1) under GPL-2
# original authors: Trevor Hastie <hastie@stanford.edu> and Rahul Mazumder <rahul.mazumder@gmail.com>
# adaptations:
#  - no warm starts, no returned call, no attributes
# inputs: x matrix of weighted (w/ integration weights) & centered (!) function evaluations, with NAs
# output: (regularized) SVD of x
simpute_svd <- function(x, J = min(dim(x)) - 1, thresh = 1e-05, lambda = 0, maxit = 100,
                        trace.it = FALSE, ...) {
  n <- dim(x)
  m <- n[2]
  n <- n[1]
  xnas <- is.na(x)

  nz <- m * n - sum(xnas)
  xfill <- x
  xfill[xnas] <- 0

  svd.xfill <- svd(xfill)
  ratio <- 1
  iter <- 0
  while ((ratio > thresh) & (iter < maxit)) {
    iter <- iter + 1
    svd.old <- svd.xfill
    d <- svd.xfill$d
    d <- pmax(d - lambda, 0)
    xhat <- svd.xfill$u[, seq(J)] %*% (d[seq(J)] * t(svd.xfill$v[, seq(J)]))
    xfill[xnas] <- xhat[xnas]
    svd.xfill <- svd(xfill)
    ratio <- frob(
      svd.old$u[, seq(J)], d[seq(J)], svd.old$v[, seq(J)],
      svd.xfill$u[, seq(J)], pmax(svd.xfill$d - lambda, 0)[seq(J)], svd.xfill$v[, seq(J)]
    )
    if (trace.it) {
      obj <- (.5 * sum((xfill - xhat)[!xnas]^2) + lambda * sum(d)) / nz
      cat(iter, ":", "obj", format(round(obj, 5)), "ratio", ratio, "\n")
    }
  }
  d <- pmax(svd.xfill$d[seq(J)] - lambda, 0)
  J <- min(sum(d > 0) + 1, J)
  svd.xfill <- list(u = svd.xfill$u[, seq(J)], d = d[seq(J)], v = svd.xfill$v[, seq(J)])
  if (iter == maxit) {
    warning(paste("Incomplete-data-SVD convergence not achieved by", maxit,
                  "iterations"), call. = FALSE)
  }
  svd.xfill
}


