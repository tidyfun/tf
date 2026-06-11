test_that("simpute_svd matches svd() on a complete matrix with lambda = 0", {
  set.seed(42)
  x <- matrix(rnorm(60), 12, 5)
  s_ref <- svd(x)
  s_new <- simpute_svd(x)
  # leading min(dim)-1 singular values should match (signs of u/v can flip,
  # but their outer products / d should be identical)
  k <- length(s_new$d)
  expect_equal(s_new$d, s_ref$d[seq_len(k)], tolerance = 1e-10)
  expect_equal(
    s_new$u %*% (s_new$d * t(s_new$v)),
    s_ref$u[, seq_len(k), drop = FALSE] %*%
      (s_ref$d[seq_len(k)] * t(s_ref$v[, seq_len(k), drop = FALSE])),
    tolerance = 1e-10
  )
})

test_that("simpute_svd imputed entries equal the rank-J reconstruction", {
  set.seed(7)
  n <- 25
  m <- 8
  x_full <- tcrossprod(matrix(rnorm(n * 3), n, 3), matrix(rnorm(m * 3), m, 3))
  x_full <- x_full + 0.01 * matrix(rnorm(n * m), n, m)
  x <- x_full
  nas <- matrix(runif(n * m) < 0.1, n, m)
  x[nas] <- NA
  s <- simpute_svd(x)

  # reconstruct from returned factors
  recon <- s$u %*% (s$d * t(s$v))
  # at convergence: filled[nas] are well-approximated by the rank-J recon.
  # We check the fixed-point property: re-running SVD on (observed | recon[nas])
  # yields singular values close to s$d (the convergence threshold inside
  # simpute_svd is on a relative-Frobenius ratio, so individual singular values
  # match only up to ~ sqrt(thresh) in relative terms).
  x_filled <- x
  x_filled[nas] <- recon[nas]
  s_check <- svd(x_filled)
  expect_equal(s_check$d[seq_along(s$d)], s$d, tolerance = 1e-2)
})

test_that("simpute_svd is a no-op when there are no NAs (rank-J slice)", {
  set.seed(3)
  x <- matrix(rnorm(40), 10, 4)
  s_new <- simpute_svd(x)
  s_ref <- svd(x)
  # full reconstruction should match
  expect_equal(
    s_new$u %*% (s_new$d * t(s_new$v)),
    s_ref$u[, seq_along(s_new$d), drop = FALSE] %*%
      (s_ref$d[seq_along(s_new$d)] *
         t(s_ref$v[, seq_along(s_new$d), drop = FALSE])),
    tolerance = 1e-10
  )
})

test_that("simpute_svd clamps J to the available rank and handles 1-row inputs", {
  set.seed(5)
  # skinny matrix, J larger than min(dim) must not cause out-of-bounds errors
  x <- matrix(rnorm(6), 3, 2)
  x[1, 2] <- NA
  s <- simpute_svd(x, J = 10)
  expect_lte(length(s$d), min(dim(x)))
  expect_equal(dim(s$u), c(nrow(x), length(s$d)))
  expect_equal(dim(s$v), c(ncol(x), length(s$d)))

  # 1-row input: default J would be 0, must be clamped to 1
  x1 <- matrix(rnorm(5), 1, 5)
  x1[1, 3] <- NA
  s1 <- simpute_svd(x1)
  expect_equal(length(s1$d), 1L)
  expect_equal(dim(s1$u), c(1L, 1L))
  expect_equal(dim(s1$v), c(5L, 1L))

  # complete-data branch with 1 row
  s1c <- simpute_svd(matrix(rnorm(5), 1, 5))
  expect_equal(length(s1c$d), 1L)
})

test_that("simpute_svd warns instead of erroring when maxit leaves no iterations", {
  set.seed(8)
  x <- matrix(rnorm(20), 5, 4)
  x[2, 3] <- NA
  expect_warning(simpute_svd(x, maxit = 0), "Convergence not achieved")
})

test_that("fpc_wsvd weighted SVD matches svd() on uniform-grid centered data", {
  set.seed(11)
  n <- 30
  m <- 21
  arg <- seq(0, 1, length.out = m)
  data <- matrix(rnorm(n * m), n, m)
  res <- fpc_wsvd(data, arg = arg, pve = 1)

  # Reproduce expected structure: trapezoidal weights on uniform grid
  delta <- c(0, diff(arg))
  weights <- 0.5 * c(delta[-1] + head(delta, -1), tail(delta, 1))
  data_wc <- t((t(data) - colMeans(data)) * sqrt(weights))
  s_ref <- svd(data_wc, nu = 0, nv = min(dim(data)))
  efun_ref <- s_ref$v[, seq_len(res$npc), drop = FALSE] / sqrt(weights)

  expect_equal(abs(res$efunctions), abs(efun_ref), tolerance = 1e-10)
  expect_equal((res$evalues), (s_ref$d[seq_len(res$npc)])^2, tolerance = 1e-10)

  # eigenfunctions are L2-orthonormal under the trapezoidal inner product
  gram <- t(res$efunctions) %*% (weights * res$efunctions)
  expect_equal(gram, diag(res$npc), tolerance = 1e-8)
})
