grid <- round(seq(0, 10, length.out = 11), 3)
lin <- -3:3 * tfd(0.1 * grid, grid)
parallel <- -3:3 + tfd(0 * grid, grid)
names(lin) <- names(parallel) <- 1:7

spike_regular <- c(parallel, tfd(100 * (grid == 10), grid))
spike_irregular <- c(
  -3:3 + tfd(c(0 * grid, 0), c(grid, 20)),
  tfd(100 * (c(grid, 20) == 20), c(grid, 20))
)
na <- 1 * NA + lin[1]

lin_irreg <- {
  m <- as.matrix(lin)
  m[cbind(2:7, 2:7)] <- NA
  tfd(m, evaluator = tf_approx_linear)
}

lin_b <- tfb(lin, verbose = FALSE) |> suppressWarnings()

test_that("MBD works", {
  ranks <- c(1.5, 3.5, 5.5, 7, 5.5, 3.5, 1.5)
  names(ranks) <- names(lin)
  expect_equal(rank(tf_depth(lin, depth = "MBD")), ranks)
  expect_equal(rank(tf_depth(parallel, depth = "MBD")), ranks)
  expect_equal(rank(tf_depth(lin_irreg, depth = "MBD")), ranks)
  expect_equal(rank(tf_depth(lin_b, depth = "MBD")), ranks)
  # weighting by interval length:
  # increases importance of last point -> lower depth
  expect_gt(
    tail(tf_depth(spike_regular), 1),
    tail(tf_depth(spike_irregular), 1)
  )
})

test_that("FM works", {
  # FM should give central curve the highest depth (1 = most central)
  expect_equal(which.max(tf_depth(lin, depth = "FM")), c("4" = 4))
  expect_equal(which.max(tf_depth(parallel, depth = "FM")), c("4" = 4))
  # FM works with irregular and basis representations
  expect_equal(which.max(tf_depth(lin_irreg, depth = "FM")), c("4" = 4))
  expect_equal(which.max(tf_depth(lin_b, depth = "FM")), c("4" = 4))
  # FM values in [0, 1]
  fm_vals <- tf_depth(lin, depth = "FM")
  expect_true(all(fm_vals >= 0 & fm_vals <= 1))
  # FM ranking: central curve deepest for parallel data
  expect_equal(which.max(tf_depth(parallel, depth = "FM")), c("4" = 4))
  # weighting by interval length
  expect_gt(
    tail(tf_depth(spike_regular, depth = "FM"), 1),
    tail(tf_depth(spike_irregular, depth = "FM"), 1)
  )
})

test_that("FM handles ties consistently", {
  identical_curves <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 3, byrow = TRUE)
  expect_equal(tf_depth(identical_curves, depth = "FM"), c(0, 0, 0))

  tied_curves <- matrix(c(0, 0, 0, 0, 1, 1), nrow = 3, byrow = TRUE)
  expect_equal(tf_depth(tied_curves, depth = "FM"), c(2 / 3, 2 / 3, 0))
  expect_equal(
    tf_depth(tied_curves[c(3, 1, 2), , drop = FALSE], depth = "FM"),
    c(0, 2 / 3, 2 / 3)
  )
})

test_that("FSD works", {
  symmetric_ranks <- c(1.5, 3.5, 5.5, 7, 5.5, 3.5, 1.5)
  names(symmetric_ranks) <- names(lin)
  # FSD gives symmetric ranking for parallel lines
  expect_equal(rank(tf_depth(parallel, depth = "FSD")), symmetric_ranks)
  # central curve has highest depth (1 = most central)
  expect_equal(which.max(tf_depth(lin, depth = "FSD")), c("4" = 4))
  expect_equal(which.max(tf_depth(parallel, depth = "FSD")), c("4" = 4))
  # works with irregular and basis representations
  expect_equal(which.max(tf_depth(lin_irreg, depth = "FSD")), c("4" = 4))
  expect_equal(which.max(tf_depth(lin_b, depth = "FSD")), c("4" = 4))
  # FSD values in [0, 1]
  fsd_vals <- tf_depth(lin, depth = "FSD")
  expect_true(all(fsd_vals >= 0 & fsd_vals <= 1))
  # weighting by interval length
  expect_gt(
    tail(tf_depth(spike_regular, depth = "FSD"), 1),
    tail(tf_depth(spike_irregular, depth = "FSD"), 1)
  )
})

test_that("RPD works", {
  withr::local_seed(4217)
  # central curve has highest depth (1 = most central)
  expect_equal(which.max(tf_depth(lin, depth = "RPD")), c("4" = 4))
  # central curve deepest for parallel lines
  expect_equal(which.max(tf_depth(parallel, depth = "RPD")), c("4" = 4))
  # RPD values in (0, 1]
  rpd_vals <- tf_depth(lin, depth = "RPD")
  expect_true(all(rpd_vals > 0 & rpd_vals <= 1))
  # works with irregular and basis representations
  expect_equal(which.max(tf_depth(lin_irreg, depth = "RPD")), c("4" = 4))
  expect_equal(which.max(tf_depth(lin_b, depth = "RPD")), c("4" = 4))
  # RPD detects shape outliers that MBD misses:
  skip_on_cran()
  set.seed(42)
  t_grid <- seq(0, 1, length.out = 101)
  n_curves <- 50
  normal_mat <- matrix(0, n_curves, length(t_grid))
  for (i in seq_len(n_curves)) {
    normal_mat[i, ] <- cumsum(rnorm(length(t_grid), sd = 0.1))
  }
  outlier_mat <- matrix(sin(20 * pi * t_grid) * 0.5, nrow = 1)
  all_curves <- tfd(rbind(normal_mat, outlier_mat), arg = t_grid)
  rpd_depths <- tf_depth(
    all_curves,
    depth = "RPD",
    n_projections = 10000L,
    n_projections_beta = 1000L
  )
  mbd_depths <- tf_depth(all_curves, depth = "MBD")
  # RPD should rank the shape outlier lower (less central) than MBD does
  expect_lt(rank(rpd_depths)[n_curves + 1], rank(mbd_depths)[n_curves + 1])
})


test_that("matrix na.rm drops incomplete observations", {
  x <- matrix(c(0, NA, 1, 1, 2, 2), nrow = 3, byrow = TRUE)

  expect_equal(tf_depth(x, depth = "FM", na.rm = TRUE), c(1, 0))
  expect_equal(tf_depth(x, depth = "FSD", na.rm = TRUE), c(0.5, 0.5))

  withr::local_seed(4217)
  expect_equal(
    tf_depth(
      x,
      depth = "RPD",
      na.rm = TRUE,
      n_projections = 100L,
      n_projections_beta = 20L
    ),
    c(0.5, 0.5)
  )
})

test_that("median works", {
  expect_true(is.na(median(c(na, lin))))
  expect_identical(median(c(na, lin), na.rm = TRUE), median(lin))
  expect_message(median(lin[1:2]), "maximal depth")
})
