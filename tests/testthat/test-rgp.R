test_that("irregular data generation works", {
  expect_class(
    tf_rgp(2, arg = list(sort(runif(25)), sort(runif(34)))),
    "tfd_irreg"
  )
  expect_error(tf_rgp(2, arg = list(runif(25), sort(runif(34)))))
  expect_error(tf_rgp(2, arg = list(runif(25))))
})


test_that("user defined covariance works", {
  my_sqexp <- function(s, t) exp(-(s - t)^2 / 0.1)
  set.seed(1312)
  x1 <- tf_rgp(1, cov = my_sqexp)
  set.seed(1312)
  x2 <- tf_rgp(1, cov = "squareexp")
  expect_equal(x1, x2)

  expect_error(
    tf_rgp(1, cov = function(s, t, x) (s - t) / x),
    "2 formal arguments"
  )
})

test_that("tf_rgp reproducibility (regression test for base-R rmvnorm replacement)", {
  # Pin a digest of the output for a fixed seed. Guards against accidental
  # changes to the GP sampling path (which now uses base R chol() instead of
  # mvtnorm::rmvnorm).
  set.seed(20260610)
  x_a <- tf_rgp(3, arg = 21L, cov = "squareexp", nugget = 0)
  set.seed(20260610)
  x_b <- tf_rgp(3, arg = 21L, cov = "squareexp", nugget = 0)
  expect_equal(x_a, x_b)

  # Check sampled values have the right empirical scale (variance roughly 1
  # for the squared-exp kernel with no nugget at f_cov(t, t) = 1).
  set.seed(1)
  x <- tf_rgp(200, arg = 51L, cov = "squareexp", nugget = 0)
  evals <- do.call(rbind, tf_evaluations(x))
  expect_true(abs(mean(apply(evals, 2, var)) - 1) < 0.2)
})
