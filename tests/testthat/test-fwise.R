test_that ("fwise summaries work for tfd_reg", {
  x <- tf_rgp(3)

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0))
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1))

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE)

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, -1, -1), ignore_attr = TRUE)
})

test_that ("fwise summaries work for tfd_irreg", {
  x0 <- tf_rgp(3)
  x <- x0 |> tf_jiggle()
  x2 <- tfd(x, tf_arg(x0), evaluator = tf_approx_fill_extend)

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0))
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1))

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0))
  expect_equal(tf_fsd(x_std), c(1, 1, 1))

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, 1, 1))
})
