test_that("fwise summaries work for tfd_reg", {
  x <- tf_rgp(3, arg = seq(0, 5, l = 101)) #use non-unit length to verify scaling

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE)

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, -1, -1), ignore_attr = TRUE)

})

test_that("fwise summaries work for tfd_irreg", {
  set.seed(1221)
  x <- tf_rgp(3, arg = 201L)
  tf_evaluator(x) <- tf_approx_fill_extend
  x0 <- tf_jiggle(x)
  x2 <- tfd(x, tf_arg(x0), evaluator = tf_approx_fill_extend)

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE)

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, -1, -1), ignore_attr = TRUE)

  tf_rgp(3) |> tf_jiggle() |> tf_fmean()
})


test_that("fwise summaries work for tfb_spline", {
  suppressMessages(x <- tf_rgp(3) |> tfb(k = 35))

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE)

  expect_equal(tf_crosscov(x, x),
               tf_fvar(x))
})

test_that("fwise summaries work for tfb_fpc", {
  set.seed(1212)
  x <- tf_rgp(3, arg = 501L) |> tfb_fpc(k = 10)

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0),
               ignore_attr = TRUE, tolerance = .05) #!! uh oh
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1),
               ignore_attr = TRUE, tolerance = .05) #!! uh oh

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0),
               ignore_attr = TRUE, tolerance = .05) #!! uh oh
  expect_equal(tf_fsd(x_std), c(1, 1, 1),
               ignore_attr = TRUE, tolerance = .05) #!! uh oh

  expect_equal(tf_crosscov(x, x),
               tf_fvar(x))
})
