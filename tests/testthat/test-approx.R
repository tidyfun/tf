test_that("tf_approx_linear interpolates and returns NA outside", {
  res <- tf_approx_linear(
    x = c(-0.1, 0, 0.25, 0.5, 1, 1.1),
    arg = c(0, 0.5, 1),
    evaluations = c(0, 1, 2)
  )
  expect_equal(res[2:5], c(0, 0.5, 1, 2))
  expect_true(is.na(res[1]) && is.na(res[6]))
})

test_that("tf_approx_spline interpolates linear data exactly", {
  arg <- seq(0, 1, length.out = 11)
  evals <- 2 * arg + 1
  res <- tf_approx_spline(x = c(0.05, 0.27, 0.95), arg = arg, evaluations = evals)
  expect_equal(res, 2 * c(0.05, 0.27, 0.95) + 1, tolerance = 1e-8)
})

test_that("tf_approx_none returns NA at unseen args, passes through seen ones", {
  res <- tf_approx_none(
    x = c(0, 0.25, 0.5, 1),
    arg = c(0, 0.5, 1),
    evaluations = c(0, 1, 2)
  )
  expect_equal(res[c(1, 3, 4)], c(0, 1, 2))
  expect_true(is.na(res[2]))
})

test_that("tf_approx_fill_extend extrapolates with boundary values", {
  res <- tf_approx_fill_extend(
    x = c(-0.5, 0, 0.5, 1, 2),
    arg = c(0, 1),
    evaluations = c(3, 7)
  )
  expect_equal(res, c(3, 3, 5, 7, 7))
})

test_that("tf_approx_locf carries last observation forward", {
  res <- tf_approx_locf(
    x = c(0, 0.3, 0.5, 0.7, 1),
    arg = c(0, 0.5, 1),
    evaluations = c(10, 20, 30)
  )
  expect_equal(res, c(10, 10, 20, 20, 30))
})

test_that("tf_approx_nocb carries next observation backward", {
  res <- tf_approx_nocb(
    x = c(0, 0.3, 0.5, 0.7, 1),
    arg = c(0, 0.5, 1),
    evaluations = c(10, 20, 30)
  )
  expect_equal(res, c(10, 20, 20, 30, 30))
})

test_that("evaluators are wired up via tf_evaluator<- on tfd", {
  x <- tfd(matrix(c(0, 1, 2), nrow = 1), arg = c(0, 0.5, 1),
           domain = c(-1, 2))
  expect_true(is.na(as.numeric(x[, 1.5])))
  tf_evaluator(x) <- tf_approx_fill_extend
  expect_equal(as.numeric(x[, 1.5]), 2)
})
