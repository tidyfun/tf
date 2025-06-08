test_that("tf_register works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(1234)

  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })

  x <- tfd(t(data))
  reg <- tf_register(x)
  expect_list(reg, len = 2)
  expect_s3_class(reg$x, "tfd")
  expect_length(reg$x, length(x))
  expect_s3_class(reg$warp, "tfd")
  expect_identical(tf_domain(reg$warp), c(0, 1))
  expect_true(all(tf_fmin(reg$warp) == 0))
  expect_true(all(tf_fmax(reg$warp) == 1))
})

test_that("tf_register_template works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(123)

  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })

  # with default template function (Karcher mean)
  x <- tfd(t(data))
  warp <- tf_register_template(x)
  expect_identical(tf_domain(warp), c(0, 1))
  expect_true(all(tf_fmin(warp) == 0))
  expect_true(all(tf_fmax(warp) == 1))
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  # simple template function
  template <- mean(x)
  warp <- tf_register_template(x, template)
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), c(0, 1))
  expect_true(all(tf_fmin(warp) == 0))
  expect_true(all(tf_fmax(warp) == 1))
})

test_that("tf_warp works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(1234)

  x <- tf_rgp(10)
  reg <- tf_register(x)
  unreg <- tf_warp(reg$x, reg$warp)
  expect_s3_class(unreg, "tfd_reg")
  expect_length(unreg, length(x))
  expect_equal(unreg, x, tolerance = 1.0, ignore_attr = "names")

  unreg <- tf_warp(reg$x, reg$warp, keep_arg = TRUE)
  expect_s3_class(unreg, "tfd_irreg")
  expect_length(unreg, length(x))

  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })
  x <- tfd(t(data))
  reg <- tf_register(x)
  unreg <- do.call(tf_warp, reg)
  expect_s3_class(unreg, "tfd")
  expect_length(unreg, length(x))
  expect_equal(unreg, x, tolerance = 1.0, ignore_attr = "names")
})

test_that("tf_unwarp works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(1234)

  x <- tf_rgp(10)
  reg <- tf_register(x)
  res <- tf_unwarp(x, reg$warp)
  expect_s3_class(res, "tfd")
  expect_length(res, length(x))
  expect_equal(res, x, tolerance = 0.1, ignore_attr = "names")

  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })
  x <- tfd(t(data))
  reg <- tf_register(x)
  res <- tf_unwarp(x, reg$warp)
  expect_s3_class(res, "tfd")
  expect_length(res, length(x))
  expect_equal(res, x, tolerance = 0.1, ignore_attr = "names")
})
