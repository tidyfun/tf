test_that("tf_register works", {
  skip_if_not_installed("fdasrvf")

  set.seed(123)
  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })

  x <- as.tfd(t(data))
  reg <- tf_register(x)
  expect_list(reg, len = 2)
  expect_s3_class(reg$x, "tfd")
  expect_length(reg$x, length(x))
  expect_s3_class(reg$warp, "tfd")
})

test_that("tf_register_template works", {
  skip_if_not_installed("fdasrvf")

  set.seed(123)
  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), function(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    noise_level <- 0.1
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, noise_level)
  })

  # with default template function (Karcher mean)
  x <- as.tfd(t(data))
  warp <- tf_register_template(x)
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  # simple template function
  template <- mean(x)
  warp <- tf_register_template(x, template)
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
})

test_that("tf_warp works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(1234)
  x <- tf_rgp(10)
  reg <- tf_register(x)
  unreg <- tf_warp(reg$x, reg$warp)
  x
  tf_warp(reg$x, reg$warp, keep_arg = FALSE)
  unreg
  expect_s3_class(unreg, "tfd")
})

test_that("tf_unwarp works", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(1234)
  x <- tf_rgp(10)
  reg <- tf_register(x)
  unreg <- tf_warp(reg$x, reg$warp)
})

test_that("tf_register_template works", {
  withr::local_seed(1234)
  x <- tf_rgp(10)
  template <- tf_register_template(x)
})
