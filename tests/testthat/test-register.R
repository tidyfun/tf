test_that("tf_warp works", {
  withr::local_seed(1234)
  min_t <- c(-2, 0, 1, 1)
  max_t <- c(3, 1, 2, 10)
  walk2(min_t, max_t, function(min_t, max_t) {
    t <- seq(min_t, max_t, length.out = 101)
    f0 <- tfd(sin((t - min_t) / (max_t - min_t) * 2 * pi), t)
    x <- rep(f0, 10)
    w <- tf_rgp(10, t) |> exp() |> tf_integrate(definite = FALSE)
    w_min <- tf_fmin(w)
    w_max <- tf_fmax(w)
    w <- (w - w_min) / (w_max - w_min) * (max_t - min_t) + min_t

    unreg <- tf_warp(x, w)
    expect_s3_class(unreg, "tfd_reg")
    expect_length(unreg, length(x))
    expect_identical(tf_domain(unreg), tf_domain(x))

    # keep_arg = TRUE returns tfd_irreg
    unreg <- tf_warp(x, w, keep_arg = TRUE)
    expect_s3_class(unreg, "tfd_irreg")
    expect_length(unreg, length(x))
    expect_identical(tf_domain(unreg), tf_domain(x))

    # tf_warp and tf_unwarp are inverses
    expect_equal(tf_unwarp(tf_warp(x, w), w), x, tolerance = 0.1)
  })
})

test_that("tf_warp identity works", {
  t <- seq(-1, 2, length.out = 50)
  f0 <- tfd(cos(t), t)
  x <- rep(f0, 5)
  w <- rep(tfd(t, arg = t), 5)
  expect_identical(tf_unwarp(tf_warp(x, w), w), x)
  ret <- tf_warp(x, w, keep_arg = TRUE)
  expect_identical(tf_unwarp(ret, w), x)
})

test_that("tf_unwarp works", {
  withr::local_seed(1234)
  min_t <- c(-2, 0, 1, 1)
  max_t <- c(3, 1, 2, 10)
  walk2(min_t, max_t, function(min_t, max_t) {
    t <- seq(min_t, max_t, length.out = 101)
    f0 <- tfd(sin((t - min_t) / (max_t - min_t) * 2 * pi), t)
    x <- rep(f0, 10)
    w <- tf_rgp(10, t) |> exp() |> tf_integrate(definite = FALSE)
    w_min <- tf_fmin(w)
    w_max <- tf_fmax(w)
    w <- (w - w_min) / (w_max - w_min) * (max_t - min_t) + min_t

    unreg <- tf_unwarp(x, w)
    reg <- tf_warp(unreg, w)
    expect_s3_class(reg, "tfd_reg")
    expect_length(reg, length(x))
    expect_identical(tf_domain(reg), tf_domain(x))

    # keep_arg = TRUE returns tfd_irreg
    irr <- tf_unwarp(unreg, w, keep_arg = TRUE)
    expect_s3_class(irr, "tfd_irreg")
    expect_length(irr, length(x))
    expect_identical(tf_domain(irr), tf_domain(x))

    # tf_warp and tf_unwarp are inverses
    expect_equal(tf_warp(tf_unwarp(x, w), w), x, tolerance = 0.1)
  })
})

test_that("tf_unwarp identity works", {
  t <- seq(-1, 2, length.out = 50)
  f0 <- tfd(cos(t), t)
  x <- rep(f0, 5)
  w <- rep(tfd(t, arg = t), 5)
  expect_identical(tf_warp(tf_unwarp(x, w), w), x)
  expect_identical(tf_warp(tf_unwarp(x, w, keep_arg = TRUE), w), x)
})

test_that("tf_register_template works", {
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

  # with default template function (Karcher mean)
  x <- tfd(t(data))
  domain <- tf_domain(x)
  warp <- tf_register_template(x)
  expect_identical(tf_domain(warp), domain)
  expect_true(all(tf_fmin(warp) == domain[1]))
  expect_true(all(tf_fmax(warp) == domain[2]))
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))

  # simple template function
  template <- mean(x)
  warp <- tf_register_template(x, template = template)
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), domain)
  expect_true(all(tf_fmin(warp) == domain[1]))
  expect_true(all(tf_fmax(warp) == domain[2]))

  # works with fda package
  warp <- tf_register_template(x, method = "fda")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), domain)
  expect_true(all(tf_fmin(warp) == domain[1]))
  expect_true(all(tf_fmax(warp) == domain[2]))

  template <- mean(x)
  warp <- tf_register_template(x, template = template, method = "fda")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), domain)
  expect_true(all(tf_fmin(warp) == domain[1]))
  expect_true(all(tf_fmax(warp) == domain[2]))
})
