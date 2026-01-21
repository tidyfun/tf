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

    # keep_new_arg = TRUE returns tfd_irreg
    unreg <- tf_warp(x, w, keep_new_arg = TRUE)
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
  ret <- tf_warp(x, w, keep_new_arg = TRUE)
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

    # keep_new_arg = TRUE returns tfd_irreg
    irr <- tf_unwarp(unreg, w, keep_new_arg = TRUE)
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
  expect_identical(tf_warp(tf_unwarp(x, w, keep_new_arg = TRUE), w), x)
})

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

  test_x <- function(x) {
    # with default template function (Karcher mean)
    domain <- tf_domain(x)
    warp <- tf_register(x)
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))

    # simple template function
    template <- mean(x)
    warp <- tf_register(x, .template = template)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    # works with fda package
    warp <- tf_register(x, .method = "fda", crit = 1)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    warp <- tf_register(x, .method = "fda", crit = 2)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    template <- mean(x)
    warp2 <- tf_register(x, .template = template, .method = "fda")
    expect_equal(warp, warp2, tolerance = 0.1) #mean is default template
    warp <- warp2
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))
  }

  # test for all major tf-subclasses:
  test_x(tfd(t(data)))

  test_x(suppressMessages(tfb(t(data), k = 20)))

  test_x(tfb_fpc(t(data), pve = .95))

  expect_error(
    tf_register(tfd(t(data)) |> tf_sparsify(.2)),
    "objects cannot be registered. Please convert"
  )
})

test_that("tf_register_landmark aligns peaks to mean landmark location", {
  # Create functions with known shifted peaks
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.3, 0.5, 0.7)
  x <- tfd(
    t(sapply(peak_locs, function(s) dnorm(t, mean = s, sd = 0.1))),
    arg = t
  )

  landmarks <- matrix(peak_locs, ncol = 1)
  warp <- tf_register_landmark(x, landmarks)

  # Basic structure checks
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))

  # Behavioral check: warping function should map landmarks to template
  # Template defaults to mean of landmarks = 0.5
  template_landmark <- mean(peak_locs)
  warp_at_template <- as.numeric(warp[, template_landmark])
  expect_equal(warp_at_template, peak_locs, tolerance = 1e-10)
})

test_that("tf_register_landmark aligns to custom template", {
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.3, 0.5, 0.7)
  x <- tfd(
    t(sapply(peak_locs, function(s) dnorm(t, mean = s, sd = 0.1))),
    arg = t
  )

  landmarks <- matrix(peak_locs, ncol = 1)
  custom_template <- 0.4
  warp <- tf_register_landmark(
    x,
    landmarks,
    .template_landmarks = custom_template
  )

  # Warp should map custom template to observed landmarks
  warp_at_template <- as.numeric(warp[, custom_template])
  expect_equal(warp_at_template, peak_locs, tolerance = 1e-10)

  # After unwarping, peaks should be aligned near the template
  x_aligned <- tf_unwarp(x, warp)
  aligned_peaks <- tf_where(x_aligned, value == max(value), "first")
  expect_equal(aligned_peaks, rep(custom_template, 3), tolerance = 0.02)
})

test_that("tf_landmarks_extrema finds correct extrema locations", {
  t <- seq(0, 2 * pi, length.out = 101)
  phase_shifts <- c(0, 0.5, -0.3)
  x <- tfd(t(cbind(sin(t), sin(t + 0.5), sin(t - 0.3))), arg = t)

  # Expected maxima: sin peaks at pi/2, so shifted peaks at pi/2 - shift
  expected_max <- pi / 2 - phase_shifts

  maxima <- tf_landmarks_extrema(x, "max")
  expect_true(is.matrix(maxima))
  expect_equal(dim(maxima), c(3, 1))
  # Check maxima are at expected locations (within grid resolution)
  expect_equal(as.numeric(maxima), expected_max, tolerance = 0.1)

  # Expected minima: sin troughs at 3*pi/2
  expected_min <- 3 * pi / 2 - phase_shifts
  minima <- tf_landmarks_extrema(x, "min")
  expect_equal(dim(minima), c(3, 1))
  expect_equal(as.numeric(minima), expected_min, tolerance = 0.1)

  # Both: maxima in col 1, minima in col 2
  both <- tf_landmarks_extrema(x, "both")
  expect_equal(dim(both), c(3, 2))
  expect_equal(both[, 1], maxima[, 1])
  expect_equal(both[, 2], minima[, 1])
})

test_that("tf_register_landmark validates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  # Wrong number of rows
  expect_error(
    tf_register_landmark(x, matrix(c(0.3, 0.5, 0.7), ncol = 1)),
    "rows"
  )

  # Non-increasing landmarks - this is caught before template_arg is created
  expect_error(
    tf_register_landmark(
      x,
      matrix(c(0.5, 0.3, 0.4, 0.2), ncol = 2, byrow = TRUE)
    ),
    "strictly increasing"
  )

  # Landmarks outside domain
  expect_error(
    tf_register_landmark(x, matrix(c(-0.1, 1.1), ncol = 1)),
    "within the domain"
  )

  # Template landmarks wrong length
  expect_error(
    tf_register_landmark(
      x,
      matrix(c(0.3, 0.5), ncol = 1),
      .template_landmarks = c(0.4, 0.5)
    ),
    "len"
  )
})

test_that("tf_register_affine shift recovers known phase shifts", {
  # Create shifted sinusoids with known shifts
  t <- seq(0, 2 * pi, length.out = 101)
  true_shifts <- c(-0.5, -0.2, 0, 0.2, 0.5)
  x <- tfd(t(sapply(true_shifts, function(s) sin(t + s))), arg = t)

  warp <- tf_register_affine(x, .type = "shift")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))

  # For shift-only warping, h(t) = t + b, where b is the estimated shift
  # The warp at domain midpoint reveals the shift parameter
  mid <- mean(tf_domain(x))
  estimated_shifts <- as.numeric(warp[, mid]) - mid

  # Shifts should center around 0 (template is mean, which has shift = 0)
  expect_equal(mean(estimated_shifts), 0, tolerance = 0.1)

  # Aligned functions should have lower pointwise variance
  x_aligned <- tf_unwarp(x, warp)
  var_before <- mean(apply(as.matrix(x), 2, var))
  var_after <- mean(apply(as.matrix(x_aligned), 2, var))
  expect_lte(var_after, var_before)
})

test_that("tf_register_affine scale with anchor preserves fixed point", {
  t <- seq(0, 1, length.out = 101)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi * 0.9), sin(t * pi * 1.1))), arg = t)

  # With anchor = "start", the warp should pass through (0, 0)
  warp_start <- tf_register_affine(x, .type = "scale", .anchor = "start")
  expect_s3_class(warp_start, "tfd")
  warp_at_start <- as.numeric(warp_start[, 0])
  expect_equal(warp_at_start, rep(0, 3), tolerance = 1e-6)

  # With anchor = "end", the warp should pass through (1, 1)
  warp_end <- tf_register_affine(x, .type = "scale", .anchor = "end")
  warp_at_end <- as.numeric(warp_end[, 1])
  expect_equal(warp_at_end, rep(1, 3), tolerance = 1e-6)

  # With anchor = "center", the warp should pass through (0.5, 0.5)
  warp_center <- tf_register_affine(x, .type = "scale", .anchor = "center")
  warp_at_center <- as.numeric(warp_center[, 0.5])
  expect_equal(warp_at_center, rep(0.5, 3), tolerance = 1e-6)
})

test_that("tf_register_affine shift_scale improves alignment", {
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(cbind(sin(t), sin(t * 0.95 + 0.2), sin(t * 1.05 - 0.1))), arg = t)

  warp <- tf_register_affine(x, .type = "shift_scale")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))

  # Verify warping functions are strictly monotone (valid warps)
  warp_evals <- tf_evaluations(warp)
  for (i in seq_along(warp_evals)) {
    expect_true(
      all(diff(warp_evals[[i]]) > 0),
      info = paste("Warp", i, "not strictly increasing")
    )
  }

  # Aligned functions should have lower pointwise variance
  x_aligned <- tf_unwarp(x, warp)
  var_before <- mean(apply(as.matrix(x), 2, var))
  var_after <- mean(apply(as.matrix(x_aligned), 2, var))
  expect_lte(var_after, var_before)
})

test_that("tf_register_affine validates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  # Template wrong length
  expect_error(
    tf_register_affine(x, .template = x),
    "length 1"
  )

  # Different domains
  x2 <- tfd(
    sin(seq(0, 2, length.out = 51) * pi),
    arg = seq(0, 2, length.out = 51)
  )
  expect_error(
    tf_register_affine(x, .template = x2),
    "same domain"
  )
})
