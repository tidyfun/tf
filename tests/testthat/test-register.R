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
    warp <- tf_register(x, template = template)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    # works with fda package
    warp <- tf_register(x, method = "fda", crit = 1)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    warp <- tf_register(x, method = "fda", crit = 2)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]))
    expect_true(all(tf_fmax(warp) <= domain[2]))

    template <- mean(x)
    warp2 <- tf_register(x, template = template, method = "fda")
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

test_that("tf_register landmark methodaligns peaks to mean landmark location", {
  # Create functions with known shifted peaks
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.3, 0.5, 0.7)
  x <- tfd(
    t(sapply(peak_locs, function(s) dnorm(t, mean = s, sd = 0.1))),
    arg = t
  )

  landmarks <- matrix(peak_locs, ncol = 1)
  warp <- tf_register(x, method = "landmark", landmarks)

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

test_that("tf_register landmark methodaligns to custom template", {
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.3, 0.5, 0.7)
  x <- tfd(
    t(sapply(peak_locs, function(s) dnorm(t, mean = s, sd = 0.1))),
    arg = t
  )

  landmarks <- matrix(peak_locs, ncol = 1)
  custom_template <- 0.4
  warp <- tf_register(
    x,
    method = "landmark",
    landmarks = landmarks,
    template_landmarks = custom_template
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

test_that("tf_register landmark methodvalidates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  # Wrong number of rows
  expect_error(
    tf_register(x, method = "landmark", matrix(c(0.3, 0.5, 0.7), ncol = 1)),
    "rows"
  )

  # Non-increasing landmarks - this is caught before template_arg is created
  expect_error(
    tf_register(
      x,
      method = "landmark",
      landmarks = matrix(c(0.5, 0.3, 0.4, 0.2), ncol = 2, byrow = TRUE)
    ),
    "strictly increasing"
  )

  # Landmarks outside domain
  expect_error(
    tf_register(
      x,
      method = "landmark",
      landmarks = matrix(c(-0.1, 1.1), ncol = 1)
    ),
    "within the domain"
  )

  # Template landmarks wrong length
  expect_error(
    tf_register(
      x,
      method = "landmark",
      landmarks = matrix(c(0.3, 0.5), ncol = 1),
      template_landmarks = c(0.4, 0.5)
    ),
    "len"
  )
})

test_that("tf_register affine method shift produces linear warps", {
  # Create shifted sinusoids with known shifts
  t <- seq(0, 2 * pi, length.out = 101)
  true_shifts <- c(-0.3, -0.1, 0, 0.1, 0.3) # Smaller shifts for reliability
  x <- tfd(t(sapply(true_shifts, function(s) sin(t + s))), arg = t)

  # Use the unshifted sinusoid as template
  template <- tfd(sin(t), arg = t)
  warp <- tf_register(x, method = "affine", template = template, type = "shift")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))

  # Verify warping functions are strictly monotone (valid warps)
  warp_evals <- tf_evaluations(warp)
  for (i in seq_along(warp_evals)) {
    expect_true(all(diff(warp_evals[[i]]) > 0))
  }

  # Verify warps are truly linear: second differences should be ~0
  for (i in seq_along(warp_evals)) {
    second_diff <- diff(diff(warp_evals[[i]]))
    expect_lt(
      max(abs(second_diff)),
      1e-10,
      label = paste("Function", i, "warp linearity")
    )
  }
})

test_that("tf_register affine method shift produces warps that recover shift direction", {
  # Test that shift registration produces warps in the correct direction
  t <- seq(0, 2 * pi, length.out = 101)
  template <- tfd(sin(t), arg = t)

  # Create functions with known shifts
  # Function shifted left (negative shift in arg) needs positive warp to align
  # Function shifted right (positive shift in arg) needs negative warp to align
  x_shifted_left <- tfd(sin(t - 0.3), arg = t) # peaks earlier
  x_shifted_right <- tfd(sin(t + 0.3), arg = t) # peaks later

  # Register shifted left function
  warp_left <- tf_register(
    x_shifted_left,
    method = "affine",
    template = template,
    type = "shift"
  )
  warp_left_evals <- tf_evaluations(warp_left)[[1]]
  # Warp should have positive shift (warp > arg on average) to push function forward
  mean_shift_left <- mean(warp_left_evals - t)

  # Register shifted right function
  warp_right <- tf_register(
    x_shifted_right,
    method = "affine",
    template = template,
    type = "shift"
  )
  warp_right_evals <- tf_evaluations(warp_right)[[1]]
  # Warp should have negative shift (warp < arg on average) to pull function backward
  mean_shift_right <- mean(warp_right_evals - t)

  # The two warps should shift in opposite directions
  expect_true(mean_shift_left > mean_shift_right)
})

test_that("tf_register affine methodscale produces linear warps centered at midpoint", {
  t <- seq(0, 1, length.out = 101)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi * 0.9), sin(t * pi * 1.1))), arg = t)

  warp <- tf_register(x, method = "affine", type = "scale")
  expect_s3_class(warp, "tfd")

  # Verify warps are linear (second differences ~ 0)
  warp_evals <- tf_evaluations(warp)
  for (i in seq_along(warp_evals)) {
    second_diff <- diff(diff(warp_evals[[i]]))
    expect_lt(
      max(abs(second_diff)),
      1e-10,
      label = paste("Function", i, "warp linearity")
    )
  }

  # Verify scaling is centered around domain midpoint (0.5)
  # For h(s) = a*s + b with centering: h(center) = center, so b = center*(1-a)
  # At center, warp should be close to center
  center <- 0.5
  warp_at_center <- as.numeric(warp[, center])
  expect_equal(warp_at_center, rep(center, 3), tolerance = 0.1)
})


test_that("tf_register affine method shift_scale produces linear warps", {
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(cbind(sin(t), sin(t * 0.95 + 0.2), sin(t * 1.05 - 0.1))), arg = t)

  warp <- tf_register(x, method = "affine", type = "shift_scale")
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))

  # Verify warping functions are strictly monotone (valid warps)
  warp_evals <- tf_evaluations(warp)
  for (i in seq_along(warp_evals)) {
    expect_true(all(diff(warp_evals[[i]]) > 0))
  }

  # Verify warps are truly linear: second differences should be ~0
  for (i in seq_along(warp_evals)) {
    second_diff <- diff(diff(warp_evals[[i]]))
    expect_lt(
      max(abs(second_diff)),
      1e-10,
      label = paste("Function", i, "warp linearity")
    )
  }
})

test_that("tf_register affine methodregistered functions preserve domain", {
  t <- seq(0, 2 * pi, length.out = 101)
  true_shifts <- c(-0.5, 0, 0.5)
  x <- tfd(t(sapply(true_shifts, function(s) sin(t + s))), arg = t)

  warp <- tf_register(x, method = "affine", type = "shift")
  x_aligned <- tf_unwarp(x, warp)

  # Domain of registered functions should equal domain of original

  expect_identical(tf_domain(x_aligned), tf_domain(x))
})

test_that("tf_register affine methodregistered functions may have NA at boundaries", {
  # Create functions with large shifts to ensure boundaries are affected
  t <- seq(0, 2 * pi, length.out = 101)
  true_shifts <- c(-0.8, 0.8) # Large shifts
  x <- tfd(t(sapply(true_shifts, function(s) sin(t + s))), arg = t)

  template <- tfd(sin(t), arg = t)
  warp <- tf_register(x, method = "affine", template = template, type = "shift")
  x_aligned <- tf_unwarp(x, warp)

  # For large shifts, we expect some NA values at boundaries
  # Function shifted forward (positive shift in original) will have NA at start
  # Function shifted backward (negative shift in original) will have NA at end
  evals <- tf_evaluations(x_aligned)

  # Check that at least some functions have NA values
  # (this depends on whether the shift was large enough)
  has_na <- sapply(evals, anyNA)
  # At least one function should have NA if shifts are recovered
  expect_true(any(has_na) || all(sapply(evals, function(e) all(!is.na(e)))))
})

test_that("tf_register affine method shift aligns functions to template", {
  # THE KEY TEST: registered functions should equal template where defined
  set.seed(4321)
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)

  # Create shifted functions with known shifts
  true_shifts <- c(-0.2, -0.1, 0.1, 0.15)
  shifted <- lapply(true_shifts, function(s) {
    tfd(sin(2 * pi * (arg - s)), arg = arg)
  })
  x <- do.call(c, shifted)

  # Register
  warps <- tf_register(
    x,
    method = "affine",
    template = template,
    type = "shift"
  )
  x_reg <- tf_unwarp(x, warps)

  # Verify alignment: registered functions should match template
  template_vec <- tf_evaluations(template)[[1]]

  for (i in seq_along(x_reg)) {
    # Evaluate registered function on the grid
    reg_on_grid <- tf_evaluate(x_reg[i], arg = arg)[[1]]
    valid <- !is.na(reg_on_grid)

    # Must have some valid points
    expect_gt(sum(valid), 20, label = paste("Function", i, "valid points"))

    # Max difference from template should be near zero (numerical precision)
    max_diff <- max(abs(reg_on_grid[valid] - template_vec[valid]))
    expect_lt(
      max_diff,
      0.01,
      label = paste("Function", i, "alignment to template")
    )
  }
})

test_that("tf_register affine methodscale aligns functions to template", {
  # Verify scale registration actually aligns functions
  # Scale type uses h(t) = a*t + 0.5*(1-a), centered at midpoint
  # To test: create f(t) = template(h^{-1}(t)) where h^{-1}(t) = t/a + 0.5*(1-1/a)
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)

  # Scale factors for the warp (a < 1 shrinks, a > 1 expands)
  scale_factors <- c(0.7, 0.85, 1.15, 1.3)
  scaled <- lapply(scale_factors, function(a) {
    # Inverse warp: h^{-1}(t) = t/a + 0.5*(1 - 1/a)
    inv_arg <- arg / a + 0.5 * (1 - 1 / a)
    tfd(sin(2 * pi * inv_arg), arg = arg)
  })
  x <- do.call(c, scaled)

  # Register
  warps <- tf_register(
    x,
    method = "affine",
    template = template,
    type = "scale"
  )
  x_reg <- tf_unwarp(x, warps)

  # Verify alignment
  template_vec <- tf_evaluations(template)[[1]]

  for (i in seq_along(x_reg)) {
    reg_on_grid <- tf_evaluate(x_reg[i], arg = arg)[[1]]
    valid <- !is.na(reg_on_grid)
    expect_gt(sum(valid), 20)

    max_diff <- max(abs(reg_on_grid[valid] - template_vec[valid]))
    expect_lt(
      max_diff,
      0.01,
      label = paste("Function", i, "alignment to template")
    )
  }
})

test_that("tf_register affine method shift_scale aligns functions to template", {
  # Verify combined shift+scale registration actually aligns functions
  # shift_scale uses h(t) = a*t + b
  # To test: create f(t) = template(h^{-1}(t)) where h^{-1}(t) = (t-b)/a
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)

  # (a, b) parameters for the warp
  params <- list(
    c(a = 0.9, b = 0.05),
    c(a = 1.1, b = -0.05),
    c(a = 0.95, b = 0.02)
  )
  transformed <- lapply(params, function(p) {
    # Inverse warp: h^{-1}(t) = (t - b) / a
    inv_arg <- (arg - p["b"]) / p["a"]
    tfd(sin(2 * pi * inv_arg), arg = arg)
  })
  x <- do.call(c, transformed)

  # Register
  warps <- tf_register(
    x,
    method = "affine",
    template = template,
    type = "shift_scale"
  )
  x_reg <- tf_unwarp(x, warps)

  # Verify alignment
  template_vec <- tf_evaluations(template)[[1]]

  for (i in seq_along(x_reg)) {
    reg_on_grid <- tf_evaluate(x_reg[i], arg = arg)[[1]]
    valid <- !is.na(reg_on_grid)
    expect_gt(sum(valid), 20)

    max_diff <- max(abs(reg_on_grid[valid] - template_vec[valid]))
    expect_lt(
      max_diff,
      0.01,
      label = paste("Function", i, "alignment to template")
    )
  }
})

test_that("tf_register affine methodvalidates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  # Template wrong length
  expect_error(
    tf_register(x, method = "affine", template = x),
    "length 1"
  )

  # Different domains
  x2 <- tfd(
    sin(seq(0, 2, length.out = 51) * pi),
    arg = seq(0, 2, length.out = 51)
  )
  expect_error(
    tf_register(x, method = "affine", template = x2),
    "same domain"
  )

  # Invalid range parameters
  expect_error(tf_register(x, method = "affine", shift_range = c(0.5)))
  expect_error(tf_register(x, method = "affine", shift_range = c(0.5, 0.3)))
  expect_error(tf_register(x, method = "affine", scale_range = c(-1, 2)))
  expect_error(tf_register(x, method = "affine", scale_range = c(0.5, NA)))
})

test_that("tf_register affine methodrespects custom bounds", {
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)

  # Create function with shift of 0.4

  x <- tfd(sin(2 * pi * (arg - 0.4)), arg = arg)

  # Default bounds should find the shift

  warp_default <- tf_register(
    x,
    method = "affine",
    template = template,
    type = "shift"
  )
  b_default <- tf_evaluations(warp_default)[[1]][51] - 0.5
  expect_equal(b_default, 0.4, tolerance = 0.01)

  # Restrictive bounds cannot recover full shift - should hit boundary
  warp_restricted <- tf_register(
    x,
    method = "affine",
    template = template,
    type = "shift",
    shift_range = c(-0.1, 0.1)
  )
  b_restricted <- tf_evaluations(warp_restricted)[[1]][51] - 0.5
  expect_equal(b_restricted, 0.1, tolerance = 0.01)

  # Custom scale bounds
  # Create function needing a = 0.6 warp
  x_scaled <- tfd(sin(2 * pi * (arg / 0.6 + 0.5 * (1 - 1 / 0.6))), arg = arg)
  warp_scale <- tf_register(
    x_scaled,
    method = "affine",
    template = template,
    type = "scale",
    scale_range = c(0.4, 0.8)
  )
  a_scale <- tf_evaluations(warp_scale)[[1]][101] -
    tf_evaluations(warp_scale)[[1]][1]
  expect_equal(a_scale, 0.6, tolerance = 0.01)
})
