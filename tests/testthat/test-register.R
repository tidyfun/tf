# --- tf_warp / tf_unwarp -------------------------------------------------------

quiet_expected_registration_warnings <- function(expr) {
  expected_patterns <- c(
    "evaluations were `NA`",
    "All .* evaluations on `arg =",
    "Returning irregular <tfd>\\.",
    "Returning regular data <tfd_reg> on the reduced grid\\.",
    "argument-wise `mean` were `NA`",
    "Attempting <tfd_irreg> - <tfd_irreg> for different argument values",
    "interpolate = FALSE.*no values present",
    "`NA`s created\\.",
    "collapsing to unique 'x' values"
  )

  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      is_expected <- any(vapply(
        expected_patterns,
        \(pat) grepl(pat, msg, perl = TRUE),
        logical(1)
      ))
      if (is_expected) {
        invokeRestart("muffleWarning")
      }
      stop(w)
    }
  )
}

test_that("tf_warp and tf_unwarp are inverses", {
  withr::local_seed(1234)
  # Two representative domains: one non-standard, one wide
  min_t <- c(-2, 1)
  max_t <- c(3, 10)
  walk2(min_t, max_t, function(min_t, max_t) {
    t <- seq(min_t, max_t, length.out = 101)
    f0 <- tfd(sin((t - min_t) / (max_t - min_t) * 2 * pi), t)
    x <- rep(f0, 10)
    w <- tf_rgp(10, t) |> exp() |> tf_integrate(definite = FALSE)
    w_min <- tf_fmin(w)
    w_max <- tf_fmax(w)
    w <- (w - w_min) / (w_max - w_min) * (max_t - min_t) + min_t

    # Warp: class, length, domain
    unreg <- tf_warp(x, w)
    expect_s3_class(unreg, "tfd_reg")
    expect_length(unreg, length(x))
    expect_identical(tf_domain(unreg), tf_domain(x))

    # keep_new_arg = TRUE returns tfd_irreg
    expect_s3_class(tf_warp(x, w, keep_new_arg = TRUE), "tfd_irreg")
    expect_s3_class(tf_unwarp(x, w, keep_new_arg = TRUE), "tfd_irreg")

    # Both inverse directions
    expect_equal(tf_unwarp(tf_warp(x, w), w), x, tolerance = 0.1)
    expect_equal(tf_warp(tf_unwarp(x, w), w), x, tolerance = 0.1)
  })
})

test_that("tf_warp/tf_unwarp identity roundtrip", {
  t <- seq(-1, 2, length.out = 50)
  f0 <- tfd(cos(t), t)
  x <- rep(f0, 5)
  w <- rep(tfd(t, arg = t), 5)
  # Identity warp: both directions exact
  expect_identical(tf_unwarp(tf_warp(x, w), w), x)
  expect_identical(tf_warp(tf_unwarp(x, w), w), x)
  # Also with keep_new_arg
  expect_identical(tf_unwarp(tf_warp(x, w, keep_new_arg = TRUE), w), x)
  expect_identical(tf_warp(tf_unwarp(x, w, keep_new_arg = TRUE), w), x)
})

test_that("tf_warp and tf_unwarp accept tfb warps", {
  t <- seq(-10, 10, length.out = 101)
  x <- tfd(
    t(sapply(c(-0.2, 0, 0.15), \(s) sin((t - s) * 0.3))),
    arg = t
  )
  warp_tfd <- rep(tfd(t, arg = t), length(x))
  warp_tfb <- suppressMessages(tfb(warp_tfd, k = 9))

  expect_no_error(x_unreg <- tf_warp(x, warp_tfb))
  expect_no_error(x_reg <- tf_unwarp(x_unreg, warp_tfb))
  expect_s3_class(x_unreg, "tfd")
  expect_s3_class(x_reg, "tfd")
})

test_that("tf_warp/tf_unwarp reset keep_new_arg for tfb and warn", {
  t <- seq(0, 1, length.out = 51)
  x_tfb <- suppressMessages(tfb(
    matrix(sin(2 * pi * t), nrow = 1),
    arg = t,
    k = 9
  ))
  warp <- rep(tfd(t, arg = t), length(x_tfb))

  expect_warning(
    warped <- tf_warp(x_tfb, warp, keep_new_arg = TRUE),
    "reset to FALSE"
  )
  expect_s3_class(warped, "tfb")

  expect_warning(
    unwarped <- tf_unwarp(x_tfb, warp, keep_new_arg = TRUE),
    "reset to FALSE"
  )
  expect_s3_class(unwarped, "tfb")
})

test_that("tf_unwarp handles irregular grids with non-domain-preserving warps", {
  arg_x <- list(
    seq(0, 1, length.out = 41),
    seq(0, 1, length.out = 73)
  )
  x <- tfd(
    list(
      sin(2 * pi * arg_x[[1]]),
      cos(2 * pi * arg_x[[2]])
    ),
    arg = arg_x
  )

  warp_arg <- seq(0, 1, length.out = 101)
  warp_tfd <- tfd(
    rbind(
      warp_arg + 0.15,
      warp_arg - 0.12
    ),
    arg = warp_arg
  )
  warp_tfb <- suppressMessages(tfb(warp_tfd, k = 9))

  ret_irreg <- tf_unwarp(x, warp_tfd, keep_new_arg = TRUE)
  expect_s3_class(ret_irreg, "tfd_irreg")
  expect_length(ret_irreg, length(x))
  expect_identical(tf_domain(ret_irreg), tf_domain(x))

  ret_irreg_tfb <- tf_unwarp(x, warp_tfb, keep_new_arg = TRUE)
  expect_s3_class(ret_irreg_tfb, "tfd_irreg")
  expect_length(ret_irreg_tfb, length(x))
  expect_identical(tf_domain(ret_irreg_tfb), tf_domain(x))

  ret_grid <- quiet_expected_registration_warnings(
    tf_unwarp(x, warp_tfd, keep_new_arg = FALSE)
  )
  expect_length(ret_grid, length(x))
  expect_identical(tf_domain(ret_grid), tf_domain(x))
})


# --- tf_register main ---------------------------------------------------------

test_that("tf_register works for SRVF and FDA methods", {
  skip_if_not_installed("fdasrvf")
  skip_if_not_installed("fda")
  withr::local_seed(1234)

  n_curves <- 10
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(n_curves), \(i) {
    phase_shift <- runif(1, -pi / 2, pi / 2)
    amplitude <- runif(1, 0.8, 1.2)
    amplitude * sin(t + phase_shift) + rnorm(length(t), 0, 0.1)
  })

  check_warp <- function(warp, x, label = "") {
    domain <- tf_domain(x)
    expect_s3_class(warp, "tfd")
    expect_length(warp, length(x))
    expect_identical(tf_domain(warp), domain)
    expect_true(all(tf_fmin(warp) >= domain[1]), label = label)
    expect_true(all(tf_fmax(warp) <= domain[2]), label = label)
  }

  x <- tfd(t(data))

  # SRVF: default (Karcher mean) and explicit template
  check_warp(tf_register(x), x, "SRVF default")
  check_warp(tf_register(x, template = mean(x)), x, "SRVF template")

  # FDA: default and non-default crit (tests ... passthrough)
  check_warp(
    quiet_expected_registration_warnings(tf_register(x, method = "fda")),
    x,
    "FDA default"
  )
  check_warp(
    quiet_expected_registration_warnings(tf_register(
      x,
      method = "fda",
      crit = 1
    )),
    x,
    "FDA crit=1"
  )

  # tfd_irreg rejected
  expect_error(
    tf_register(tfd(t(data)) |> tf_sparsify(.2)),
    "objects cannot be registered"
  )
})

test_that("tf_register exposes FDA warp basis controls", {
  skip_if_not_installed("fda")
  withr::local_seed(123)

  t <- seq(0, 1, length.out = 81)
  x <- tfd(
    t(sapply(c(-0.07, 0, 0.06), \(s) sin(2 * pi * (t + s)))),
    arg = t
  )

  w_default <- quiet_expected_registration_warnings(
    tf_register(x, method = "fda")
  )
  w_explicit_default <- quiet_expected_registration_warnings(
    tf_register(
      x,
      method = "fda",
      fda_nbasis = 6L,
      fda_lambda = 0
    )
  )
  expect_equal(as.matrix(w_default), as.matrix(w_explicit_default))

  expect_no_error(
    w_tuned <- tf_register(
      x,
      method = "fda",
      crit = 2,
      fda_nbasis = 8L,
      fda_lambda = 1e-3
    )
  )
  expect_s3_class(w_tuned, "tfd")
  expect_length(w_tuned, length(x))
  expect_identical(tf_domain(w_tuned), tf_domain(x))

  expect_error(
    tf_register(x, method = "fda", fda_nbasis = 1L),
    ">= 2"
  )
  expect_error(
    tf_register(x, method = "fda", fda_lambda = -1),
    ">= 0"
  )
})

test_that("tf_register dispatches correctly for tfb subclasses", {
  withr::local_seed(1234)
  t <- seq(0, 2 * pi, length.out = 100)
  data <- sapply(seq_len(5), \(i) {
    sin(t + runif(1, -0.5, 0.5))
  })

  # Use cheap affine method (not SRVF/FDA) to test class dispatch
  x_tfb <- suppressMessages(tfb(t(data), k = 20))
  warp_tfb <- quiet_expected_registration_warnings(
    tf_register(x_tfb, method = "affine", type = "shift")
  )
  expect_s3_class(warp_tfb, "tfd")
  expect_length(warp_tfb, length(x_tfb))
  expect_identical(tf_domain(warp_tfb), tf_domain(x_tfb))

  x_fpc <- tfb_fpc(t(data), pve = .95)
  warp_fpc <- quiet_expected_registration_warnings(
    tf_register(x_fpc, method = "affine", type = "shift")
  )
  expect_s3_class(warp_fpc, "tfd")
  expect_length(warp_fpc, length(x_fpc))
  expect_identical(tf_domain(warp_fpc), tf_domain(x_fpc))
})

test_that("tf_register SRVF/FDA works for tfb subclasses", {
  skip_if_not_installed("fdasrvf")
  skip_if_not_installed("fda")
  withr::local_seed(1234)

  t <- seq(0, 2 * pi, length.out = 61)
  data <- sapply(seq_len(4), \(i) sin(t + runif(1, -0.4, 0.4)))

  x_tfb <- suppressMessages(tfb(t(data), k = 12))
  x_fpc <- tfb_fpc(t(data), pve = .95)

  warp_srvf <- tf_register(x_tfb, method = "srvf")
  expect_s3_class(warp_srvf, "tfd")
  expect_length(warp_srvf, length(x_tfb))
  expect_identical(tf_domain(warp_srvf), tf_domain(x_tfb))

  warp_fda <- tf_register(x_fpc, method = "fda")
  expect_s3_class(warp_fda, "tfd")
  expect_length(warp_fda, length(x_fpc))
  expect_identical(tf_domain(warp_fda), tf_domain(x_fpc))
})

test_that("tf_register validates SRVF/FDA templates", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(2 * pi * t), cos(2 * pi * t))), arg = t)

  template_bad_len <- tfd(
    t(cbind(sin(2 * pi * t), cos(2 * pi * t), sin(4 * pi * t))),
    arg = t
  )
  expect_error(
    tf_register(x, method = "srvf", template = template_bad_len),
    "length 1 or the same length"
  )
  expect_error(
    tf_register(x, method = "fda", template = template_bad_len),
    "length 1 or the same length"
  )

  template_bad_domain <- tfd(
    sin(pi * seq(0, 2, length.out = 51)),
    arg = seq(0, 2, length.out = 51)
  )
  expect_error(
    tf_register(x, method = "srvf", template = template_bad_domain),
    "same domain"
  )
  expect_error(
    tf_register(x, method = "fda", template = template_bad_domain),
    "same domain"
  )

  t2 <- seq(0, 1, length.out = 61)
  template_bad_grid <- tfd(sin(2 * pi * t2), arg = t2)
  expect_error(
    tf_register(x, method = "srvf", template = template_bad_grid),
    "same grid"
  )
  expect_error(
    tf_register(x, method = "fda", template = template_bad_grid),
    "same grid"
  )
})

test_that("tf_register SRVF/FDA reject unknown method-specific arguments", {
  skip_if_not_installed("fdasrvf")
  skip_if_not_installed("fda")
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(2 * pi * t), sin(2 * pi * (t + 0.05)))), arg = t)

  expect_error(
    tf_register(x, method = "srvf", unknown_arg = 1),
    "unused argument|unused arguments|formal argument"
  )
  expect_error(
    tf_register(x, method = "fda", unknown_arg = 1),
    "unused argument|unused arguments|formal argument"
  )
})

test_that("tf_register FDA explicit template path is exercised", {
  skip_if_not_installed("fda")
  withr::local_seed(4321)
  t <- seq(0, 1, length.out = 101)
  x <- tfd(
    t(sapply(c(-0.08, 0, 0.08), \(s) sin(2 * pi * (t + s)))),
    arg = t
  )
  template <- mean(x)

  w_default <- tf_register(x, method = "fda")
  w_template <- tf_register(x, method = "fda", template = template)
  expect_equal(as.matrix(w_default), as.matrix(w_template), tolerance = 0.15)
})


# --- Landmark detection -------------------------------------------------------

test_that("tf_landmarks_extrema finds correct extrema and zero crossings", {
  # Single-period sin: quantitative accuracy with smooth = TRUE (default path)
  t1 <- seq(0, 2 * pi, length.out = 101)
  phase_shifts <- c(0, 0.5, -0.3)
  x1 <- tfd(t(cbind(sin(t1), sin(t1 + 0.5), sin(t1 - 0.3))), arg = t1)

  expected_max <- pi / 2 - phase_shifts
  maxima <- tf_landmarks_extrema(x1, "max")
  expect_true(is.matrix(maxima))
  expect_equal(dim(maxima), c(3, 1))
  expect_equal(as.numeric(maxima), expected_max, tolerance = 0.1)

  expected_min <- 3 * pi / 2 - phase_shifts
  minima <- tf_landmarks_extrema(x1, "min")
  expect_equal(dim(minima), c(3, 1))
  expect_equal(as.numeric(minima), expected_min, tolerance = 0.1)

  both <- tf_landmarks_extrema(x1, "both")
  expect_equal(dim(both), c(3, 2))
  expect_equal(both[, 1], maxima[, 1])
  expect_equal(both[, 2], minima[, 1])
  expect_equal(attr(both, "feature_types"), c("max", "min"))

  # 2-period sin: structural checks + all types
  t2 <- seq(0, 2, length.out = 201)
  template <- sin(2 * pi * t2)
  x2 <- tfd(
    t(sapply(c(0, 0.03, -0.03), \(s) {
      approx(t2, template, xout = t2 + s, rule = 2)$y
    })),
    arg = t2
  )

  lm_all <- tf_landmarks_extrema(x2, "all")
  types <- attr(lm_all, "feature_types")
  expect_true(ncol(lm_all) >= 5)
  expect_true("max" %in% types)
  expect_true("min" %in% types)
  expect_true("zero" %in% types)
  # Each row sorted
  for (i in seq_len(nrow(lm_all))) {
    non_na <- lm_all[i, !is.na(lm_all[i, ])]
    expect_true(all(diff(non_na) > 0))
  }
})

test_that("tf_landmarks_extrema drops boundary features", {
  t <- seq(0, 1, length.out = 101)
  vals <- pnorm(t, 0.2, 0.03) + 0.3 * dnorm(t, 0.7, 0.08) - 0.5
  x <- tfd(
    t(sapply(c(0, 0.02, -0.02), \(s) {
      approx(t, vals, xout = t + s, rule = 2)$y
    })),
    arg = t
  )

  lm <- tf_landmarks_extrema(x, "both")
  expect_equal(ncol(lm), 1)
  expect_equal(attr(lm, "feature_types"), "max")
  expect_true(all(abs(lm[, 1] - 0.7) < 0.05, na.rm = TRUE))
})

test_that("tf_landmarks_extrema detects landmarks on irregular tfd", {
  set.seed(123)
  peak_locs <- c(0.4, 0.5, 0.6, 0.45)
  arg_list <- lapply(1:4, \(i) sort(c(0, 1, runif(80))))
  vals_list <- lapply(seq_along(peak_locs), \(i) {
    dnorm(arg_list[[i]], mean = peak_locs[i], sd = 0.1)
  })
  x_irr <- tfd(vals_list, arg = arg_list)
  expect_true(inherits(x_irr, "tfd_irreg"))

  lm <- tf_landmarks_extrema(x_irr, "max")
  expect_true(is.matrix(lm))
  expect_equal(nrow(lm), 4)
  expect_equal(ncol(lm), 1)
  expect_equal(as.numeric(lm), peak_locs, tolerance = 0.05)

  lm_both <- tf_landmarks_extrema(x_irr, "both")
  expect_equal(ncol(lm_both), 1)
  expect_equal(attr(lm_both, "feature_types"), "max")

  # Full chain: detection -> registration -> unwarp
  x_reg <- tfd(x_irr, arg = seq(0, 1, length.out = 101))
  warp <- tf_register(x_reg, method = "landmark", landmarks = lm)
  aligned <- tf_unwarp(x_reg, warp)
  aligned_peaks <- tf_where(aligned, value == max(value), "first")
  expect_equal(aligned_peaks, rep(mean(peak_locs), 4), tolerance = 0.03)
})

test_that("tf_landmarks_extrema threshold parameter filters rare features", {
  set.seed(101)
  t <- seq(0, 1, length.out = 201)

  make_curve <- function(main_peak, add_secondary = FALSE) {
    y <- dnorm(t, main_peak, 0.06)
    if (add_secondary) {
      y <- y + 0.8 * dnorm(t, 0.8, 0.04)
    }
    y
  }
  vals <- rbind(
    make_curve(0.38, add_secondary = TRUE),
    make_curve(0.40),
    make_curve(0.42),
    make_curve(0.39),
    make_curve(0.41)
  )
  x <- tfd(vals, arg = t)

  # threshold = 0.5 -> only main peak
  lm_strict <- tf_landmarks_extrema(x, "max", threshold = 0.5)
  expect_true(is.matrix(lm_strict))
  expect_equal(ncol(lm_strict), 1)
  expect_true(all(abs(lm_strict[, 1] - 0.4) < 0.1, na.rm = TRUE))

  # threshold = 0.2 -> both peaks, warns about missing landmarks
  expect_warning(
    lm_loose <- tf_landmarks_extrema(x, "max", threshold = 0.2),
    "missing landmark"
  )
  expect_true(is.matrix(lm_loose))
  expect_equal(ncol(lm_loose), 2)
  expect_true(all(abs(lm_loose[, 1] - 0.4) < 0.1, na.rm = TRUE))
  expect_true(any(abs(lm_loose[, 2] - 0.8) < 0.1, na.rm = TRUE))
})

test_that("tf_landmarks_extrema warns when no stable landmarks found", {
  set.seed(202)
  t <- seq(0, 1, length.out = 101)
  x_const <- tfd(rbind(rep(1, 101), rep(2, 101), rep(3, 101)), arg = t)

  expect_warning(
    lm <- tf_landmarks_extrema(x_const, "max"),
    "No stable landmarks"
  )
  expect_true(is.matrix(lm))
  expect_equal(ncol(lm), 0)
  expect_equal(nrow(lm), 3)
  expect_equal(attr(lm, "feature_types"), character(0))

  expect_warning(
    lm_both <- tf_landmarks_extrema(x_const, "both"),
    "No stable landmarks"
  )
  expect_equal(ncol(lm_both), 0)
  expect_equal(nrow(lm_both), 3)
})


# --- Landmark registration ----------------------------------------------------

test_that("landmark registration aligns to default and custom template", {
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.3, 0.5, 0.7)
  x <- tfd(
    t(sapply(peak_locs, \(s) dnorm(t, mean = s, sd = 0.1))),
    arg = t
  )
  landmarks <- matrix(peak_locs, ncol = 1)

  # Default template (mean of landmarks = 0.5)
  warp <- tf_register(x, method = "landmark", landmarks)
  expect_s3_class(warp, "tfd")
  expect_length(warp, length(x))
  expect_identical(tf_domain(warp), tf_domain(x))
  template_landmark <- mean(peak_locs)
  warp_at_template <- as.numeric(warp[, template_landmark])
  expect_equal(warp_at_template, peak_locs, tolerance = 1e-10)

  # Custom template
  custom_template <- 0.4
  warp2 <- tf_register(
    x,
    method = "landmark",
    landmarks = landmarks,
    template_landmarks = custom_template
  )
  warp_at_custom <- as.numeric(warp2[, custom_template])
  expect_equal(warp_at_custom, peak_locs, tolerance = 1e-10)

  # After unwarping with custom template, peaks aligned near template
  x_aligned <- tf_unwarp(x, warp2)
  aligned_peaks <- tf_where(x_aligned, value == max(value), "first")
  expect_equal(aligned_peaks, rep(custom_template, 3), tolerance = 0.02)
})

test_that("register_landmark handles NA landmarks correctly", {
  set.seed(42)
  t <- seq(0, 1, length.out = 101)
  peak_locs <- c(0.4, 0.45, 0.5, 0.55, 0.6)
  x <- tfd(t(sapply(peak_locs, \(p) dnorm(t, p, 0.08))), arg = t)

  lm_mat <- matrix(NA_real_, nrow = 5, ncol = 2)
  lm_mat[, 1] <- peak_locs
  lm_mat[1:3, 2] <- c(0.75, 0.78, 0.80)

  warp <- tf_register(x, method = "landmark", landmarks = lm_mat)
  expect_s3_class(warp, "tfd")
  expect_length(warp, 5)

  # All warps strictly increasing with correct endpoints
  warp_evals <- tf_evaluations(warp)
  domain <- tf_domain(x)
  for (i in seq_len(5)) {
    expect_true(
      all(diff(warp_evals[[i]]) > 0),
      info = paste("Warp", i, "is not strictly increasing")
    )
    vals <- warp_evals[[i]]
    expect_equal(vals[1], domain[1])
    expect_equal(vals[length(vals)], domain[2])
  }
})

test_that("tf_register landmark method validates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  # Wrong number of rows
  expect_error(
    tf_register(x, method = "landmark", matrix(c(0.3, 0.5, 0.7), ncol = 1)),
    "rows"
  )

  # Non-increasing landmarks
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
    "strictly inside the domain"
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


# --- Affine registration ------------------------------------------------------

test_that("affine registration produces linear warps for all types", {
  # Shift data on [0, 2*pi]
  t1 <- seq(0, 2 * pi, length.out = 101)
  template1 <- tfd(sin(t1), arg = t1)
  x_shift <- tfd(
    t(sapply(c(-0.3, -0.1, 0, 0.1, 0.3), \(s) sin(t1 + s))),
    arg = t1
  )

  # Scale data on [0, 1]
  t2 <- seq(0, 1, length.out = 101)
  x_scale <- tfd(
    t(cbind(sin(t2 * pi), sin(t2 * pi * 0.9), sin(t2 * pi * 1.1))),
    arg = t2
  )

  # Shift_scale data on [0, 2*pi]
  x_ss <- tfd(
    t(cbind(sin(t1), sin(t1 * 0.95 + 0.2), sin(t1 * 1.05 - 0.1))),
    arg = t1
  )

  check_linear <- function(warp, type) {
    warp_evals <- tf_evaluations(warp)
    for (i in seq_along(warp_evals)) {
      expect_true(
        all(diff(warp_evals[[i]]) > 0),
        label = paste(type, "function", i, "monotonicity")
      )
      second_diff <- diff(diff(warp_evals[[i]]))
      expect_lt(
        max(abs(second_diff)),
        1e-10,
        label = paste(type, "function", i, "linearity")
      )
    }
  }

  check_linear(
    quiet_expected_registration_warnings(
      tf_register(
        x_shift,
        method = "affine",
        template = template1,
        type = "shift"
      )
    ),
    "shift"
  )
  check_linear(
    quiet_expected_registration_warnings(
      tf_register(x_scale, method = "affine", type = "scale")
    ),
    "scale"
  )
  check_linear(
    quiet_expected_registration_warnings(
      tf_register(x_ss, method = "affine", type = "shift_scale")
    ),
    "shift_scale"
  )

  # Scale centering: warp(midpoint) ~ midpoint
  warp_scale <- quiet_expected_registration_warnings(
    tf_register(x_scale, method = "affine", type = "scale")
  )
  center <- 0.5
  warp_at_center <- as.numeric(warp_scale[, center])
  expect_equal(warp_at_center, rep(center, 3), tolerance = 0.1)
})

test_that("affine shift produces warps that recover shift direction", {
  t <- seq(0, 2 * pi, length.out = 101)
  template <- tfd(sin(t), arg = t)

  x_shifted_left <- tfd(sin(t - 0.3), arg = t)
  x_shifted_right <- tfd(sin(t + 0.3), arg = t)

  warp_left <- quiet_expected_registration_warnings(
    tf_register(
      x_shifted_left,
      method = "affine",
      template = template,
      type = "shift"
    )
  )
  mean_shift_left <- mean(tf_evaluations(warp_left)[[1]] - t)

  warp_right <- quiet_expected_registration_warnings(
    tf_register(
      x_shifted_right,
      method = "affine",
      template = template,
      type = "shift"
    )
  )
  mean_shift_right <- mean(tf_evaluations(warp_right)[[1]] - t)

  expect_true(mean_shift_left > mean_shift_right)
})

test_that("affine registration aligns functions to template", {
  set.seed(4321)
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)
  template_vec <- tf_evaluations(template)[[1]]

  # Shift: shifted functions
  true_shifts <- c(-0.2, -0.1, 0.1, 0.15)
  x_shift <- do.call(
    c,
    lapply(true_shifts, \(s) {
      tfd(sin(2 * pi * (arg - s)), arg = arg)
    })
  )

  # Scale: scaled functions
  scale_factors <- c(0.7, 0.85, 1.15, 1.3)
  x_scale <- do.call(
    c,
    lapply(scale_factors, \(a) {
      inv_arg <- arg / a + 0.5 * (1 - 1 / a)
      tfd(sin(2 * pi * inv_arg), arg = arg)
    })
  )

  # Shift_scale: combined-transform functions
  params <- list(
    c(a = 0.9, b = 0.05),
    c(a = 1.1, b = -0.05),
    c(a = 0.95, b = 0.02)
  )
  x_ss <- do.call(
    c,
    lapply(params, \(p) {
      tfd(sin(2 * pi * ((arg - p["b"]) / p["a"])), arg = arg)
    })
  )

  check_alignment <- function(x, type) {
    warps <- quiet_expected_registration_warnings(
      tf_register(x, method = "affine", template = template, type = type)
    )
    x_reg <- quiet_expected_registration_warnings(tf_unwarp(x, warps))

    # Domain preserved (replaces former standalone smoke test)
    expect_identical(
      tf_domain(x_reg),
      tf_domain(x),
      label = paste(type, "domain preservation")
    )

    for (i in seq_along(x_reg)) {
      reg_on_grid <- tf_evaluate(x_reg[i], arg = arg)[[1]]
      valid <- !is.na(reg_on_grid)
      expect_gt(
        sum(valid),
        20,
        label = paste(type, "function", i, "valid points")
      )
      max_diff <- max(abs(reg_on_grid[valid] - template_vec[valid]))
      expect_lt(
        max_diff,
        0.01,
        label = paste(type, "function", i, "alignment")
      )
    }
  }

  check_alignment(x_shift, "shift")
  check_alignment(x_scale, "scale")
  check_alignment(x_ss, "shift_scale")
})

test_that("affine registered functions may have NA at boundaries", {
  t <- seq(0, 2 * pi, length.out = 101)
  true_shifts <- c(-0.8, 0.8)
  x <- tfd(t(sapply(true_shifts, \(s) sin(t + s))), arg = t)

  template <- tfd(sin(t), arg = t)
  warp <- quiet_expected_registration_warnings(
    tf_register(x, method = "affine", template = template, type = "shift")
  )
  x_aligned <- quiet_expected_registration_warnings(tf_unwarp(x, warp))

  evals <- tf_evaluations(x_aligned)
  expect_length(evals, length(true_shifts))
  # Warp values should extend outside domain for large shifts
  warp_evals <- tf_evaluations(warp)
  warp_ranges <- vapply(warp_evals, range, numeric(2))
  expect_true(
    any(warp_ranges[1, ] < tf_domain(x)[1]) ||
      any(warp_ranges[2, ] > tf_domain(x)[2])
  )
})

test_that("tf_register affine method validates input", {
  t <- seq(0, 1, length.out = 51)
  x <- tfd(t(cbind(sin(t * pi), sin(t * pi + 0.1))), arg = t)

  expect_error(
    tf_register(x, method = "affine", template = x),
    "length 1"
  )

  x2 <- tfd(
    sin(seq(0, 2, length.out = 51) * pi),
    arg = seq(0, 2, length.out = 51)
  )
  expect_error(
    tf_register(x, method = "affine", template = x2),
    "same domain"
  )

  expect_error(tf_register(x, method = "affine", shift_range = c(0.5)))
  expect_error(tf_register(x, method = "affine", shift_range = c(0.5, 0.3)))
  expect_error(tf_register(x, method = "affine", scale_range = c(-1, 2)))
  expect_error(tf_register(x, method = "affine", scale_range = c(0.5, NA)))
})

test_that("tf_register affine method respects custom bounds", {
  arg <- seq(0, 1, length.out = 101)
  template <- tfd(sin(2 * pi * arg), arg = arg)

  # Function with shift of 0.4
  x <- tfd(sin(2 * pi * (arg - 0.4)), arg = arg)

  # Default bounds find the shift
  warp_default <- quiet_expected_registration_warnings(
    tf_register(
      x,
      method = "affine",
      template = template,
      type = "shift"
    )
  )
  b_default <- tf_evaluations(warp_default)[[1]][51] - 0.5
  expect_equal(b_default, 0.4, tolerance = 0.01)

  # Restrictive bounds hit boundary
  warp_restricted <- quiet_expected_registration_warnings(
    tf_register(
      x,
      method = "affine",
      template = template,
      type = "shift",
      shift_range = c(-0.1, 0.1)
    )
  )
  b_restricted <- tf_evaluations(warp_restricted)[[1]][51] - 0.5
  expect_equal(b_restricted, 0.1, tolerance = 0.01)

  # Custom scale bounds
  x_scaled <- tfd(sin(2 * pi * (arg / 0.6 + 0.5 * (1 - 1 / 0.6))), arg = arg)
  warp_scale <- quiet_expected_registration_warnings(
    tf_register(
      x_scaled,
      method = "affine",
      template = template,
      type = "scale",
      scale_range = c(0.4, 0.8)
    )
  )
  a_scale <- tf_evaluations(warp_scale)[[1]][101] -
    tf_evaluations(warp_scale)[[1]][1]
  expect_equal(a_scale, 0.6, tolerance = 0.01)
})


# --- Procrustes Iteration (max_iter / tol) ------------------------------------

test_that("tf_register rejects invalid max_iter and tol", {
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(sapply(c(-0.3, 0, 0.3), \(s) sin(t + s))), arg = t)
  expect_error(tf_register(x, method = "affine", type = "shift", max_iter = 0L))
  expect_error(tf_register(
    x,
    method = "affine",
    type = "shift",
    max_iter = -1L
  ))
  expect_error(tf_register(x, method = "affine", type = "shift", tol = -1))
})

test_that("template forces single-pass regardless of max_iter", {
  withr::local_seed(123)
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(sapply(c(-0.3, 0, 0.3), \(s) sin(t + s))), arg = t)
  template <- tfd(matrix(sin(t), nrow = 1), arg = t)

  # Without template: explicit max_iter=1 matches default
  w_default <- quiet_expected_registration_warnings(
    tf_register(x, method = "affine", type = "shift")
  )
  w_explicit <- quiet_expected_registration_warnings(
    tf_register(x, method = "affine", type = "shift", max_iter = 1L)
  )
  expect_equal(as.matrix(w_default), as.matrix(w_explicit))

  # With template: max_iter=1 and max_iter=5 are identical
  w1 <- quiet_expected_registration_warnings(
    tf_register(
      x,
      method = "affine",
      type = "shift",
      template = template,
      max_iter = 1L
    )
  )
  w5 <- quiet_expected_registration_warnings(
    tf_register(
      x,
      method = "affine",
      type = "shift",
      template = template,
      max_iter = 5L
    )
  )
  expect_equal(as.matrix(w1), as.matrix(w5))
})

test_that("tf_register Procrustes iteration runs and improves for affine", {
  withr::local_seed(42)
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(sapply(c(-0.3, 0, 0.3), \(s) sin(t + s))), arg = t)

  w1 <- quiet_expected_registration_warnings(
    tf_register(x, method = "affine", type = "shift", max_iter = 1L)
  )
  w3 <- quiet_expected_registration_warnings(
    tf_register(x, method = "affine", type = "shift", max_iter = 3L)
  )
  expect_s3_class(w3, "tfd_reg")
  expect_length(w3, 3)

  # Procrustes iteration should not increase residual variance.
  # Interpolate to a common regular grid to avoid irregular-grid arithmetic warnings.
  aligned_1 <- quiet_expected_registration_warnings(tf_unwarp(x, w1))
  aligned_3 <- quiet_expected_registration_warnings(tf_unwarp(x, w3))
  arg <- tf_arg(x)
  aligned_1_mat <- quiet_expected_registration_warnings(
    as.matrix(tf_interpolate(aligned_1, arg = arg))
  )
  aligned_3_mat <- quiet_expected_registration_warnings(
    as.matrix(tf_interpolate(aligned_3, arg = arg))
  )

  center_1 <- matrix(
    colMeans(aligned_1_mat, na.rm = TRUE),
    nrow = nrow(aligned_1_mat),
    ncol = ncol(aligned_1_mat),
    byrow = TRUE
  )
  center_3 <- matrix(
    colMeans(aligned_3_mat, na.rm = TRUE),
    nrow = nrow(aligned_3_mat),
    ncol = ncol(aligned_3_mat),
    byrow = TRUE
  )
  var_1 <- mean((aligned_1_mat - center_1)^2, na.rm = TRUE)
  var_3 <- mean((aligned_3_mat - center_3)^2, na.rm = TRUE)
  expect_lte(var_3, var_1 + 1e-6)
})

test_that("tf_register Procrustes iteration handles irregular affine updates", {
  t <- seq(0, 2 * pi, length.out = 101)
  x <- tfd(t(sapply(c(-0.3, 0, 0.3), \(s) sin(t + s))), arg = t)

  expect_no_error(
    w <- quiet_expected_registration_warnings(
      tf_register(
        x,
        method = "affine",
        type = "shift",
        max_iter = 2L,
        shift_range = c(0.05, 0.2)
      )
    )
  )
  expect_s3_class(w, "tfd_reg")
  expect_length(w, length(x))
})

test_that("tf_register Procrustes iteration runs for FDA", {
  skip_if_not_installed("fda")
  withr::local_seed(42)
  t <- seq(0, 1, length.out = 101)
  x <- tfd(
    t(sapply(c(-0.05, 0, 0.05), \(s) sin(2 * pi * (t + s)))),
    arg = t
  )

  w <- tf_register(x, method = "fda", max_iter = 3L)
  expect_s3_class(w, "tfd_reg")
  expect_length(w, 3)
})

test_that("SRVF with template=NULL gives same result regardless of max_iter", {
  skip_if_not_installed("fdasrvf")
  withr::local_seed(42)
  t <- seq(0, 1, length.out = 101)
  x <- tfd(
    t(sapply(c(-0.05, 0, 0.05), \(s) sin(2 * pi * (t + s)))),
    arg = t
  )

  w1 <- tf_register(x, method = "srvf", max_iter = 1L)
  w5 <- tf_register(x, method = "srvf", max_iter = 5L)
  expect_equal(as.matrix(w1), as.matrix(w5))
})
