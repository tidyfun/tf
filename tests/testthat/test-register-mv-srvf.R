mv_registration_meanvar <- function(mv) {
  mean(sapply(tf_components(mv), function(comp) {
    suppressWarnings(mean(tf_evaluations(var(comp))[[1]], na.rm = TRUE))
  }))
}

make_shifted_bump_mv <- function(t = seq(0, 1, length.out = 51)) {
  centers <- c(0.4, 0.5, 0.6)
  bump <- function(center) exp(-80 * (t - center)^2)
  x <- tfd(t(sapply(centers, bump)), arg = t)
  y <- tfd(t(sapply(centers, \(center) 0.5 * bump(center))), arg = t)
  tfd_mv(list(x = x, y = y))
}

make_shape_mv <- function(t = seq(0, 1, length.out = 51)) {
  base <- rbind(t, t^2)
  scales <- c(1, 0.7, 1.3)
  angles <- c(0, 0.4, -0.25)
  offsets <- rbind(c(0.2, -0.1), c(0.4, -0.2), c(0.6, -0.3))
  beta <- array(
    NA_real_,
    dim = c(3, length(t), 2),
    dimnames = list(c("a", "b", "c"), NULL, c("x", "y"))
  )

  for (i in seq_len(3)) {
    rot <- matrix(
      c(cos(angles[i]), sin(angles[i]), -sin(angles[i]), cos(angles[i])),
      nrow = 2
    )
    curve <- scales[i] *
      (rot %*% base) +
      matrix(offsets[i, ], nrow = 2, ncol = length(t))
    beta[i, , 1] <- curve[1, ]
    beta[i, , 2] <- curve[2, ]
  }
  tfd_mv(beta, arg = t)
}

test_that("srvf_mv estimates true multivariate SRVF warps for tf_mv", {
  skip_if_not_installed("fdasrvf")

  f <- make_shifted_bump_mv()
  warps <- tf_estimate_warps(f, method = "srvf_mv", max_iter = 2)
  aligned <- tf_align(f, warps)
  reg <- tf_register(f, method = "srvf_mv", max_iter = 2)

  expect_s3_class(warps, "tfd")
  expect_s3_class(attr(warps, "template"), "tfd_mv")
  expect_s3_class(aligned, "tfd_mv")
  expect_s3_class(reg, "tf_registration")
  expect_s3_class(tf_aligned(reg), "tfd_mv")
  expect_s3_class(tf_template(reg), "tfd_mv")
  expect_s3_class(tf_inv_warps(reg), "tfd")
  expect_identical(tf_domain(warps), tf_domain(f))
  expect_identical(tf_arg(warps), tf_arg(f))
  expect_lt(
    mv_registration_meanvar(tf_aligned(reg)),
    0.1 * mv_registration_meanvar(f)
  )
})

test_that("srvf_mv supports explicit tf_mv templates", {
  skip_if_not_installed("fdasrvf")

  f <- make_shifted_bump_mv()
  template <- f[2]
  warps <- tf_estimate_warps(
    f,
    method = "srvf_mv",
    template = template
  )

  expect_s3_class(warps, "tfd")
  expect_identical(attr(warps, "template"), template)
  expect_length(warps, length(f))
  expect_identical(tf_arg(warps), tf_arg(f))
})

test_that("srvf_mv keeps shape options out of tf_register", {
  skip_if_not_installed("fdasrvf")

  f <- make_shifted_bump_mv()
  expect_error(
    tf_estimate_warps(f, method = "srvf_mv", rotation = TRUE),
    "tf_register_shape"
  )
  expect_error(
    tf_estimate_warps(f$x, method = "srvf_mv"),
    "only available for .*tf_mv"
  )
})

test_that("srvf_mv rejects non shared-grid tf_mv inputs", {
  skip_if_not_installed("fdasrvf")

  t1 <- seq(0, 1, length.out = 31)
  t2 <- seq(0, 1, length.out = 41)
  f <- tfd_mv(list(
    x = tfd(matrix(sin(2 * pi * t1), nrow = 1), arg = t1),
    y = tfd(matrix(cos(2 * pi * t2), nrow = 1), arg = t2)
  ))

  expect_error(
    tf_estimate_warps(f, method = "srvf_mv"),
    "one shared argument grid"
  )
})

test_that("tf_register_shape aligns translated, rotated, and scaled curves", {
  skip_if_not_installed("fdasrvf")

  f <- make_shape_mv()
  reg <- tf_register_shape(f, max_iter = 2)

  expect_s3_class(reg, "tf_shape_registration")
  expect_s3_class(reg, "tf_registration")
  expect_s3_class(tf_aligned(reg), "tfd_mv")
  expect_s3_class(tf_inv_warps(reg), "tfd")
  expect_s3_class(tf_template(reg), "tfd_mv")
  expect_equal(dim(tf_rotations(reg)), c(2, 2, length(f)))
  expect_length(tf_scales(reg), length(f))
  expect_true(all(is.finite(tf_scales(reg))))
  expect_true(all(tf_scales(reg) > 0))
  expect_lt(
    mv_registration_meanvar(tf_aligned(reg)),
    0.1 * mv_registration_meanvar(f)
  )
})

test_that("tf_register_shape rejects closed-curve mode", {
  skip_if_not_installed("fdasrvf")

  f <- make_shape_mv()
  expect_error(
    tf_register_shape(f, max_iter = 1, mode = "C"),
    "not supported"
  )
})

test_that("tf_register_shape can disable rotation and scaling", {
  skip_if_not_installed("fdasrvf")

  f <- make_shape_mv()
  reg <- tf_register_shape(f, max_iter = 1, rotation = FALSE, scale = FALSE)

  expect_equal(unname(tf_scales(reg)), rep(1, length(f)))
  expect_equal(names(tf_scales(reg)), names(f))
  expect_equal(
    unname(tf_rotations(reg)),
    array(diag(2), dim = c(2, 2, length(f)))
  )
  expect_equal(dimnames(tf_rotations(reg))[[3]], names(f))
})

test_that("tf_shape_registration subsetting keeps shape outputs aligned", {
  skip_if_not_installed("fdasrvf")

  f <- make_shape_mv()
  reg <- tf_register_shape(f, max_iter = 1)
  sub <- reg[1:2]
  sub_named <- reg[c("a", "c")]

  expect_s3_class(sub, "tf_shape_registration")
  expect_length(tf_aligned(sub), 2)
  expect_length(tf_inv_warps(sub), 2)
  expect_equal(dim(tf_rotations(sub)), c(2, 2, 2))
  expect_length(tf_scales(sub), 2)
  expect_equal(dim(tf_rotations(sub_named)), c(2, 2, 2))
  expect_equal(names(tf_scales(sub_named)), c("a", "c"))
})

test_that("summary.tf_registration reports a numeric amplitude-variance reduction for mv (#249)", {
  skip_if_not_installed("fdasrvf")
  set.seed(123)
  fm <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  reg <- tf_register(fm, method = "srvf_mv")
  expect_no_warning(s <- summary(reg))
  expect_true(is.numeric(s$amp_var_reduction))
  expect_true(is.finite(s$amp_var_reduction))
  out <- capture.output(print(s))
  expect_false(any(grepl("not computable", out)))
})

test_that("summary.tf_shape_registration reports rotation angles and scale deciles", {
  skip_if_not_installed("fdasrvf")
  f <- make_shape_mv()
  reg <- tf_register_shape(f, max_iter = 1)
  expect_no_warning(s <- summary(reg))
  expect_s3_class(s, "summary.tf_shape_registration")
  expect_true(is.numeric(s$rotation_angles_deg))
  expect_true(is.numeric(s$scale_quantiles))
  out <- capture.output(print(s))
  expect_true(any(grepl("Rotation angles", out)))
  expect_true(any(grepl("Scale factors", out)))
})
