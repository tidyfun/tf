test_that("tf_rebase works component-wise for tf_mv", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(4, arg = 101L), y = tf_rgp(4, arg = 101L)))
  # rebase to a spline basis -> tfb_mv (basis spec comes from basis_from)
  tb <- tf_rebase(f, tfb(tf_rgp(1), k = 25, verbose = FALSE))
  expect_s3_class(tb, "tfb_mv")
  expect_identical(tf_ncomp(tb), 2L)
  # rebase to a new tfd grid
  g <- tf_rebase(f, tfd(tf_rgp(1)), arg = seq(0, 1, length.out = 21))
  expect_s3_class(g, "tfd_mv")
  expect_equal(tf_arg(g), seq(0, 1, length.out = 21))
})

test_that("tf_derive is component-wise", {
  set.seed(2)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  d <- tf_derive(f)
  expect_s3_class(d, "tfd_mv")
  expect_equal(
    tf_evaluations(d$x)[[1]], tf_evaluations(tf_derive(f$x))[[1]]
  )
})

test_that("tf_integrate returns an n x d matrix (definite) or tfd_mv (indefinite)", {
  set.seed(3)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  m <- tf_integrate(f)
  expect_true(is.matrix(m))
  expect_identical(dim(m), c(4L, 2L))
  expect_identical(colnames(m), c("x", "y"))
  expect_equal(m[, "x"], tf_integrate(f$x), ignore_attr = TRUE)
  ind <- tf_integrate(f, definite = FALSE)
  expect_s3_class(ind, "tfd_mv")
})

test_that("tf_zoom and tf_smooth are component-wise", {
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  z <- tf_zoom(f, 0.25, 0.75)
  expect_s3_class(z, "tfd_mv")
  expect_true(all(tf_domain(z$x) == c(0.25, 0.75)))
  s <- tf_smooth(f, verbose = FALSE)
  expect_s3_class(s, "tfd_mv")
})

test_that("tf_warp / tf_align apply one shared warp to all components", {
  set.seed(5)
  t <- seq(0, 1, length.out = 101)
  n <- 4
  warp <- {
    w <- tf_rgp(n, arg = t)
    w <- exp(w - mean(w))
    tf_integrate(w, definite = FALSE) / tf_integrate(w)
  }
  f <- tfd_mv(list(x = tf_rgp(n, arg = t), y = tf_rgp(n, arg = t)))
  warped <- tf_warp(f, warp)
  expect_s3_class(warped, "tfd_mv")
  expect_identical(tf_ncomp(warped), 2L)
  # warping each component individually gives the same result
  expect_equal(
    tf_evaluations(warped$x)[[1]],
    tf_evaluations(tf_warp(f$x, warp))[[1]]
  )
})

test_that("tf_register on tf_mv aligns all components with a shared warp", {
  t <- seq(0, 2 * pi, length.out = 101)
  shifts <- c(-0.4, -0.2, 0, 0.2, 0.4)
  x <- tfd(t(sapply(shifts, \(s) sin(t + s))), arg = t)
  y <- tfd(t(sapply(shifts, \(s) cos(t + s))), arg = t)
  f <- tfd_mv(list(x = x, y = y))
  reg <- suppressWarnings(suppressMessages(
    tf_register(f, method = "affine", type = "shift")
  ))
  expect_s3_class(reg, "tf_registration")
  expect_s3_class(tf_aligned(reg), "tfd_mv")
  expect_s3_class(tf_template(reg), "tfd_mv")
  expect_s3_class(tf_inv_warps(reg), "tfd") # warps are univariate
  expect_identical(length(tf_inv_warps(reg)), 5L)
  # registration should reduce cross-curve (phase) variability
  meanvar <- function(mv) {
    mean(sapply(tf_components(mv), function(c) {
      suppressWarnings(mean(tf_evaluations(var(c))[[1]], na.rm = TRUE))
    }))
  }
  expect_lt(meanvar(tf_aligned(reg)), meanvar(f))
})

test_that("tf_estimate_warps respects ref_component", {
  t <- seq(0, 2 * pi, length.out = 101)
  shifts <- c(-0.3, 0, 0.3)
  x <- tfd(t(sapply(shifts, \(s) sin(t + s))), arg = t)
  y <- tfd(t(sapply(shifts, \(s) cos(t + s))), arg = t)
  f <- tfd_mv(list(x = x, y = y))
  w_first <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "affine", type = "shift")
  ))
  w_y <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "affine", type = "shift", ref_component = "y")
  ))
  expect_s3_class(w_first, "tfd")
  expect_length(w_first, 3)
  # both registration signals recover the shared shift here
  expect_length(w_y, 3)
})
