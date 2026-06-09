test_that("tf_rebase works component-wise for tf_mv", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(4, arg = 101L), y = tf_rgp(4, arg = 101L)))
  # rebase to a spline basis -> tfb_mv (basis spec comes from basis_from)
  basis_from <- tfb(tf_rgp(1), k = 25, verbose = FALSE)
  tb <- tf_rebase(f, basis_from)
  expect_s3_class(tb, "tfb_mv")
  expect_identical(tf_ncomp(tb), 2L)
  expect_equal(tb$x, tf_rebase(f$x, basis_from))
  expect_equal(tb$y, tf_rebase(f$y, basis_from))
  # rebase to a new tfd grid
  tfd_basis <- tfd(tf_rgp(1))
  g <- tf_rebase(f, tfd_basis, arg = seq(0, 1, length.out = 21))
  expect_s3_class(g, "tfd_mv")
  expect_equal(tf_arg(g), seq(0, 1, length.out = 21))
  expect_equal(g$x, tf_rebase(f$x, tfd_basis, arg = seq(0, 1, length.out = 21)))
  expect_equal(g$y, tf_rebase(f$y, tfd_basis, arg = seq(0, 1, length.out = 21)))
})

test_that("tf_derive is component-wise", {
  set.seed(2)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  d <- tf_derive(f)
  expect_s3_class(d, "tfd_mv")
  expect_equal(
    tf_evaluations(d$x)[[1]],
    tf_evaluations(tf_derive(f$x))[[1]]
  )
  expect_equal(
    tf_evaluations(d$y)[[2]],
    tf_evaluations(tf_derive(f$y))[[2]]
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
  expect_equal(m[, "y"], tf_integrate(f$y), ignore_attr = TRUE)
  ind <- tf_integrate(f, definite = FALSE)
  expect_s3_class(ind, "tfd_mv")
  expect_equal(ind$x, tf_integrate(f$x, definite = FALSE))
  expect_equal(ind$y, tf_integrate(f$y, definite = FALSE))
})

test_that("tf_zoom and tf_smooth are component-wise", {
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  z <- tf_zoom(f, 0.25, 0.75)
  expect_s3_class(z, "tfd_mv")
  expect_true(all(tf_domain(z$x) == c(0.25, 0.75)))
  expect_equal(z$x, tf_zoom(f$x, 0.25, 0.75))
  expect_equal(z$y, tf_zoom(f$y, 0.25, 0.75))
  s <- tf_smooth(f, verbose = FALSE)
  expect_s3_class(s, "tfd_mv")
  expect_equal(s$x, tf_smooth(f$x, verbose = FALSE))
  expect_equal(s$y, tf_smooth(f$y, verbose = FALSE))
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
  expect_equal(warped$y, tf_warp(f$y, warp))
  aligned <- tf_align(f, warp)
  expect_equal(aligned$x, tf_align(f$x, warp))
  expect_equal(aligned$y, tf_align(f$y, warp))
})

test_that("tf_register on tf_mv aligns all components with a shared warp", {
  t <- seq(0, 1, length.out = 101)
  shifts <- c(-0.08, -0.04, 0, 0.04, 0.08)
  bump <- function(shift) exp(-80 * (t - (0.5 + shift))^2)
  x <- tfd(t(sapply(shifts, bump)), arg = t)
  y <- tfd(t(sapply(shifts, \(shift) 0.5 * bump(shift))), arg = t)
  f <- tfd_mv(list(x = x, y = y))
  w <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "affine", type = "shift")
  ))
  aligned <- suppressWarnings(suppressMessages(tf_align(f, w)))
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
  expect_lt(meanvar(aligned), 0.1 * meanvar(f))
  expect_equal(aligned$x, suppressWarnings(tf_align(x, w)))
  expect_equal(aligned$y, suppressWarnings(tf_align(y, w)))
  expect_equal(tf_aligned(reg), aligned)
})

test_that("SRVF registration for tf_mv applies one shared warp to every component", {
  skip_if_not_installed("fdasrvf")

  t <- seq(0, 1, length.out = 101)
  centers <- c(0.4, 0.5, 0.6)
  bump <- function(center) exp(-80 * (t - center)^2)
  x <- tfd(t(sapply(centers, bump)), arg = t)
  y <- tfd(t(sapply(centers, \(center) 0.5 * bump(center))), arg = t)
  f <- tfd_mv(list(x = x, y = y))

  w <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "srvf")
  ))
  aligned <- suppressWarnings(suppressMessages(tf_align(f, w)))

  meanvar <- function(mv) {
    mean(sapply(tf_components(mv), function(comp) {
      suppressWarnings(mean(tf_evaluations(var(comp))[[1]], na.rm = TRUE))
    }))
  }

  expect_s3_class(w, "tfd")
  expect_s3_class(aligned, "tfd_mv")
  expect_lt(meanvar(aligned), 0.1 * meanvar(f))
  expect_equal(aligned$x, suppressWarnings(tf_align(x, w)))
  expect_equal(aligned$y, suppressWarnings(tf_align(y, w)))
})

test_that("tf_estimate_warps respects ref_component", {
  t <- seq(0, 1, length.out = 101)
  shifts <- c(-0.08, 0, 0.08)
  x <- tfd(t(sapply(shifts, \(s) exp(-80 * (t - (0.5 + s))^2))), arg = t)
  y <- tfd(t(sapply(shifts, \(s) exp(-80 * (t - (0.3 - s))^2))), arg = t)
  f <- tfd_mv(list(x = x, y = y))
  w_first <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "affine", type = "shift")
  ))
  w_y <- suppressWarnings(suppressMessages(
    tf_estimate_warps(f, method = "affine", type = "shift", ref_component = "y")
  ))
  expect_s3_class(w_first, "tfd")
  expect_length(w_first, 3)
  expect_length(w_y, 3)
  expect_equal(
    as.matrix(w_first),
    as.matrix(suppressWarnings(suppressMessages(
      tf_estimate_warps(x, method = "affine", type = "shift")
    )))
  )
  expect_equal(
    as.matrix(w_y),
    as.matrix(suppressWarnings(suppressMessages(
      tf_estimate_warps(y, method = "affine", type = "shift")
    )))
  )
  expect_false(isTRUE(all.equal(as.matrix(w_first), as.matrix(w_y))))
})

# ---- tf_arclength ------------------------------------------------------------

test_that("tf_arclength on the unit circle returns ~ 2*pi", {
  t <- seq(0, 1, length.out = 401)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  expect_equal(tf_arclength(circ), 2 * pi, tolerance = 1e-2)
})

test_that("tf_arclength is vectorised over curves (k-loop has length 2*pi*k)", {
  t <- seq(0, 1, length.out = 401)
  ks <- 1:3
  xs <- tfd(do.call(rbind, lapply(ks, \(k) cos(2 * pi * k * t))), arg = t)
  ys <- tfd(do.call(rbind, lapply(ks, \(k) sin(2 * pi * k * t))), arg = t)
  mv <- tfd_mv(list(x = xs, y = ys))
  ls <- tf_arclength(mv)
  expect_length(ls, 3L)
  expect_equal(ls, 2 * pi * ks, tolerance = 5e-2)
})

test_that("tf_arclength definite = FALSE returns cumulative s(t) as a tfd", {
  t <- seq(0, 1, length.out = 401)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  s <- tf_arclength(circ, definite = FALSE)
  expect_s3_class(s, "tfd")
  expect_length(s, 1L)
  # s(0) = 0, s(0.5) = pi, s(1) = 2*pi (uniform-speed parameterisation)
  vals <- unlist(tf_evaluate(s, arg = c(0, 0.5, 1)))
  expect_equal(vals, c(0, pi, 2 * pi), tolerance = 1e-2)
})

test_that("tf_arclength respects lower / upper limits", {
  t <- seq(0, 1, length.out = 401)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  expect_equal(
    tf_arclength(circ, lower = 0, upper = 0.25),
    pi / 2,
    tolerance = 1e-2
  )
  expect_equal(
    tf_arclength(circ, lower = 0.25, upper = 0.75),
    pi,
    tolerance = 1e-2
  )
  expect_equal(tf_arclength(circ, lower = 0.5, upper = 0.5), 0)
  expect_equal(
    tf_arclength(circ, lower = 0.5, upper = 0.5, method = "derive"),
    0
  )
  point <- tfd_mv(
    list(
      x = tfd(matrix(1, nrow = 1), arg = 0.5, domain = c(0, 1)),
      y = tfd(matrix(2, nrow = 1), arg = 0.5, domain = c(0, 1))
    ),
    domain = c(0, 1)
  )
  expect_equal(tf_arclength(point), 0)
})

test_that("tf_arclength rejects limits outside the domain (both methods)", {
  t <- seq(0, 1, length.out = 41)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  # a zero-width interval *outside* the domain must error, not silently give 0
  expect_error(tf_arclength(circ, lower = 2, upper = 2), "within the domain")
  expect_error(
    tf_arclength(circ, lower = 2, upper = 2, method = "derive"),
    "within the domain"
  )
  expect_error(tf_arclength(circ, lower = -1, upper = 0.5), "within the domain")
  expect_error(tf_arclength(circ, lower = 0.5, upper = 5), "within the domain")
})

test_that("tf_arclength(definite = FALSE) rejects a zero-width interval", {
  t <- seq(0, 1, length.out = 41)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  expect_error(
    tf_arclength(circ, lower = 0.5, upper = 0.5, definite = FALSE),
    "zero-width"
  )
  # a proper interval still returns a cumulative tfd
  expect_s3_class(
    tf_arclength(circ, lower = 0.25, upper = 0.75, definite = FALSE),
    "tfd"
  )
})

test_that("tf_arclength polyline is more accurate than derive on raw tfd", {
  t <- seq(0, 1, length.out = 401)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  err_poly <- abs(tf_arclength(circ, method = "polyline") - 2 * pi)
  err_der <- abs(tf_arclength(circ, method = "derive") - 2 * pi)
  expect_lt(err_poly, err_der)
  # both still in the right ballpark
  expect_equal(tf_arclength(circ, method = "derive"), 2 * pi, tolerance = 1e-2)
})

test_that("tf_arclength works for a 3-d helix", {
  # one full turn of a unit-radius helix climbing 2pi*c in z over t in [0, 1]:
  # f(t) = (cos(2*pi*t), sin(2*pi*t), 2*pi*c*t)
  # arc length = sqrt((2*pi)^2 + (2*pi*c)^2) = 2*pi*sqrt(1 + c^2)
  t <- seq(0, 1, length.out = 401)
  c0 <- 0.5
  hx <- tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t)
  hy <- tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  hz <- tfd(matrix(2 * pi * c0 * t, nrow = 1), arg = t)
  helix <- tfd_mv(list(x = hx, y = hy, z = hz))
  expect_equal(tf_arclength(helix), 2 * pi * sqrt(1 + c0^2), tolerance = 1e-2)
})

test_that("tf_arclength errors clearly when component domains only partially overlap", {
  x <- tfd(matrix(c(0, 1), nrow = 1), arg = c(0, 1))
  y <- tfd(matrix(c(0, 0.5, 1), nrow = 1), arg = c(0.5, 0.75, 1))
  f <- tfd_mv(list(x = x, y = y))

  expect_error(tf_arclength(f), "missing paired component")
  expect_equal(
    tf_arclength(f, lower = 0.5, upper = 1),
    sqrt(1.25),
    tolerance = 1e-8
  )
})
