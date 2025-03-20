test_that("fourier basis matches fda::create.fourier.basis for same specs", {
  skip_if_not_installed("fda")

  # define test range
  rng <- c(0, 10)
  nbasis <- 5 # odd -> okay
  period <- 8
  xvals <- seq(rng[1], rng[2], length.out = 50)

  # fda basis
  fda_fourier <- fda::create.fourier.basis(
    rangeval = rng,
    nbasis = nbasis,
    period = period
  )
  fda_mat <- fda::eval.basis(xvals, fda_fourier)

  # mgcv spec
  fourier_spec <- mgcv::s(
    x,
    bs = "fourier",
    k = nbasis,
    xt = list(period = period, rangeval = rng)
  )
  dat <- data.frame(x = xvals)

  # manually construct basis
  sm <- mgcv::smoothCon(fourier_spec, data = dat, knots = NULL)[[1]]
  mgcv_mat <- mgcv::PredictMat(sm, dat)

  expect_equal(mgcv_mat, fda_mat, tolerance = 1e-7, ignore_attr = TRUE)
})

test_that("fourier basis useable in tfb", {
  set.seed(1221)
  # generate cyclic functions:
  x <- tf_rgp(15, arg = 201L, cov = "brown", scale = 0.01, nugget = 0)
  x_sp <- tf_sparsify(x)

  expect_class(
    tfb(x, bs = "fourier", verbose = FALSE) |> suppressMessages(),
    "tfb_spline"
  )
  expect_class(
    tfb(x_sp, bs = "fourier", verbose = FALSE) |> suppressMessages(),
    "tfb_spline"
  )

  # check penalization:
  x_u <- tfb(x, bs = "fourier", penalized = FALSE, k = 25, verbose = FALSE) |>
    suppressMessages() |>
    as.matrix()
  x_p <- tfb(x, bs = "fourier", sp = .05, k = 25, verbose = FALSE) |>
    suppressWarnings() |>
    suppressMessages() |>
    as.matrix()
  x_c <- tfb(x, bs = "fourier", sp = 1e10, k = 25, verbose = FALSE) |>
    suppressWarnings() |>
    suppressMessages() |>
    as.matrix()
  x_mat <- as.matrix(x)
  u_error <- sum(abs(x_mat - x_u))
  p_error <- sum(abs(x_mat - x_p))
  c_error <- sum(abs(x_mat - x_c))
  expect_true(u_error < p_error)
  expect_true(p_error < c_error)
})
