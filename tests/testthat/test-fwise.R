test_that("fwise summaries work for tfd_reg", {
  x <- tf_rgp(3, arg = seq(0, 5, length.out = 101))
  # use non-unit length to verify scaling
  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE)

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, -1, -1), ignore_attr = TRUE)
})

test_that("fwise summaries work for tfd_irreg", {
  set.seed(1221)
  x <- tf_rgp(3, arg = 201L) |>
    tf_jiggle() |>
    tfd(evaluator = tf_approx_fill_extend)

  x_clamp <- (x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))
  expect_equal(tf_fmin(x_clamp), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- (x - tf_fmean(x)) / tf_fsd(x)
  expect_equal(
    tf_fmean(x_std),
    c(0, 0, 0),
    ignore_attr = TRUE,
    tolerance = 0.01
  )
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
  expect_equal(tf_crosscor(x, -x), c(-1, -1, -1), ignore_attr = TRUE)
})

test_that("fwise summaries work for tfb_spline", {
  suppressMessages(x <- tf_rgp(3) |> tfb(k = 35))

  x_clamp <- ((x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))) |>
    suppressWarnings()
  expect_equal(
    tf_fmin(x_clamp),
    c(0, 0, 0),
    ignore_attr = TRUE,
    tolerance = 0.001
  )
  expect_equal(tf_fmax(x_clamp), c(1, 1, 1), ignore_attr = TRUE)

  x_std <- ((x - tf_fmean(x)) / tf_fsd(x)) |> suppressWarnings()
  expect_equal(tf_fmean(x_std), c(0, 0, 0), ignore_attr = TRUE)
  expect_equal(
    tf_fsd(x_std) |> suppressWarnings(),
    c(1, 1, 1),
    ignore_attr = TRUE
  )

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
})

test_that("fwise summaries work for tfb_fpc", {
  set.seed(1212)
  x <- (tf_rgp(20, arg = 501L) |> tfb_fpc(k = 10, pve = 0.999))[1:3]

  x_clamp <- ((x - tf_fmin(x)) / (tf_fmax(x) - tf_fmin(x))) |>
    suppressWarnings()
  expect_equal(
    tf_fmin(x_clamp),
    c(0, 0, 0),
    ignore_attr = TRUE,
    tolerance = 0.05
  ) #!! uh oh
  expect_equal(
    tf_fmax(x_clamp),
    c(1, 1, 1),
    ignore_attr = TRUE,
    tolerance = 0.05
  ) #!! uh oh

  x_std <- ((x - tf_fmean(x)) / tf_fsd(x)) |> suppressWarnings()
  expect_equal(
    tf_fmean(x_std),
    c(0, 0, 0),
    ignore_attr = TRUE,
    tolerance = 0.05
  ) #!! uh oh
  expect_equal(tf_fsd(x_std), c(1, 1, 1), ignore_attr = TRUE, tolerance = 0.05) #!! uh oh

  expect_equal(tf_crosscov(x, x), tf_fvar(x))
})

test_that("function-wise scalar summaries work componentwise for tf_mv", {
  x <- tfd(rbind(c(0, 1, 2), c(1, 2, 3)), arg = 0:2)
  y <- tfd(rbind(c(0, 2, 4), c(2, 4, 6)), arg = 0:2)
  fm <- tfd_mv(list(x = x, y = y))
  names(fm) <- c("a", "b")

  expected_mean <- cbind(x = tf_fmean(fm$x), y = tf_fmean(fm$y))
  expected_var <- cbind(x = tf_fvar(fm$x), y = tf_fvar(fm$y))
  expected_sd <- cbind(x = tf_fsd(fm$x), y = tf_fsd(fm$y))
  rownames(expected_var) <- names(fm)
  rownames(expected_sd) <- names(fm)

  expect_equal(tf_fmean(fm), expected_mean)
  expect_equal(tf_fvar(fm), expected_var)
  expect_equal(tf_fsd(fm), expected_sd)

  custom_arg <- seq(0, 2, length.out = 5)
  expect_equal(
    tf_fmean(fm, arg = custom_arg),
    cbind(
      x = tf_fmean(fm$x, arg = custom_arg),
      y = tf_fmean(fm$y, arg = custom_arg)
    )
  )
})

test_that("function-wise scalar summaries use per-component args for tf_mv", {
  x_arg <- seq(0, 1, length.out = 3)
  y_arg <- seq(0, 1, length.out = 5)
  x <- tfd(rbind(a = x_arg, b = x_arg^2), arg = x_arg)
  y <- tfd(rbind(a = sin(y_arg), b = cos(y_arg)), arg = y_arg)
  fm <- tfd_mv(list(x = x, y = y))

  expected <- cbind(x = tf_fmean(x), y = tf_fmean(y))
  expected_var <- cbind(x = tf_fvar(x), y = tf_fvar(y))
  expected_sd <- cbind(x = tf_fsd(x), y = tf_fsd(y))
  rownames(expected) <- names(fm)
  rownames(expected_var) <- names(fm)
  rownames(expected_sd) <- names(fm)

  expect_equal(tf_fmean(fm), expected)
  expect_equal(tf_fvar(fm), expected_var)
  expect_equal(tf_fsd(fm), expected_sd)
})

test_that("tf_fwise works componentwise for tf_mv", {
  x <- tfd(rbind(c(0, 1, 2), c(1, 2, 3)), arg = 0:2)
  y <- tfd(rbind(c(0, 2, 4), c(2, 4, 6)), arg = 0:2)
  fm <- tfd_mv(list(x = x, y = y))
  names(fm) <- c("a", "b")

  out <- tf_fwise(fm, \(df) max(df$value))

  expect_named(out, c("a", "b"))
  expect_named(out[[1]], c("x", "y"))
  expect_equal(
    out,
    list(
      a = list(
        x = tf_fwise(fm$x, \(df) max(df$value))[[1]],
        y = tf_fwise(fm$y, \(df) max(df$value))[[1]]
      ),
      b = list(
        x = tf_fwise(fm$x, \(df) max(df$value))[[2]],
        y = tf_fwise(fm$y, \(df) max(df$value))[[2]]
      )
    )
  )
})
