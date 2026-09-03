source(system.file("testdata", "make-test-data.R", package = "tf"))

test_that("tf_evaluate.tfd works for regular", {
  expect_identical(tf_evaluate(smoo), tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = tf_arg(smoo)), tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = 0.5), tf_evaluate(smoo, 0.5))
})

test_that("tf_evaluate.tfd works for irregular", {
  expect_identical(tf_evaluate(irr), tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = tf_arg(irr)), tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = 0.5), tf_evaluate(irr, 0.5))
})

test_that("tf_evaluate.tfb works", {
  smoo_tfb <- suppressMessages({
    capture.output(smoo_tfb <- tfb(smoo, penalized = FALSE, verbose = FALSE))
    smoo_tfb
  })
  expect_identical(tf_evaluate(smoo_tfb), tf_evaluations(smoo_tfb))
  expect_equal(
    tf_evaluate(smoo_tfb, arg = tf_arg(smoo_tfb)),
    tf_evaluations(smoo_tfb)
  )
  expect_equal(tf_evaluate(smoo_tfb, arg = 0.5), tf_evaluate(smoo_tfb, 0.5))
})

test_that("tf_evaluate.tfb keeps NA entries for shared arg", {
  t <- seq(0, 1, length.out = 51)
  mixed <- tfd(
    list(
      1 + abs(sin(2 * pi * t)),
      -1 - abs(cos(2 * pi * t)),
      1 + abs(sin(4 * pi * t))
    ),
    arg = t
  )
  b <- suppressWarnings(suppressMessages({
    capture.output(b <- tfb(mixed, k = 7, verbose = FALSE))
    b
  }))
  b_na <- suppressWarnings(log(b))

  eval_point <- tf_evaluate(b_na, arg = 0.5)
  expect_length(eval_point, length(b_na))
  expect_true(is.na(eval_point[[2]]))

  eval_grid <- tf_evaluate(b_na, arg = seq(0, 1, length.out = 5))
  expect_length(eval_grid, length(b_na))
  expect_equal(eval_grid[[2]], rep(NA_real_, 5))
})

test_that("tf_evaluate.tfd returns values at correct positions for duplicated args (#236)", {
  arg <- seq(0, 1, length.out = 11)
  x <- tfd(matrix(arg, nrow = 1), arg = arg)
  # duplicated unseen args must all be placed correctly
  expect_equal(
    tf_evaluate(x, c(0.25, 0.25, 0.35))[[1]],
    c(0.25, 0.25, 0.35)
  )
  # duplicated values, all unseen, more than one duplicate
  expect_equal(
    tf_evaluate(x, c(0.25, 0.25, 0.35, 0.35))[[1]],
    c(0.25, 0.25, 0.35, 0.35)
  )
  # `[` operator path reaches the same code
  expect_equal(as.numeric(x[1, c(0.25, 0.25, 0.35)]), c(0.25, 0.25, 0.35))
})

test_that("tf_evaluate.tfb works for a single off-grid arg value (#302)", {
  set.seed(1)
  x <- tf_rgp(3, arg = seq(0, 1, length.out = 21))
  fs <- tfb_spline(x, k = 8, verbose = FALSE)
  fp <- tfb_fpc(x)
  for (f in list(fs, fp)) {
    single <- tf_evaluate(f, arg = 0.123)
    expect_length(single, 3)
    expect_true(all(lengths(single) == 1))
    pair <- tf_evaluate(f, arg = c(0.123, 0.456))
    expect_equal(unlist(single), vapply(pair, `[`, numeric(1), 1))
    expect_equal(as.numeric(f[1, 0.123]), single[[1]])
    expect_equal(f[cbind(1:3, 0.123)], unname(unlist(single)))
  }
})

test_that("tf_evaluate.tfb with per-curve arg lists matches shared-arg results", {
  set.seed(2)
  x <- tf_rgp(4, arg = seq(0, 1, length.out = 21))
  f <- tfb_spline(x, k = 8, verbose = FALSE)
  f[2] <- NA
  args <- list(c(0.05, 0.5), 0.33, c(0.1, 0.9), c(0.05, 0.95))
  per_curve <- tf_evaluate(f, arg = args)
  expect_true(is.na(per_curve[[2]]))
  for (i in c(1, 3, 4)) {
    expect_equal(per_curve[[i]], tf_evaluate(f[i], arg = args[[i]])[[1]])
  }
})
