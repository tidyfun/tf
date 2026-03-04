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
