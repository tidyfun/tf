source(system.file("testdata", "make-test-data.R", package = "tf"))

test_that("tf_evaluate.tfd works for regular", {
  expect_identical(tf_evaluate(smoo),
                   tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = tf_arg(smoo)),
                   tf_evaluations(smoo))
  expect_identical(tf_evaluate(smoo, arg = 0.5),
                   tf_evaluate(smoo, 0.5))
})

test_that("tf_evaluate.tfd works for irregular", {
  expect_identical(tf_evaluate(irr),
                   tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = tf_arg(irr)),
                   tf_evaluations(irr))
  expect_identical(tf_evaluate(irr, arg = 0.5),
                   tf_evaluate(irr, 0.5))
})

smoo_tfb <- tfb(smoo, penalized = FALSE, verbose = FALSE)

test_that("tf_evaluate.tfb works", {
  expect_identical(tf_evaluate(smoo_tfb),
                   tf_evaluations(smoo_tfb))
  expect_equal(tf_evaluate(smoo_tfb, arg = tf_arg(smoo_tfb)),
                   tf_evaluations(smoo_tfb))
  expect_equal(tf_evaluate(smoo_tfb, arg = 0.5),
                   tf_evaluate(smoo_tfb, 0.5))
})


