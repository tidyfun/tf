test_that("sparkline formatting is quiet for existing NA entries", {
  arg <- seq(0, 1, length.out = 51)
  x <- suppressWarnings(
    tfd(
      rbind(
        sin(2 * pi * arg),
        rep(NA_real_, length(arg)),
        cos(2 * pi * arg)
      ),
      arg = arg
    )
  )

  expect_true(any(is.na(x)))
  expect_no_warning(tf:::spark_rep_tf(x, bins = 8))
  expect_no_warning(format(x, bins = 8))
  expect_no_warning(capture.output(print(x)))
})
