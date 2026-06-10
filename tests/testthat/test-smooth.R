test_that("tf_smooth works with lowess", {
  set.seed(1)
  x <- tf_rgp(3, arg = 51L)
  out <- suppressMessages(tf_smooth(x, method = "lowess"))
  expect_s3_class(out, "tfd")
  expect_equal(length(out), length(x))
  expect_equal(tf_arg(out), tf_arg(x))
})

test_that("tf_smooth works with savgol", {
  set.seed(1)
  x <- tf_rgp(3, arg = 51L)
  out <- suppressMessages(tf_smooth(x, method = "savgol"))
  expect_s3_class(out, "tfd")
  expect_equal(length(out), length(x))
  out2 <- suppressMessages(tf_smooth(x, method = "savgol", fl = 5))
  expect_s3_class(out2, "tfd")
})

test_that("tf_smooth works with rollmean", {
  set.seed(1)
  x <- tf_rgp(3, arg = 51L)
  out <- suppressMessages(tf_smooth(x, method = "rollmean"))
  expect_s3_class(out, "tfd")
  expect_equal(length(out), length(x))
})

test_that("tf_smooth works with rollmedian", {
  set.seed(1)
  x <- tf_rgp(3, arg = 51L)
  out <- suppressMessages(tf_smooth(x, method = "rollmedian", k = 5))
  expect_s3_class(out, "tfd")
  expect_equal(length(out), length(x))
})

test_that("tf_smooth.tfb returns input unchanged", {
  set.seed(1)
  x <- suppressMessages(tfb(tf_rgp(3, arg = 51L)))
  expect_identical(suppressMessages(tf_smooth(x)), x)
})
