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

test_that("savgol matches pracma::savgol numerically (regression)", {
  # Pin numerical equivalence between the inline savgol() and the upstream
  # implementation it replaces. Snapshot values computed against
  # pracma::savgol() at the time of the dependency removal.
  set.seed(20260610)
  x <- cumsum(rnorm(101))

  # Default (fl, forder = 4, dorder = 0)
  out_default <- savgol(x, fl = 11)
  expect_length(out_default, length(x))
  expect_equal(
    out_default[c(1, 25, 50, 75, 101)],
    c(
      -0.4616933452485163,
       4.9930783306980793,
       6.2228728562669682,
       4.4474764097120572,
       7.2186156201758047
    ),
    tolerance = 1e-10
  )

  # First derivative
  out_d1 <- savgol(x, fl = 11, forder = 3, dorder = 1)
  expect_equal(
    out_d1[c(1, 25, 50, 75, 101)],
    c(
      -0.0062313892718525,
       0.0091651046404730,
      -0.4741681396791447,
       0.2145733291250493,
      -2.4397772718621287
    ),
    tolerance = 1e-10
  )

  # Caching does not change the answer
  out_default_again <- savgol(x, fl = 11)
  expect_identical(out_default, out_default_again)
})

test_that("savgol input validation", {
  expect_error(savgol(1:10, fl = 4), "odd integer")
  expect_error(savgol(1:10, fl = 1), "odd integer")
})

test_that("tf_smooth(savgol) still works after dropping pracma", {
  set.seed(1)
  f <- tf_rgp(2, arg = 51L, nugget = 0.05)
  f_sg <- suppressMessages(tf_smooth(f, "savgol", verbose = FALSE))
  expect_s3_class(f_sg, "tfd")
  expect_length(f_sg, length(f))
})
