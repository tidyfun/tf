test_that("bracket evaluation returns a [curve, arg, component] array", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  arr <- f[1:3, c(0.2, 0.5, 0.8)]
  expect_identical(dim(arr), c(3L, 3L, 2L))
  expect_identical(dimnames(arr)[[3]], c("x", "y"))
  # consistent with the univariate component bracket
  expect_equal(arr[, , "x"], unclass(f$x[1:3, c(0.2, 0.5, 0.8)]),
               ignore_attr = TRUE)
})

test_that("matrix-index extraction returns one row per (function, arg) pair", {
  set.seed(2)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  idx <- cbind(1:3, c(0, 0.5, 1))
  out <- f[idx]
  expect_true(is.matrix(out))
  expect_identical(dim(out), c(3L, 2L))
  expect_identical(colnames(out), c("x", "y"))
})

test_that("component= drops to the univariate result", {
  set.seed(3)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  m <- f[1:2, c(0.1, 0.9), component = "x"]
  expect_true(is.matrix(m))
  expect_identical(dim(m), c(2L, 2L))
  expect_equal(m, f$x[1:2, c(0.1, 0.9)], ignore_attr = TRUE)
})

test_that("matrix = FALSE returns per-curve data.frames", {
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  out <- f[1:2, c(0.2, 0.7), matrix = FALSE]
  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out[[1]], c("arg", "x", "y"))
})

test_that("arithmetic is component-wise", {
  set.seed(5)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  s <- f + f
  expect_s3_class(s, "tfd_mv")
  expect_equal(tf_evaluations(s$x)[[1]], 2 * tf_evaluations(f$x)[[1]])
  d <- f - f
  expect_true(all(abs(unlist(tf_evaluations(d))) < 1e-9))
  scaled <- 3 * f
  expect_equal(tf_evaluations(scaled$y)[[2]], 3 * tf_evaluations(f$y)[[2]])
})

test_that("Math and Summary group generics are component-wise", {
  set.seed(6)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  el <- exp(log(abs(f) + 1)) - 1
  expect_s3_class(el, "tfd_mv")
  expect_equal(
    tf_evaluations(el$x)[[1]], abs(tf_evaluations(f$x)[[1]]),
    tolerance = 1e-6
  )
})

test_that("mean / median return a length-1 tf_mv", {
  set.seed(7)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  m <- mean(f)
  expect_s3_class(m, "tfd_mv")
  expect_length(m, 1)
  expect_equal(tf_evaluations(m$x)[[1]],
               tf_evaluations(mean(f$x))[[1]])
  expect_length(median(f), 1)
})

test_that("equality is component-wise", {
  set.seed(8)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_true(all(f == f))
  expect_false(any(f != f))
})

test_that("as.matrix returns a [curve, arg, component] array", {
  set.seed(9)
  f <- tfd_mv(list(x = tf_rgp(3, arg = 11L), y = tf_rgp(3, arg = 11L)))
  m <- as.matrix(f)
  expect_identical(dim(m), c(3L, 11L, 2L))
  expect_identical(dimnames(m)[[3]], c("x", "y"))
})
