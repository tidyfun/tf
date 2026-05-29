test_that("tfb_mv fits a basis per component", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  tb <- tfb_mv(f, k = 8, verbose = FALSE)
  expect_s3_class(tb, "tfb_mv")
  expect_true(is_tfb_mv(tb))
  expect_false(is_tfb(tb)) # not a univariate tfb
  expect_length(tb, 4)
  expect_identical(tf_ncomp(tb), 2L)
  expect_true(all(map_lgl(tf_components(tb), is_tfb)))
  expect_equal(tb$x, tfb(f$x, k = 8, verbose = FALSE))
  expect_equal(tb$y, tfb(f$y, k = 8, verbose = FALSE))
})

test_that("tfb_mv round-trips tfd_mv -> tfb_mv -> tfd_mv approximately", {
  set.seed(2)
  arg <- seq(0, 1, length.out = 101)
  f <- tfd_mv(list(x = tf_rgp(3, arg = arg), y = tf_rgp(3, arg = arg)))
  tb <- tfb_mv(f, k = 25, verbose = FALSE)
  back <- as.tfd_mv(tb)
  expect_s3_class(back, "tfd_mv")
  diff_x <- max(abs(
    unlist(tf_evaluations(f$x)) - unlist(tf_evaluations(back$x))
  ))
  diff_y <- max(abs(
    unlist(tf_evaluations(f$y)) - unlist(tf_evaluations(back$y))
  ))
  expect_lt(diff_x, 0.1)
  expect_lt(diff_y, 0.1)
})

test_that("tfb_mv supports fpc basis", {
  set.seed(3)
  f <- tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10)))
  tb <- tfb_mv(f, basis = "fpc", verbose = FALSE)
  expect_s3_class(tb, "tfb_mv")
  expect_true(all(map_lgl(tf_components(tb), is_tfb_fpc)))
  expect_equal(tb$x, tfb(f$x, basis = "fpc", verbose = FALSE))
  expect_equal(tb$y, tfb(f$y, basis = "fpc", verbose = FALSE))
})

test_that("per-component basis is reachable via tf_components()", {
  set.seed(4)
  tb <- tfb_mv(tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3))), verbose = FALSE)
  b <- map(tf_components(tb), tf_basis)
  expect_length(b, 2)
  expect_true(all(map_lgl(b, is.function)))
  expect_equal(b$x(tf_arg(tb$x)), tf_basis(tb$x)(tf_arg(tb$x)))
  expect_equal(b$y(tf_arg(tb$y)), tf_basis(tb$y)(tf_arg(tb$y)))
})

test_that("tf_count aborts with a clear message on basis-represented data", {
  set.seed(5)
  tb <- tfb_mv(tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3))), verbose = FALSE)
  expect_error(tf_count(tb), "not defined for basis-represented")
})
