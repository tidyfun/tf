test_that("tfd_mv construction from a list of tf vectors works", {
  set.seed(1)
  fx <- tf_rgp(4)
  fy <- tf_rgp(4)
  f <- tfd_mv(list(x = fx, y = fy))
  expect_s3_class(f, "tfd_mv")
  expect_s3_class(f, "tf_mv")
  expect_s3_class(f, "tf")
  expect_length(f, 4)
  expect_identical(tf_ncomp(f), 2L)
  expect_identical(names(tf_components(f)), c("x", "y"))
  expect_equal(tf_component(f, "x"), fx, ignore_attr = TRUE)
  expect_equal(f$y, fy, ignore_attr = TRUE)
})

test_that("tfd_mv is not a univariate tfd", {
  f <- tfd_mv(list(a = tf_rgp(2), b = tf_rgp(2)))
  expect_true(is_tf(f))
  expect_true(is_tf_mv(f))
  expect_true(is_tfd_mv(f))
  expect_false(is_tfd(f))
  expect_false(is_tfb_mv(f))
})

test_that("tfd_mv construction from a list of matrices works", {
  arg <- seq(0, 1, length.out = 11)
  mx <- matrix(rnorm(33), nrow = 3)
  my <- matrix(rnorm(33), nrow = 3)
  f <- tfd_mv(list(x = mx, y = my), arg = arg)
  expect_s3_class(f, "tfd_mv")
  expect_length(f, 3)
  expect_equal(tf_arg(f), arg)
})

test_that("tfd_mv construction from a 3-d array works", {
  arr <- array(rnorm(3 * 11 * 2), dim = c(3, 11, 2),
               dimnames = list(NULL, NULL, c("x", "y")))
  f <- tfd_mv(arr, arg = seq(0, 1, length.out = 11))
  expect_length(f, 3)
  expect_identical(tf_ncomp(f), 2L)
  expect_identical(names(tf_components(f)), c("x", "y"))
})

test_that("tfd_mv construction from a long data.frame works", {
  df <- data.frame(
    id = rep(1:3, each = 5),
    t = rep(seq(0, 1, length.out = 5), 3),
    x = rnorm(15),
    y = rnorm(15)
  )
  f <- tfd_mv(df, id = "id", arg = "t", value = c("x", "y"))
  expect_length(f, 3)
  expect_identical(names(tf_components(f)), c("x", "y"))
})

test_that("tfd_mv supports regular and irregular components", {
  set.seed(3)
  reg <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_true(all(map_lgl(tf_components(reg), is_reg)))
  irr <- tfd_mv(list(x = tf_sparsify(tf_rgp(3)), y = tf_sparsify(tf_rgp(3))))
  expect_true(all(map_lgl(tf_components(irr), is_irreg)))
  # per-component args may differ -> tf_arg returns a list
  expect_type(tf_arg(irr), "list")
  expect_true(is.matrix(tf_count(irr)))
  expect_identical(dim(tf_count(irr)), c(3L, 2L))
})

test_that("tfd_mv accessors and replacement work", {
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_equal(tf_domain(f), c(0, 1))
  evs <- tf_evaluations(f)
  expect_length(evs, 3)
  expect_true(is.matrix(evs[[1]]))
  expect_identical(colnames(evs[[1]]), c("x", "y"))
  # replace a component
  f2 <- f
  f2$x <- f$x * 2
  expect_equal(tf_evaluations(f2$x)[[1]], 2 * tf_evaluations(f$x)[[1]])
  # add a new component by name
  f3 <- f
  tf_component(f3, "z") <- tf_rgp(3)
  expect_identical(tf_ncomp(f3), 3L)
})

test_that("tfd_mv handles NA curves (any component NA)", {
  set.seed(5)
  fx <- tf_rgp(3)
  fx[2] <- NA
  f <- tfd_mv(list(x = fx, y = tf_rgp(3)))
  expect_equal(unname(is.na(f)), c(FALSE, TRUE, FALSE))
})

test_that("tfd_mv length-0 prototype works", {
  f0 <- tfd_mv(list())
  expect_s3_class(f0, "tfd_mv")
  expect_length(f0, 0)
  expect_identical(tf_ncomp(f0), 0L)
})

test_that("tfd_mv errors on incompatible components", {
  expect_error(tfd_mv(list(x = tf_rgp(3), y = tf_rgp(4))), "same length")
  expect_error(
    tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2, arg = seq(0, 2, length.out = 5)))),
    "domain"
  )
})
