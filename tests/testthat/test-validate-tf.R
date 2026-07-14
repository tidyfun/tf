# Tests for the internal validate_tf() invariant checker.

test_that("validate_tf accepts valid tfd_reg", {
  set.seed(1)
  x <- tf_rgp(3)
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts valid tfd_irreg", {
  set.seed(1)
  x <- tf_rgp(3) |> tf_sparsify(0.5)
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts valid tfb_spline", {
  set.seed(1)
  suppressMessages(x <- tfb(tf_rgp(5)))
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts valid tfb_fpc", {
  set.seed(1)
  suppressMessages(x <- tfb_fpc(tf_rgp(5)))
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts valid tfd_mv", {
  set.seed(1)
  x <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts valid tfb_mv", {
  set.seed(1)
  suppressMessages(x <- tfb_mv(list(x = tf_rgp(3), y = tf_rgp(3))))
  expect_true(validate_tf(x))
  expect_valid_tf(x)
})

test_that("validate_tf accepts length-0 prototypes of all subclasses", {
  set.seed(1)
  x_reg <- tf_rgp(3)
  x_irreg <- tf_sparsify(x_reg, 0.5)
  suppressMessages(x_spline <- tfb(x_reg))
  suppressMessages(x_fpc <- tfb_fpc(x_reg))
  x_tfd_mv <- tfd_mv(list(a = x_reg, b = x_irreg))
  suppressMessages(x_tfb_mv <- tfb_mv(list(a = x_reg, b = x_reg)))
  expect_valid_tf(vctrs::vec_ptype(x_reg))
  expect_valid_tf(vctrs::vec_ptype(x_irreg))
  expect_valid_tf(vctrs::vec_ptype(x_spline))
  expect_valid_tf(vctrs::vec_ptype(x_fpc))
  expect_valid_tf(vctrs::vec_ptype(x_tfd_mv))
  expect_valid_tf(vctrs::vec_ptype(x_tfb_mv))
  # constructor-made empties carry only minimal attributes (no basis etc.)
  expect_valid_tf(tfd(numeric(0)))
  expect_valid_tf(tfb(numeric(0)))
})

test_that("validate_tf rejects tfd_irreg with non-character evaluator_name", {
  set.seed(1)
  x <- tf_rgp(3) |> tf_sparsify(0.5)
  attr(x, "evaluator_name") <- 1L
  expect_error(validate_tf(x), "evaluator_name")
})

test_that("validate_tf rejects non-tf input", {
  expect_error(validate_tf(1:10), "not a .* object")
})

test_that("validate_tf rejects bad domain", {
  set.seed(1)
  x <- tf_rgp(3)
  attr(x, "domain") <- c(1, 1)  # not distinct
  expect_error(validate_tf(x), "domain")
})

test_that("validate_tf rejects bad domain (length != 2)", {
  set.seed(1)
  x <- tf_rgp(3)
  attr(x, "domain") <- c(0, 0.5, 1)
  expect_error(validate_tf(x), "domain")
})

test_that("validate_tf rejects tfd_reg with mismatched element length", {
  set.seed(1)
  x <- tf_rgp(3)
  # truncate one element to break the length invariant
  bad <- unclass(x)
  attrs <- attributes(x)
  bad[[1]] <- bad[[1]][-1]
  attributes(bad) <- attrs
  expect_error(validate_tf(bad), "length")
})

test_that("validate_tf rejects tfd_irreg with bad element shape", {
  set.seed(1)
  x <- tf_rgp(3) |> tf_sparsify(0.5)
  bad <- unclass(x)
  attrs <- attributes(x)
  bad[[1]] <- list(arg = bad[[1]]$arg, data = bad[[1]]$value)  # #234-style bug
  attributes(bad) <- attrs
  expect_error(validate_tf(bad), "arg.*value|value")
})

test_that("validate_tf rejects tfb_spline with wrong coef length", {
  set.seed(1)
  suppressMessages(x <- tfb(tf_rgp(5)))
  bad <- unclass(x)
  attrs <- attributes(x)
  bad[[1]] <- bad[[1]][-1]  # truncate coefficient vector
  attributes(bad) <- attrs
  expect_error(validate_tf(bad), "ncol\\(basis_matrix\\)|length")
})

test_that("validate_tf rejects tfb_spline with mismatched basis_matrix nrow", {
  set.seed(1)
  suppressMessages(x <- tfb(tf_rgp(5)))
  attr(x, "basis_matrix") <- attr(x, "basis_matrix")[-1, ]
  expect_error(validate_tf(x), "nrow\\(basis_matrix\\)|length\\(arg\\)")
})

test_that("validate_tf rejects tfb_fpc with wrong score_variance length", {
  set.seed(1)
  suppressMessages(x <- tfb_fpc(tf_rgp(5)))
  # score_variance must have length ncol(basis_matrix) - 1; truncate to break it
  attr(x, "score_variance") <- attr(x, "score_variance")[-1]
  expect_error(validate_tf(x), "score_variance")
})

test_that("validate_tf catches the #234 corruption pattern", {
  # Manually construct an irregular tfd whose elements have list(arg=, data=)
  # instead of the correct list(arg=, value=) -- i.e. the #234 bug shape.
  # The base branch still has the #234 bug, so we build the corruption by hand
  # rather than relying on tf_arg<-.
  set.seed(1)
  xi <- tf_sparsify(tf_rgp(2))
  raw <- unclass(xi)
  attrs <- attributes(xi)
  raw[[1]] <- list(arg = raw[[1]]$arg, data = raw[[1]]$value)
  bad <- raw
  attributes(bad) <- attrs
  expect_error(validate_tf(bad), "value|field|name")
})

test_that("validate_tf rejects tf_mv with mismatched payload/component length", {
  set.seed(1)
  x <- tfd_mv(list(a = tf_rgp(3), b = tf_rgp(3)))
  # corrupt: swap in a shorter component
  comps <- attr(x, "components")
  comps$a <- comps$a[1:2]
  attr(x, "components") <- comps
  expect_error(validate_tf(x), "payload length")
})

test_that("validate_tf rejects tf_mv with mismatched comp_names", {
  set.seed(1)
  x <- tfd_mv(list(a = tf_rgp(3), b = tf_rgp(3)))
  attr(x, "comp_names") <- c("a", "wrong")
  expect_error(validate_tf(x), "comp_names")
})
