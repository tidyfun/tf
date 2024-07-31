test_that("tfd.numeric works", {
  # regular data
  set.seed(1234)
  x <- runif(100)
  f <- tfd(x)
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), list(1:100))
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_type(attr(f, "evaluator"), "closure")
  expect_named(formals(attr(f, "evaluator")), c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # irregular data
  x[c(2, 4, 6)] <- NA
  f <- tfd(x)
  expect_s3_class(f, "tfd_irreg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), numeric())
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_type(attr(f, "evaluator"), "closure")
  expect_named(formals(attr(f, "evaluator")), c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # empty data
  f <- tfd(numeric())
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 0)
  expect_identical(attr(f, "arg"), list(integer()))
  expect_identical(attr(f, "domain"), c(0, 0))
  expect_type(attr(f, "evaluator"), "closure")
  expect_named(formals(attr(f, "evaluator")), c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # single NA
  for (x in list(NA_real_, NA_integer_)) {
    f <- tfd(x)
    expect_s3_class(f, "tfd_irreg")
    expect_length(f, 0)
    expect_identical(attr(f, "arg"), list(1L))
    expect_identical(attr(f, "domain"), c(0, 0))
    expect_type(attr(f, "evaluator"), "closure")
    expect_named(formals(attr(f, "evaluator")), c("x", "arg", "evaluations"))
    expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")
  }
})
