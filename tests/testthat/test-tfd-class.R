test_that("tfd.numeric works", {
  # regular data
  set.seed(1234)
  x <- runif(100)
  f <- tfd(x)
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), list(1:100))
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # irregular data
  x[c(2, 4, 6)] <- NA
  f <- tfd(x)
  expect_s3_class(f, "tfd_irreg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), numeric())
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # empty data
  f <- tfd(numeric())
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 0)
  expect_identical(attr(f, "arg"), list(integer()))
  expect_identical(attr(f, "domain"), c(0, 0))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # single NA
  for (x in list(NA_real_, NA_integer_)) {
    f <- tfd(x)
    expect_s3_class(f, "tfd_reg")
    expect_length(f, 0)
    expect_identical(attr(f, "arg"), list(1L))
    expect_identical(attr(f, "domain"), c(0, 0))
    expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
    expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")
  }

  # evaluations must be inside the domain
  x <- 1:10
  expect_no_error(tfd(x, domain = c(1, 10)))
  expect_error(tfd(x, domain = c(2, 10)), "Evaluations must be inside the domain.")
  expect_error(tfd(x, domain = c(1, 9)), "Evaluations must be inside the domain.")
  expect_error(tfd(x, domain = c(2, 9)), "Evaluations must be inside the domain.")
})

test_that("tfd works consistently for partially missing data", {
  x <- tf_rgp(10)
  x_df <- x|> tf:::tf_2_df()
  x_df[x_df$id == "2", "value"] <- NA
  x_mat <- x|> as.matrix()
  x_mat[2, ] <- NA
  expect_warning(tfd(x_df), "NA")
  expect_class(tfd(x_df) |> suppressWarnings(), "tfd_reg")
  expect_equal(tfd(x_df) |> suppressWarnings(),
               tfd(x_mat)|> suppressWarnings())

  x <- tf_rgp(10) |> tf_sparsify(0.8)
  x_df <- x |> tf:::tf_2_df()
  x_df[x_df$id == "2", "value"] <- NA
  x_mat <- x |> as.matrix() |> suppressWarnings()
  x_mat[2, ] <- NA
  expect_warning(tfd(x_df), "NA")
  expect_class(tfd(x_df) |> suppressWarnings(), "tfd_irreg")
  expect_equal(tfd(x_df) |> suppressWarnings(),
               tfd(x_mat)|> suppressWarnings())
})
