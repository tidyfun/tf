test_that("matrix indexing extracts (function, arg) pairs", {
  set.seed(42)
  x <- tf_rgp(5, arg = 1:5)
  x_mat <- as.matrix(x)

  # diagonal extraction matches as.matrix result
  idx <- cbind(1:5, 1:5)
  result <- x[idx]
  expect_equal(result, unname(x_mat[idx]))

  # single pair
  result <- x[cbind(1, 3)]
  expect_equal(result, unname(x_mat[cbind(1, 3)]))

  # arbitrary pairs
  idx <- cbind(c(1, 3, 5), c(2, 4, 1))
  result <- x[idx]
  expect_equal(result, unname(x_mat[idx]))
})

test_that("matrix indexing works with non-integer arg values", {
  set.seed(42)
  x <- tf_rgp(3, arg = seq(0, 1, length.out = 11))

  # interpolated arg values
  idx <- cbind(1:3, c(0.15, 0.55, 0.85))
  result <- x[idx]
  expect_length(result, 3)
  expect_type(result, "double")

  # should match individual evaluations
  expect_equal(result[1], x[1, 0.15][1])
  expect_equal(result[2], x[2, 0.55][1])
  expect_equal(result[3], x[3, 0.85][1])
})

test_that("matrix indexing errors for invalid inputs", {
  set.seed(42)
  x <- tf_rgp(3, arg = 1:5)

  # wrong number of columns
  expect_error(x[cbind(1:3, 1:3, 1:3)])

  # out-of-bounds row index
  expect_error(x[cbind(10, 1)])

  # j must not be given when i is a matrix
  expect_error(x[cbind(1:3, 1:3), 1:5])
})

test_that("missing j with explicit matrix= evaluates on default grid", {
  set.seed(42)
  x <- tf_rgp(5, arg = 1:5)

  # matrix = FALSE defaults j to tf_arg(x), returns list of data.frames
  result <- x[1:3, , matrix = FALSE]
  expected <- x[1:3, tf_arg(x), matrix = FALSE]
  expect_identical(result, expected)

  # matrix = TRUE defaults j to tf_arg(x), returns matrix
  result <- x[1:3, , matrix = TRUE]
  expected <- x[1:3, tf_arg(x), matrix = TRUE]
  expect_identical(result, expected)
})

test_that("existing bracket behavior is preserved", {
  set.seed(42)
  x <- tf_rgp(5, arg = 1:5)

  # x[i] still returns a tf subset
  sub <- x[1:3]
  expect_s3_class(sub, "tfd")
  expect_length(sub, 3)

  # x[i, j] still returns a matrix by default
  result <- x[1:3, c(1, 3, 5)]
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))

  # x[i, j, matrix = FALSE] still returns list of data.frames
  result <- x[1:3, c(1, 3, 5), matrix = FALSE]
  expect_true(is.list(result))
  expect_length(result, 3)
  expect_s3_class(result[[1]], "data.frame")
})
