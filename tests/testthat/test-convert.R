test_that("as.data.frame.tf works", {
  # unnest = FALSE
  set.seed(1312)
  x <- tf_rgp(3)
  expected <- data.frame(x = 1:3)
  expected$x <- x
  df <- as.data.frame(x)
  expect_data_frame(df)
  expect_identical(df, expected)
  expect_identical(as.data.frame(df), expected)

  x <- tf_sparsify(x)
  expected <- data.frame(x = 1:3)
  expected$x <- x
  df <- as.data.frame(x)
  expect_identical(df, expected)
  expect_identical(as.data.frame(df), expected)

  # unnest = TRUE
  set.seed(1312)
  x <- tf_rgp(3)
  df <- as.data.frame(x, unnest = TRUE)
  expect_data_frame(df,
    types = c("factor", "numeric", "numeric"),
    nrows = 153, ncols = 3, row.names = "named"
  )
  expect_named(df, c("id", "arg", "value"))
  expect_identical(df$id, as.factor(rep(1:3, each = 51)))
  expect_identical(df$arg, rep(seq(0, 1, by = 0.02), 3))
  expect_identical(df$value, unname(unlist(tf_evaluations(x))))
  expect_identical(as.data.frame(df), df)

  x <- tf_sparsify(x)
  df <- as.data.frame(x, unnest = TRUE)
  expect_data_frame(df,
    types = c("factor", "numeric", "numeric"),
    nrow = 74, ncol = 3, row.names = "named"
  )
  expect_named(df, c("id", "arg", "value"))
  expect_factor(df$id)
  expect_identical(df$arg, unname(unlist(tf_arg(x))))
  expect_identical(df$value, unname(unlist(tf_evaluations(x))))
  expect_identical(as.data.frame(df), df)
})

test_that("as.matrix.tf works", {
  # missing arg
  set.seed(1312)
  x <- tf_rgp(3)
  mat <- as.matrix(x)
  expect_matrix(mat, mode = "numeric", nrows = 3, ncols = 51)
  expect_identical(row.names(mat), as.character(1:3))
  expect_identical(colnames(mat), as.character(tf_arg(x)))
  expect_identical(attr(mat, "arg"), tf_arg(x))

  # arg provided
  arg <- seq(0, 1, by = 0.04)
  mat <- as.matrix(x, arg = arg)
  expect_matrix(mat, mode = "numeric", nrows = 3, ncols = 26)
  expect_identical(row.names(mat), as.character(1:3))
  expect_identical(colnames(mat), as.character(arg))
  expect_identical(attr(mat, "arg"), arg)
})

test_that("as.function.tf works", {
  set.seed(1312)
  x <- tf_rgp(3)
  fn <- as.function(x)
  expect_function(fn, args = "arg")
})
