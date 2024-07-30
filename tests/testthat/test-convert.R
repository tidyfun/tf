test_that("as.data.frame works", {
  # unnest = FALSE
  set.seed(1312)
  x <- tf_rgp(3)
  expected <- data.frame(x = 1:3)
  expected$x <- x
  df <- as.data.frame(x)
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
  expect_named(df, c("id", "arg", "value"))
  expect_identical(dim(df), c(153L, 3L))
  expect_identical(df$id, as.factor(rep(1:3, each = 51)))
  expect_identical(df$arg, rep(seq(0, 1, by = 0.02), 3))
  expect_identical(df$value, unname(unlist(tf_evaluations(x))))
  expect_false(is.null(row.names(df)))
  expect_identical(as.data.frame(df), df)

  x <- tf_sparsify(x)
  df <- as.data.frame(x, unnest = TRUE)
  expect_named(df, c("id", "arg", "value"))
  expect_identical(dim(df), c(74L, 3L))
  expect_s3_class(df$id, "factor")
  expect_identical(df$arg, unname(unlist(tf_arg(x))))
  expect_identical(df$value, unname(unlist(tf_evaluations(x))))
  expect_false(is.null(row.names(df)))
  expect_identical(as.data.frame(df), df)
})
