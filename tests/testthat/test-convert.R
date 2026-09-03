test_that("as.data.frame.tf works", {
  # unnest = FALSE
  set.seed(1312)
  x <- tf_rgp(3)
  expected <- data_frame(x = 1:3)
  expected$x <- x
  df <- as.data.frame(x)
  expect_data_frame(df)
  expect_identical(df, expected)
  expect_identical(as.data.frame(df), expected)

  x <- tf_sparsify(x)
  expected <- data_frame(x = 1:3)
  expected$x <- x
  df <- as.data.frame(x)
  expect_identical(df, expected)
  expect_identical(as.data.frame(df), expected)

  # unnest = TRUE
  set.seed(1312)
  x <- tf_rgp(3)
  df <- as.data.frame(x, unnest = TRUE)
  expect_data_frame(
    df,
    types = c("factor", "numeric", "numeric"),
    nrows = 153,
    ncols = 3,
    row.names = "named"
  )
  expect_named(df, c("id", "arg", "value"))
  expect_identical(df$id, as.factor(rep(1:3, each = 51)))
  expect_identical(df$arg, rep(seq(0, 1, by = 0.02), 3))
  expect_identical(df$value, unlist(tf_evaluations(x), use.names = FALSE))
  expect_identical(as.data.frame(df), df)

  x <- tf_sparsify(x)
  df <- as.data.frame(x, unnest = TRUE)
  expect_data_frame(
    df,
    types = c("factor", "numeric", "numeric"),
    nrow = 74,
    ncol = 3,
    row.names = "named"
  )
  expect_named(df, c("id", "arg", "value"))
  expect_factor(df$id)
  expect_identical(df$arg, unlist(tf_arg(x), use.names = FALSE))
  expect_identical(df$value, unlist(tf_evaluations(x), use.names = FALSE))
  expect_identical(as.data.frame(df), df)
})

test_that("as.matrix.tf works", {
  set.seed(1312)
  x <- tf_rgp(3)

  # missing arg regular
  mat <- as.matrix(x)
  expect_matrix(mat, mode = "numeric", nrows = 3, ncols = 51)
  expect_identical(row.names(mat), as.character(1:3))
  expect_identical(colnames(mat), as.character(tf_arg(x)))
  expect_identical(attr(mat, "arg"), tf_arg(x))

  # missing arg irregular
  x_irreg <- tf_sparsify(x)
  expect_warning(
    mat <- as.matrix(x_irreg),
    "`interpolate = FALSE` and no values present for some `j`"
  )
  expect_matrix(mat, mode = "numeric", nrows = 3, ncols = 49)
  expect_identical(row.names(mat), as.character(1:3))
  arg <- tf_arg(x_irreg) |> sort_unique(simplify = TRUE)
  expect_identical(
    colnames(mat),
    as.character(arg)
  )
  expect_identical(attr(mat, "arg"), arg)

  # interpolate = TRUE
  expect_no_warning(mat <- as.matrix(x_irreg, interpolate = TRUE))
  expect_matrix(mat, mode = "numeric", nrows = 3, ncols = 49)
  expect_identical(row.names(mat), as.character(1:3))
  arg <- tf_arg(x_irreg) |> sort_unique(simplify = TRUE)
  expect_identical(
    colnames(mat),
    as.character(arg)
  )
  expect_identical(attr(mat, "arg"), arg)

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

test_that("as.data.frame.tf_mv honours interpolate = FALSE on the union grid (#303)", {
  mv <- suppressWarnings(tfd_mv(list(
    x = tfd(matrix(1:22, nrow = 2), arg = seq(0, 1, length.out = 11)),
    y = tfd(matrix(1:14, nrow = 2), arg = seq(0, 1, length.out = 7))
  )))
  wide <- suppressWarnings(
    as.data.frame(mv, unnest = TRUE, long = FALSE, interpolate = FALSE)
  )
  mat <- suppressWarnings(as.matrix(mv, interpolate = FALSE))
  first <- wide[wide$id == "1", ]
  expect_equal(first$x, unname(mat[1, , "x"]))
  expect_equal(first$y, unname(mat[1, , "y"]))
  expect_true(anyNA(first$x) && anyNA(first$y))
  # default interpolation is unchanged
  expect_false(anyNA(as.data.frame(mv, unnest = TRUE, long = FALSE)$y))
  # explicit arg with interpolate = FALSE: only observed values survive
  at <- suppressWarnings(as.data.frame(
    mv,
    unnest = TRUE,
    long = FALSE,
    arg = c(0, 0.1),
    interpolate = FALSE
  ))
  expect_equal(at$x[at$id == "1"], c(1, 3))
  expect_equal(at$y[at$id == "1"], c(1, NA))
})

test_that("as.data.frame.tf_mv forwards ... (evaluator) on the union grid", {
  mv <- tfd_mv(list(
    x = tfd(matrix(1:6, nrow = 2), arg = c(0, 0.5, 1)),
    y = tfd(matrix(1:6, nrow = 2), arg = c(0, 0.5, 1))
  ))
  linear <- as.data.frame(mv, unnest = TRUE, long = FALSE, arg = 0.25)
  none <- as.data.frame(
    mv,
    unnest = TRUE,
    long = FALSE,
    arg = 0.25,
    evaluator = tf_approx_none
  )
  expect_equal(linear$x, c(2, 3))
  expect_true(all(is.na(none$x)))
})
