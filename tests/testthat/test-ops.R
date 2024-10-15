test_that("tfd arithmetic operations with numeric", {
  set.seed(1234)
  x <- tf_rgp(3)
  for (op in c("+", "-", "*")) {#, "/")) {
    expect_no_error(vec_arith(op, x, 2))
    expect_no_error(vec_arith(op, 2, x))
  }

  x <- tfd(1:10)
  expect_equal(x + 2, tfd(3:12))
  expect_equal(tfd(3:12), x + 2)
  expect_equal(x * 2, tfd(seq(2, 20, by = 2)))
  expect_equal(tfd(seq(2, 20, by = 2)), x * 2)
})

test_that("tfd arithmetic operations with other tfd", {
  set.seed(1234)
  x <- tf_rgp(3)
  expect_equal(x + x, x * 2)
  expect_equal((x - 2) + 2, x)

  y <- tf_rgp(3)
  expect_no_error(x + y)
  # tfd vectors have to be same length
  y <- tf_rgp(5)
  expect_error(x + y)
})

test_that("tfb arithmetic operations with numeric", {
  set.seed(1234)
  x <- tf_rgp(3) |> tfb()
  for (op in c("+", "-", "*")) { #, "/")) {
    expect_no_error(vec_arith(op, x, 2))
    expect_no_error(vec_arith(op, 2, x))
  }
  # no recycling
  expect_error(x + 1:5)
})

test_that("tfb arithmetic operations with other tfb", {
  set.seed(1234)
  x <- tf_rgp(3) |> tfb()
  expect_equal(x + x, x * 2, ignore_attr = TRUE)
  expect_equal((x - 2) + 2, x, ignore_attr = TRUE)

  y <- tf_rgp(3) |> tfb()
  expect_no_error(y + x)
  # tfb vectors have to be same length
  y <- tf_rgp(5) |> tfb()
  expect_error(x + y)
})
