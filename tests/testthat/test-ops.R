test_that("tfd arithmetic operations with numeric", {
  set.seed(1234)
  x <- tf_rgp(3)
  for (op in c("+", "-", "*")) {#, "/")) {
    expect_no_error(vec_arith(op, x, 2))
    expect_no_error(vec_arith(op, 2, x))
  }

  # tfd_reg arithmetic
  x <- tfd(1:10)
  expect_equal(x + 2, tfd(1:10 + 2), ignore_function_env = TRUE)
  expect_equal(x + 2, 2 + x)
  expect_equal(x - 2, tfd((1:10) - 2), ignore_function_env = TRUE)
  expect_equal(2 - x, tfd(2 - (1:10)),  ignore_function_env = TRUE)
  expect_equal(x * 2, tfd(1:10 * 2),  ignore_function_env = TRUE)
  expect_equal(2 * x, x * 2)
  expect_equal(2 / x, tfd(2 / (1:10)),  ignore_function_env = TRUE)
  expect_equal(x / 2, tfd((1:10) / 2),  ignore_function_env = TRUE)
  expect_class(x * 1:3, "tfd")
  expect_class(1:3 * x, "tfd")

  # size checks: no recycling!
  x <- tfd(1:10) * 1:3
  expect_error(x - 4:1)
  expect_error(1:2 * x)

  # irregular
  x_i <- tf_jiggle(x)
  expect_equal(tf_arg(x_i * 2), tf_arg(x_i))
  expect_equal(x_i + 2, 2 + x_i)
  expect_equal(2 * x_i, x_i * 2)
  expect_equal(tf_evaluations(x_i - 2), tf_evaluations(-2 + x))
  expect_equal(tf_evaluations(x_i / 2), tf_evaluations(x / 2))
})

test_that("tfd arithmetic operations with other tfd", {
  set.seed(1234)
  x <- tf_rgp(3)
  expect_equal(x + x, x * 2)
  expect_equal((x - 2) + 2, x)

  x_i <- tf_jiggle(x)
  expect_error(x + x_i)
  expect_equal(x_i + x_i, x_i * 2)
  expect_equal((x_i - 2) + 2, x_i)
  expect_equal(as.matrix(x_i^2) |> suppressWarnings(),
               as.matrix(x_i)^2 |> suppressWarnings())
  expect_equal(as.matrix(2^x_i) |> suppressWarnings(),
               2^as.matrix(x_i) |> suppressWarnings())
  expect_error(x_i + tf_jiggle(x_i))

  y <- tf_rgp(3)
  expect_equal(as.matrix(x + y) |> suppressWarnings(),
               (as.matrix(x) + as.matrix(y)) |> suppressWarnings())

  # no recycling
  expect_no_error(x + x[1])
  expect_error(x + tf_rgp(4))
})

test_that("tfb_spline arithmetic operations with numeric", {
  set.seed(1234)
  x <- (tfd(seq(1, 3, l = 31)) * 1:3) |> tfb(verbose = FALSE)

  # addition
  expect_warning(x + 2, "lossy cast")
  expect_warning(2 + x, "lossy cast")
  expect_equal(as.matrix(x + 2) |> suppressWarnings(),
               as.matrix(x) + 2)
  expect_equal(as.matrix(x + 2) |> suppressWarnings(),
               as.matrix(2 + x) |> suppressWarnings())
  expect_equal(as.matrix(x - 2) |> suppressWarnings(),
               as.matrix(x) - 2)
  expect_equal(as.matrix(x - 2) |> suppressWarnings(),
               as.matrix(-2 + x) |> suppressWarnings())

  # multiplication
  expect_equal(as.matrix(x * 2), as.matrix(x) * 2)
  expect_equal(x * 2, 2 * x)
  expect_equal(x / 2, 1 / 2 * x)

  # other ops
  expect_warning(x^2, "lossy cast")
  expect_equal(as.matrix(x^2) |> suppressWarnings(),
               as.matrix(x)^2, tolerance = .1)
  expect_warning(2^x, "lossy cast")
  expect_equal(as.matrix(x^2) |> suppressWarnings(),
               as.matrix(x)^2, tolerance = .1)

  # tfb with link-functions
  x_l <- tfb(tfd(x)^2, family = Gamma(link = "log"), verbose= FALSE, penalized = FALSE)
  expect_warning(2 * x_l)
  expect_equal(((((x_l * 2) / 2) + 2) - 2) |> suppressWarnings(),
                x_l |> suppressWarnings(),
               tolerance = 0.01, ignore_attr = TRUE)
  expect_equal((2 * x_l) |> suppressWarnings(),
               (x_l * 2) |> suppressWarnings())
  expect_equal((-.1 + x_l) |> suppressWarnings(),
               (x_l - .1) |> suppressWarnings())

  # no recycling
  expect_error(x * 1:2)

})

test_that("tfb arithmetic operations with other tfb", {
  set.seed(1234)
  x <- tf_rgp(3) |> tfb(verbose = FALSE, penalized = FALSE)
  expect_equal((x + x) |> suppressWarnings(),
                x * 2)
  expect_equal((x / x) |> suppressWarnings() |> suppressMessages(),
               x^0 |> suppressWarnings() |> suppressMessages(), ignore_attr = TRUE)
  expect_equal(x - x, 0 * x, ignore_attr = TRUE)

  y <- tf_rgp(3) |> tfb(verbose = FALSE, penalized = FALSE)
  expect_equal(x + y, y + x)

  # different bases/classes:
  x_p <- tfb_fpc(x)
  expect_warning(x + x_p, "2nd argument")
  expect_equal((x + x_p) |> suppressWarnings(), 2 * x, ignore_attr = TRUE)

  x2 <- tfb(tfd(x), k = 17, bs = "ps", verbose = FALSE, penalized = FALSE)
  expect_warning((x2[1] + x), "1st argument")
  expect_equal((x2[1] + x)[1] |> suppressWarnings(),
               (2 * x)[1], ignore_attr = TRUE, tolerance = 0.01)

  # recycling
  expect_error(x + x[1:2])
  expect_no_error(x + x[1])
  expect_no_error(x[3] + x)
})
