set.seed(123)
x <- tf_rgp(4, arg = seq(0, 1, length.out = 51), nugget = 0.1)
xi <- tf_sparsify(tf_jiggle(x), 0.2)
xb <- tfb(x, verbose = FALSE) |> suppressMessages() |> suppressWarnings()
xbi <- tfb(xi, verbose = FALSE) |> suppressMessages() |> suppressWarnings()

xfpc <- tfb_fpc(x, verbose = FALSE)


test_that("tf_zoom for tfd works", {
  expect_equal(tf_domain(tf_zoom(x, 0.2, 0.8)), c(0.2, 0.8))
  expect_equal(tf_domain(tf_zoom(xi, 0.2, 0.8)), c(0.2, 0.8))
  expect_equal(
    as.matrix(tf_zoom(x, 0, 0.5)), as.matrix(x)[, 1:26],
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(tf_zoom(xi, 0, 0.5), unnest = TRUE),
    as.data.frame(xi, unnest = TRUE) |> subset(arg <= 0.5),
    ignore_attr = TRUE
  )

  expect_error(tf_zoom(x, c(0.8, 0.1)))
  expect_error(tf_zoom(x, 0.11, 0.111), "no data")
  expect_error(tf_zoom(xi, 0.051, 0.0511), "no data")

  expect_true(is_irreg(tf_zoom(x, 0.2, seq(0.3, 1, length.out = length(x)))))
})

test_that("tf_zoom for tfb_spline works", {
  expect_equal(tf_domain(tf_zoom(xb, 0.2, 0.8)), c(0.2, 0.8))
  expect_equal(tf_domain(tf_zoom(xbi, 0.2, 0.8)), c(0.2, 0.8))
  expect_equal(
    as.matrix(tf_zoom(xb, 0, 0.5)), as.matrix(xb)[, 1:26],
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(tf_zoom(xbi, 0, 0.5), unnest = TRUE),
    as.data.frame(xbi, unnest = TRUE) |> subset(arg <= 0.5),
    ignore_attr = TRUE
  )

  expect_error(tf_zoom(xb, c(0.8, 0.1)))
  expect_error(tf_zoom(xb, 0.11, 0.111), "no data")

  expect_message(
    out <- tf_zoom(xb, 0.2, seq(0.3, 1, length.out = length(x))),
    "converting to tfd"
  )
  expect_true(is_irreg(out))
})

test_that("tf_zoom for tfb_fpc works", {
  expect_warning(
    tf_zoom(xfpc, 0.2, 0.8), "orthonormality of FPC basis"
  )
  expect_equal(
    tf_domain(suppressWarnings(tf_zoom(xfpc, 0.2, 0.8))),
    c(0.2, 0.8)
  )
  expect_equal(
    suppressWarnings(as.matrix(tf_zoom(xfpc, 0, 0.5))), as.matrix(xfpc)[, 1:26],
    ignore_attr = TRUE
  )
  expect_equal(
    suppressWarnings(as.data.frame(tf_zoom(xfpc, 0, 0.5), unnest = TRUE)),
    as.data.frame(xfpc, unnest = TRUE) |> subset(arg <= 0.5),
    ignore_attr = TRUE
  )

  expect_error(suppressWarnings(tf_zoom(xfpc, 0.8, 0.1)))
  expect_error(suppressWarnings(tf_zoom(xfpc, 0.11, 0.111)), "no data")
  expect_true(suppressWarnings(
    is_irreg(tf_zoom(xfpc, 0.2, seq(0.3, 1, length.out = length(x))))
  ))
})
