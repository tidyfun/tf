test_that("tf_interpolate.tfb works", {
  x <- tf_rgp(4) |> tfb() |> suppressMessages()
  x2 <- tf_interpolate(x, arg = seq(0,1,l = 11))
  expect_s3_class(
      x2,
      class = "tfb_spline")
  expect_identical(
    x2 |> tf_interpolate(seq(0,1,l = 31)),
    tf_interpolate(x, arg = seq(0, 1, l = 31)))
  expect_error(
    tf_interpolate(x, arg = seq(-1, 1, l = 3)),
    "Assertion on 'arg' failed")
})

test_that("tf_interpolate.tfb_fpc works", {
  x <- tf_rgp(4) |> tfb_fpc()
  expect_s3_class(tf_interpolate(x, arg = seq(0,1,l = 11)), "tfb_fpc")
  expect_identical(
    tf_interpolate(x, arg = seq(0, 1, l = 11)) |>
      tf_interpolate(seq(0,1,l = 31)),
    tf_interpolate(x, arg = seq(0, 1, l = 31)))
  expect_error(tf_interpolate(x, arg = seq(-1, 1, l = 3)),
               "Assertion on 'arg' failed")
})
