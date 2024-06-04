test_that("tf_interpolate.tfb works", {
  x <- tf_rgp(4) |> tfb() |> suppressMessages()
  x2 <- tf_interpolate(x, arg = seq(0, 1, length.out = 11))
  expect_s3_class(x2, class = "tfb_spline")
  expect_identical(
    tf_interpolate(x2, seq(0, 1, length.out = 31)),
    tf_interpolate(x, arg = seq(0, 1, length.out = 31))
  )
  expect_error(
    tf_interpolate(x, arg = seq(-1, 1, length.out = 3)),
    "Assertion on 'arg' failed"
  )
})

test_that("tf_interpolate.tfb_fpc works", {
  x <- tf_rgp(4) |> tfb_fpc()
  expect_s3_class(tf_interpolate(x, arg = seq(0, 1, length.out = 11)), "tfb_fpc")
  expect_identical(
    tf_interpolate(x, arg = seq(0, 1, length.out = 11)) |>
      tf_interpolate(seq(0, 1, length.out = 31)),
    tf_interpolate(x, arg = seq(0, 1, length.out = 31))
  )
  expect_error(
    tf_interpolate(x, arg = seq(-1, 1, length.out = 3)),
    "Assertion on 'arg' failed"
  )
})
