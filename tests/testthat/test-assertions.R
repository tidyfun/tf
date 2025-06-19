test_that("assert_warp works", {
  expect_error(
    assert_warp(tfd(list(1:5, 1:5), arg = 1:5), tfd(1:5)),
    "`x` and `warp` must have the same length"
  )
  expect_error(
    assert_warp(tfd(1:5), tfd(1:10)),
    "`x` and `warp` must have the same domain"
  )
  expect_error(assert_warp(tf_rgp(10), tf_rgp(10)), "`warp` must be monotonic.")

  x <- tfd(1:3, arg = c(0, 0.5, 1))
  warp <- tfd(c(1, 0.5, 0), arg = c(0, 0.5, 1))
  expect_no_error(assert_warp(warp, x, strict = TRUE))
  warp <- tfd(c(0, 0.5, 2), arg = c(0, 0.5, 1))
  expect_no_error(assert_warp(warp, x, strict = FALSE))
  expect_error(
    assert_warp(warp, x, strict = TRUE),
    "`warp` domain and range must be the same."
  )
})
