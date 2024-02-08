test_that("user defined covariance works", {
  my_sqexp <- function(s, t) exp(-(s - t)^2 / 0.1)
  set.seed(1312)
  x1 <- tf_rgp(1, cov = my_sqexp)
  set.seed(1312)
  x2 <- tf_rgp(1, cov = "squareexp")
  expect_equal(x1, x2)

  expect_error(
    tf_rgp(1, cov = function(s, t, x) (s - t) / x),
    "2 formal arguments"
  )
})
