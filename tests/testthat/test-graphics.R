test_that("plot.tf with points = TRUE does not warn (#248)", {
  x <- tf_rgp(3)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(plot(x, points = TRUE))
})

test_that("plot.tf smoke test for basic types", {
  x <- tf_rgp(3)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(x))
  expect_no_error(plot(x, points = TRUE))
  expect_no_error(plot(x, type = "lasagna"))
  expect_no_error(lines(x))
})
