cca_five <- tf_rgp(5)
names(cca_five) <- LETTERS[1:5]

test_that("names work", {
  expect_equal(names(cca_five), LETTERS[1:5])
  expect_equal(c(mean(cca_five), sd(cca_five)) |> names(), NULL)
  expect_equal(
    c(mean = mean(cca_five), sd = sd(cca_five)) |> names(),
    c("mean", "sd")
  )
  expect_equal((1:5 * cca_five[1]) |> names(), NULL)
  expect_equal((3 + cca_five[1]) |> names(), "A")
})


test_that("fun_op keeps names", {
  x <- tf_rgp(3)
  xn <- x
  names(xn) <- letters[1:3]

  expect_equal(names(x - xn), names(x))
  expect_equal(names(xn - x), names(xn))
  expect_equal(names(xn - mean(x)), names(xn))
  expect_equal(names(mean(xn) - xn), names(xn - mean(x)))
})
