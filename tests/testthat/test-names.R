cca_five <- tf_rgp(5)
names(cca_five) <- LETTERS[1:5]

test_that("names work", {
  expect_named(cca_five, LETTERS[1:5])
  expect_named(c(mean(cca_five), sd(cca_five)), NULL)
  expect_named(
    c(mean = mean(cca_five), sd = sd(cca_five)),
    c("mean", "sd")
  )
  expect_named((1:5 * cca_five[1]), NULL)
  expect_named((3 + cca_five[1]), "A")
})

test_that("fun_op keeps names", {
  x <- tf_rgp(3)
  xn <- x
  names(xn) <- letters[1:3]

  expect_named(x - xn, names(x))
  expect_named(xn - x, names(xn))
  expect_named(xn - mean(x), names(xn))
  expect_named(mean(xn) - xn, names(xn - mean(x)))
})
