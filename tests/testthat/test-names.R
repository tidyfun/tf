test_that("names work", {
  cca_five <- set_names(tf_rgp(5), LETTERS[1:5])
  expect_named(cca_five, LETTERS[1:5])
  expect_named(c(mean(cca_five), sd(cca_five)), NULL)
  expect_named(
    c(mean = mean(cca_five), sd = sd(cca_five)),
    c("mean", "sd")
  )
  expect_named((1:5 * cca_five[1]), NULL)
  expect_named((3 + cca_five[1]), "A")

  # keeps names of first argument
  cca_lower <- set_names(cca_five, tolower)
  expect_named(cca_five + cca_lower, names(cca_five))
  expect_named(cca_five * cca_lower, names(cca_five))
  expect_named(cca_lower + cca_five, names(cca_lower))
  expect_named(cca_lower * cca_five, names(cca_lower))
  # one of the arguments has no names
  names(cca_lower) <- NULL
  expect_named(cca_five * cca_lower, names(cca_five))
  expect_named(cca_five + cca_lower, names(cca_five))
  expect_named(cca_lower + cca_five, NULL)
  expect_named(cca_lower * cca_five, NULL)
})

test_that("vec_arith keeps names", {
  x <- tf_rgp(3)
  xn <- x
  names(xn) <- letters[1:3]

  expect_named(x - xn, names(x))
  expect_named(xn - x, names(xn))
  expect_named(xn - mean(x), names(xn))
  expect_named(mean(xn) - xn, names(xn - mean(x)))
})
