test_that("round_resolution works", {
  # works with default and updown = 0
  expect_equal(round_resolution(1.5, 1), 2)
  expect_equal(round_resolution(2.5, 1), 2)
  expect_equal(round_resolution(2.49, 1, updown = 0), 2)
  # works with updown > 0
  expect_equal(round_resolution(1.1, 1, 1), 2)
  expect_equal(round_resolution(2.9, 1, 1), 3)
  # works with updown < 0
  expect_equal(round_resolution(1.9, 1, -1), 1)
  expect_equal(round_resolution(2.1, 1, -1), 2)
  # negative numbers
  expect_equal(round_resolution(-1.5, 1), -2)
  expect_equal(round_resolution(-2.5, 1, 1), -2)
  expect_equal(round_resolution(-2.5, 1, -1), -3)
  # handle NA
  expect_true(is.na(round_resolution(NA, 1)))
  expect_true(is.na(round_resolution(1.5, NA)))
})

test_that("format_bib", {
  bibentries <- list(checkmate = citation("checkmate"), R = citation())
  expect_string(format_bib("checkmate", "R"))
})

test_that("trapezoid_weights matches the trapezoidal rule", {
  # On an equidistant grid all interior weights equal the spacing,
  # boundary weights equal half the spacing -> the discrete integral
  # of v == 1 on [0, 1] is 1.
  arg <- seq(0, 1, length.out = 11)
  w <- trapezoid_weights(arg)
  expect_equal(sum(w), 1)
  expect_equal(w[1], 0.05)
  expect_equal(w[length(w)], 0.05)
  expect_true(all(abs(w[2:10] - 0.1) < 1e-12))

  # On a non-equidistant grid, weights still integrate constant 1 to the
  # domain length and weights equal the average of adjacent spacings interior,
  # half-spacings at the boundary.
  arg <- c(0, 0.1, 0.3, 0.7, 1)
  w <- trapezoid_weights(arg)
  expect_equal(sum(w), 1)
  expect_equal(w[1], 0.05)
  expect_equal(w[5], 0.15)
  expect_equal(w[2], 0.15)
  expect_equal(w[3], 0.3)
  expect_equal(w[4], 0.35)

  # Equivalence with the by-hand sum((dev[-n] + dev[-1])/2 * dt) formulation.
  set.seed(42)
  v <- runif(5)
  dt <- diff(arg)
  by_hand <- sum((v[-length(v)] + v[-1]) / 2 * dt)
  expect_equal(sum(w * v), by_hand)
})

test_that("unique_id replaces empty strings after coercion", {
  ids <- unique_id(factor(c("", "a", "")))
  expect_true(all(nzchar(ids)))
  expect_false(anyDuplicated(ids) > 0)
})
