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
