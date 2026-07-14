test_that("Math.tfd threads `...` to the generic (#246)", {
  arg <- seq(0, 1, length.out = 11)
  x <- tfd(matrix(c(1.234, 2.345, 3.456, 4.567, 5.678,
                    6.789, 7.890, 8.901, 9.012, 10.123, 11.234), nrow = 1),
           arg = arg)

  # round honors digits arg
  r1 <- as.numeric(round(x, 1)[, arg])
  r0 <- as.numeric(round(x)[, arg])
  expect_false(isTRUE(all.equal(r1, r0)))
  expect_equal(r1, round(as.numeric(x[, arg]), 1))

  # log honors base arg
  l10 <- as.numeric(log(x, base = 10)[, arg])
  le <- as.numeric(log(x)[, arg])
  expect_false(isTRUE(all.equal(l10, le)))
  expect_equal(l10, log10(as.numeric(x[, arg])))

  # signif honors digits arg
  s2 <- as.numeric(signif(x, 2)[, arg])
  expect_equal(s2, signif(as.numeric(x[, arg]), 2))
})

test_that("Math.tfd basic ops without extra args still work", {
  arg <- seq(0.1, 1, length.out = 10)
  x <- tfd(matrix(arg, nrow = 1), arg = arg)
  expect_equal(as.numeric(sqrt(x)[, arg]), sqrt(arg))
  expect_equal(as.numeric(exp(x)[, arg]), exp(arg))
  expect_equal(as.numeric(abs(-x)[, arg]), arg)
})

test_that("Math.tfb threads `...` (round-trip via tfd)", {
  arg <- seq(0, 1, length.out = 51)
  raw <- tfd(matrix(1.2345 + sin(2 * pi * arg), nrow = 1), arg = arg)
  b <- suppressMessages({
    capture.output(b <- tfb(raw, k = 15, penalized = FALSE, verbose = FALSE))
    b
  })
  r1 <- suppressWarnings(round(b, 1))
  r0 <- suppressWarnings(round(b))
  expect_false(isTRUE(all.equal(
    as.numeric(r1[, arg]),
    as.numeric(r0[, arg])
  )))
})

test_that("Math.tfd works on irregular tfd", {
  irr <- tfd(list(c(1.1, 2.2, 3.3), c(2.6, 4.7)),
             arg = list(c(0, 0.5, 1), c(0, 1)))
  r <- round(irr, 0)
  vals <- tf_evaluations(r)
  expect_equal(vals[[1]], c(1, 2, 3))
  expect_equal(vals[[2]], c(3, 5))
})
