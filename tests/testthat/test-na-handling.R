# NA entries should always be NULL internally.
# This ensures consistent behavior across constructors, operations, and display.

test_that("NA entries are NULL in tfd_reg via concatenation", {
  set.seed(1234)
  x <- tf_rgp(3)
  y <- c(x, NA)
  expect_length(y, 4)
  expect_equal(is.na(y), c(FALSE, FALSE, FALSE, TRUE), ignore_attr = "names")
  expect_null(unclass(y)[[4]])
  expect_no_error(print(y))
})

test_that("NA entries are NULL in tfd_reg via assignment", {
  set.seed(1234)
  x <- tf_rgp(5)
  x[c(2, 4)] <- NA
  expect_equal(is.na(x), c(FALSE, TRUE, FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(x)[[2]])
  expect_null(unclass(x)[[4]])
  expect_no_error(print(x))
})

test_that("NA entries are NULL in tfd_irreg", {
  set.seed(1234)
  x <- tf_rgp(3) |> tf_sparsify(0.5)
  y <- c(x, NA)
  expect_null(unclass(y)[[4]])
  expect_true(is.na(y)[4])
})

test_that("arithmetic with NA_real_ produces NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3)
  for (op in list(`+`, `-`, `*`, `/`, `^`)) {
    y <- op(x, NA_real_)
    expect_equal(is.na(y), rep(TRUE, 3), ignore_attr = "names")
    for (i in 1:3) expect_null(unclass(y)[[i]])
  }
})

test_that("NA_real_ op tfd produces NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3)
  y <- NA_real_ - x
  expect_equal(is.na(y), rep(TRUE, 3), ignore_attr = "names")
  for (i in 1:3) expect_null(unclass(y)[[i]])
})

test_that("vectorized arithmetic with NA produces NULL at correct positions", {
  set.seed(1234)
  x <- tf_rgp(4)
  y <- x + c(1, 2, NA_real_, 4)
  expect_equal(is.na(y)[3], TRUE)
  expect_null(unclass(y)[[3]])
  expect_false(is.na(y)[1])
})

test_that("tfd + tfd propagates NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3)
  y <- tf_rgp(3)
  y[2] <- NA
  z <- x + y
  expect_true(is.na(z)[2])
  expect_null(unclass(z)[[2]])
  expect_false(is.na(z)[1])
})

test_that("irregular arithmetic with NA_real_ produces NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3) |> tf_sparsify(0.6)
  y <- x - NA_real_
  expect_equal(is.na(y), rep(TRUE, 3), ignore_attr = "names")
  for (i in 1:3) expect_null(unclass(y)[[i]])
  expect_no_error(print(y))
})

test_that("Math ops preserve NULL entries and convert all-NA to NULL", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[2] <- NA
  y <- log(x)
  expect_true(is.na(y)[2])
  expect_null(unclass(y)[[2]])

  # log of strictly negative function produces NULL
  t <- seq(0, 1, length.out = 51)
  neg <- tfd(list(-1 - abs(sin(2 * pi * t))), arg = t)
  y2 <- suppressWarnings(log(neg))
  expect_true(is.na(y2)[1])
  expect_null(unclass(y2)[[1]])
})

test_that("Math.tfb handles NULL entries correctly", {
  t <- seq(0, 1, length.out = 51)
  mixed <- tfd(list(
    1 + abs(sin(2 * pi * t)),
    -1 - abs(cos(2 * pi * t)),
    1 + abs(sin(4 * pi * t))
  ), arg = t)
  b <- suppressWarnings(tfb(mixed, k = 7, verbose = FALSE))
  y <- suppressWarnings(log(b))
  expect_true(is_tfb(y))
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
  expect_no_error(print(y))
})

test_that("all-NA objects print correctly", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[1:3] <- NA
  printed <- capture.output(print(x))
  expect_true(any(grepl("\\[NA,NA\\]", printed)))
  expect_false(any(grepl("Inf", printed)))
})

test_that("as.tfd_irreg preserves NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[2] <- NA
  y <- as.tfd_irreg(x)
  expect_true(is.na(y)[2])
  expect_null(unclass(y)[[2]])
})

test_that("tfd() re-evaluation preserves NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[2] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0, 1, length.out = 101)))
  expect_true(is.na(y)[2])
  expect_null(unclass(y)[[2]])
})

test_that("subsetting preserves NULL entries", {
  set.seed(1234)
  x <- tf_rgp(5)
  x[c(2, 4)] <- NA
  sub <- x[c(1, 2, 3)]
  expect_true(is.na(sub)[2])
  expect_null(unclass(sub)[[2]])
})

test_that("tfb arithmetic with NA_real_ produces NULL entries and returns tfb", {
  set.seed(1234)
  x <- tf_rgp(3) |> tfb(k = 15, verbose = FALSE)
  y <- suppressWarnings(x + NA_real_)
  expect_true(is_tfb(y))
  expect_equal(is.na(y), rep(TRUE, 3), ignore_attr = "names")
  for (i in 1:3) expect_null(unclass(y)[[i]])
  # partial NA: numeric_op_tfb
  z <- suppressWarnings(NA_real_ - x[1:2])
  expect_true(is_tfb(z))
  expect_equal(is.na(z), rep(TRUE, 2), ignore_attr = "names")
  # tfb_op_tfb
  x2 <- x
  x2[2] <- NA
  w <- suppressWarnings(x + x2)
  expect_true(is_tfb(w))
  expect_equal(is.na(w), c(FALSE, TRUE, FALSE), ignore_attr = "names")
})

test_that("data.frame constructor handles all-NA rows", {
  df <- data.frame(
    id = rep(1:3, each = 10),
    arg = rep(1:10, 3),
    value = c(1:10, rep(NA, 10), 21:30)
  )
  x <- suppressWarnings(tfd(df))
  expect_true(is.na(x)[2])
  expect_null(unclass(x)[[2]])
  expect_no_error(print(x))
})

test_that("tf_arg returns numeric(0) for NA entries in tfd_irreg", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.3, 1), c(0, 0.7, 1))
  )
  x[2] <- NA
  args <- tf_arg(x)
  expect_length(args, 3)
  expect_equal(args[[2]], numeric(0))
  expect_true(length(args[[1]]) > 0)
  expect_true(length(args[[3]]) > 0)
})

test_that("tfd_irreg with NA survives round-trip via tf_arg", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.3, 1), c(0, 0.7, 1))
  )
  x[2] <- NA
  # This previously crashed with "argument 1 is not a vector" from order(NULL)
  y <- suppressWarnings(tfd(x, arg = tf_arg(x)))
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_s3_class(y, "tfd_irreg")
})

test_that("tf_fmean works on tfd_irreg with NA entries", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.3, 1), c(0, 0.7, 1))
  )
  x[2] <- NA
  result <- suppressWarnings(tf_fmean(x))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})
