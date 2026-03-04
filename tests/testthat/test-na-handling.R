# NA entries should always be NULL internally.
# This ensures consistent behavior across constructors, operations, and display.

test_that("NA entries are NULL in tfd_reg via concatenation", {
  set.seed(1234)
  x <- tf_rgp(3)
  y <- c(x, NA)
  expect_length(y, 4)
  expect_equal(is.na(y), c(FALSE, FALSE, FALSE, TRUE), ignore_attr = "names")
  expect_null(unclass(y)[[4]])
  expect_no_error(capture.output(print(y)))
})

test_that("NA entries are NULL in tfd_reg via assignment", {
  set.seed(1234)
  x <- tf_rgp(5)
  x[c(2, 4)] <- NA
  expect_equal(
    is.na(x),
    c(FALSE, TRUE, FALSE, TRUE, FALSE),
    ignore_attr = "names"
  )
  expect_null(unclass(x)[[2]])
  expect_null(unclass(x)[[4]])
  expect_no_error(capture.output(print(x)))
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
  z <- suppressWarnings(x + y)
  expect_true(is.na(z)[2])
  expect_null(unclass(z)[[2]])
  expect_false(is.na(z)[1])
})

test_that("all-NA irregular tfd + tfd preserves vector size", {
  x <- tfd(list(c(1, 2, 3), c(4, 5, 6)), arg = list(c(0, 0.5, 1), c(0, 0.3, 1)))
  x[] <- NA
  y <- suppressWarnings(x + x)
  expect_length(y, 2)
  expect_equal(is.na(y), c(TRUE, TRUE), ignore_attr = "names")
  for (i in seq_along(y)) expect_null(unclass(y)[[i]])
})

test_that("irregular arithmetic with NA_real_ produces NULL entries", {
  set.seed(1234)
  x <- tf_rgp(3) |> tf_sparsify(0.6)
  y <- x - NA_real_
  expect_equal(is.na(y), rep(TRUE, 3), ignore_attr = "names")
  for (i in 1:3) expect_null(unclass(y)[[i]])
  expect_no_error(capture.output(print(y)))
})

test_that("Math ops preserve NULL entries and convert all-NA to NULL", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[2] <- NA
  y <- suppressWarnings(log(x))
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
  mixed <- tfd(
    list(
      1 + abs(sin(2 * pi * t)),
      -1 - abs(cos(2 * pi * t)),
      1 + abs(sin(4 * pi * t))
    ),
    arg = t
  )
  b <- suppressWarnings(suppressMessages({
    capture.output(b <- tfb(mixed, k = 7, verbose = FALSE))
    b
  }))
  y <- suppressWarnings(log(b))
  expect_true(is_tfb(y))
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
  expect_no_error(capture.output(print(y)))
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
  x <- suppressMessages({
    capture.output(x <- tf_rgp(3) |> tfb(k = 15, verbose = FALSE))
    x
  })
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
  expect_no_error(capture.output(print(x)))
})

# --- tf_arg accessor for tfd_irreg with NAs ---

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

# --- tfd.tf re-evaluation with NAs: systematic test matrix ---
# Tests cover all combinations of:
#   shared vs per-function arg
#   with vs without NULL entries
#   same vs different extrapolation NAs vs no extrapolation NAs
# This exercises the normalize-prune-collapse logic in tfd.tf lines 370-418.

test_that("re-eval: shared arg, no NULLs, no extrapolation NAs", {
  x <- tfd(list(c(1, 2, 3), c(4, 5, 6)), arg = list(c(0, 0.5, 1), c(0, 0.5, 1)))
  y <- tfd(x, arg = seq(0, 1, length.out = 11))
  expect_s3_class(y, "tfd_reg")
  expect_equal(is.na(y), c(FALSE, FALSE), ignore_attr = "names")
})

test_that("re-eval: shared arg, with NULLs, no extrapolation NAs", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.5, 1), c(0, 0.5, 1))
  )
  x[2] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0, 1, length.out = 11)))
  expect_s3_class(y, "tfd_reg")
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
})

test_that("re-eval: shared arg, with NULLs, same extrapolation NAs", {
  # All non-NULL functions have same domain, so same NAs on wider grid
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0.1, 0.5, 0.9), c(0.1, 0.5, 0.9), c(0.1, 0.5, 0.9))
  )
  x[2] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0.1, 0.9, length.out = 11)))
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
})

test_that("re-eval: shared arg, with NULLs, different extrapolation NAs", {
  # Functions have different original domains -> different extrapolation NAs
  x <- tfd(
    list(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15)),
    arg = list(
      c(0, 0.25, 0.5, 0.75, 1),
      c(0, 0.3, 0.5, 0.7, 1),
      c(0.1, 0.3, 0.5, 0.7, 0.9)
    )
  )
  x[2] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0, 1, length.out = 21)))
  expect_s3_class(y, "tfd_irreg")
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
})

test_that("re-eval: shared arg, no NULLs, different extrapolation NAs", {
  # No NULL entries, but different arg ranges -> different extrapolation NAs
  # Domain is [0.1, 0.9] (union). Grid within domain but outside individual ranges.
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6)),
    arg = list(c(0.1, 0.5, 0.9), c(0.2, 0.5, 0.8))
  )
  y <- suppressWarnings(tfd(x, arg = seq(0.1, 0.9, length.out = 11)))
  expect_s3_class(y, "tfd_irreg")
  expect_false(anyNA(y))
})

test_that("re-eval: per-function arg, with NULLs, no extrapolation NAs", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.3, 1), c(0, 0.7, 1))
  )
  x[2] <- NA
  new_args <- list(
    seq(0, 1, length.out = 11),
    seq(0, 1, length.out = 11),
    seq(0, 1, length.out = 11)
  )
  y <- suppressWarnings(tfd(x, arg = new_args))
  expect_s3_class(y, "tfd_irreg")
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
})

test_that("re-eval: per-function arg, with NULLs, different extrapolation NAs", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0.1, 0.5, 0.9), c(0, 0.5, 1.0), c(0.2, 0.5, 0.8))
  )
  x[2] <- NA
  new_args <- list(
    seq(0, 1, length.out = 11),
    seq(0, 1, length.out = 11),
    seq(0, 1, length.out = 11)
  )
  y <- suppressWarnings(tfd(x, arg = new_args))
  expect_s3_class(y, "tfd_irreg")
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE), ignore_attr = "names")
  expect_null(unclass(y)[[2]])
})

test_that("re-eval: all NULLs", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    arg = list(c(0, 0.5, 1), c(0, 0.5, 1), c(0, 0.5, 1))
  )
  x[1:3] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0, 1, length.out = 11)))
  expect_equal(is.na(y), c(TRUE, TRUE, TRUE), ignore_attr = "names")
})

test_that("re-eval: single non-NULL entry with extrapolation NAs", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6)),
    arg = list(c(0.1, 0.5, 0.9), c(0.1, 0.5, 0.9))
  )
  x[1] <- NA
  y <- suppressWarnings(tfd(x, arg = seq(0.1, 0.9, length.out = 11)))
  expect_true(is.na(y)[1])
  expect_null(unclass(y)[[1]])
  expect_false(is.na(y)[2])
})
