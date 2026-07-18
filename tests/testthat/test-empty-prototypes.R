# Empty prototypes carry the sentinel domain c(NA, NA) (see new_tfd); they
# must combine cleanly with populated vectors in all vctrs paths (#241
# regression cluster).

test_that("vctrs combinations with empty tfd prototypes work", {
  set.seed(4711)
  x <- tf_rgp(3)

  expect_identical(vctrs::vec_c(tfd(), x), x)
  expect_identical(vctrs::vec_c(x, tfd()), x)
  expect_identical(c(tfd(), x), x)
  expect_identical(vctrs::vec_c(tfd(), tfd()), tfd())

  combined <- vctrs::vec_rbind(
    vctrs::data_frame(f = tfd()),
    vctrs::data_frame(f = x)
  )
  expect_identical(combined$f, x)

  xi <- tf_jiggle(x)
  expect_identical(vctrs::vec_c(tfd(), xi), xi)

  xb <- tfb(x, verbose = FALSE)
  expect_identical(vctrs::vec_c(tfb(), xb), xb)
})

test_that("tfd(list()) returns the length-0 prototype", {
  x <- tfd(list())
  expect_s3_class(x, "tfd")
  expect_length(x, 0)
  expect_identical(x, tfd(numeric(0)))
})

test_that("vctrs combinations with empty tf_mv prototypes work", {
  set.seed(4712)
  m <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  e <- tfd_mv(list(x = tfd(), y = tfd()))

  expect_identical(tf_domain(e), c(NA_real_, NA_real_))
  expect_true(validate_tf(e))
  expect_identical(c(e, m), m)
  expect_no_error(mean(e))
})

test_that("as.matrix on length-0 tf vectors returns 0-row matrices", {
  set.seed(4713)
  x <- tf_rgp(3)

  m0 <- as.matrix(x[0])
  expect_identical(dim(m0), c(0L, length(tf_arg(x))))
  expect_identical(colnames(m0), as.character(tf_arg(x)))

  expect_no_warning(m5 <- as.matrix(x[0], arg = seq(0, 1, length.out = 5)))
  expect_identical(dim(m5), c(0L, 5L))

  expect_no_warning(e5 <- as.matrix(tfd(), arg = seq(0, 1, length.out = 5)))
  expect_identical(dim(e5), c(0L, 5L))
})

test_that("tf_mfpc_scores works on length-0 tfb_mfpc", {
  set.seed(4714)
  m <- tfb_mfpc(tfd_mv(list(x = tf_rgp(6), y = tf_rgp(6))), npc = 2)
  s <- tf_mfpc_scores(m[0])
  expect_identical(dim(s), c(0L, 2L))
  expect_identical(colnames(s), c("mfpc1", "mfpc2"))
})

test_that("all-NA tfd construction and combination work (#241)", {
  expect_warning(nafn <- tfd(list(NA_real_)), "index positions")
  expect_length(nafn, 1L)
  expect_true(is.na(nafn))

  set.seed(4715)
  x <- tf_rgp(3)
  suppressWarnings(combined <- vctrs::vec_c(nafn, x))
  expect_length(combined, 4L)
  expect_identical(unname(is.na(combined)), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("tfd_mv aborts on widening tfb component domains", {
  set.seed(4716)
  b1 <- tfb(tf_rgp(4, arg = seq(0, 1, length.out = 21)), k = 8, verbose = FALSE)
  b2 <- tfb(
    tf_rgp(4, arg = seq(0, 2, length.out = 21)),
    k = 8,
    verbose = FALSE
  )
  expect_error(
    tfd_mv(list(x = b1, y = b2)),
    "extrapolate"
  )
})
