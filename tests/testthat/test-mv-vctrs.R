test_that("subsetting a tf_mv keeps components and names aligned", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  names(f) <- letters[1:5]
  g <- f[2:4]
  expect_s3_class(g, "tfd_mv")
  expect_length(g, 3)
  expect_identical(tf_ncomp(g), 2L)
  expect_equal(tf_evaluations(g$x)[[1]], tf_evaluations(f$x)[[2]])
})

test_that("c() concatenates tf_mv component-wise", {
  set.seed(2)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  g <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  cc <- c(f, g)
  expect_s3_class(cc, "tfd_mv")
  expect_length(cc, 5)
  expect_equal(tf_evaluations(cc$x)[[4]], tf_evaluations(g$x)[[1]])
})

test_that("vec_ptype2 / vec_cast work for tf_mv", {
  set.seed(3)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  tb <- tfb_mv(f, verbose = FALSE)
  # tfd_mv <-> tfb_mv combine to a common (tfd_mv) type
  cc <- suppressWarnings(c(f, tb))
  expect_s3_class(cc, "tfd_mv")
  expect_length(cc, 6)
  # explicit cast
  cast <- suppressWarnings(vctrs::vec_cast(tb, f))
  expect_s3_class(cast, "tfd_mv")
})

test_that("combining incompatible tf_mv errors", {
  f2 <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  f3 <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2), z = tf_rgp(2)))
  expect_error(c(f2, f3), class = "vctrs_error_incompatible_type")
  named_diff <- tfd_mv(list(a = tf_rgp(2), b = tf_rgp(2)))
  expect_error(c(f2, named_diff), class = "vctrs_error_incompatible_type")
})

test_that("tf_mv works as a tibble / data.frame column", {
  skip_if_not_installed("tibble")
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  tbl <- tibble::tibble(id = 1:4, traj = f)
  expect_identical(nrow(tbl), 4L)
  expect_s3_class(tbl$traj, "tfd_mv")
  expect_identical(vctrs::vec_ptype_abbr(f), "tfd_mv")
  sub <- tbl[2:3, ]
  expect_length(sub$traj, 2)
})

test_that("vec_ptype_full reports the dimension", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_match(vctrs::vec_ptype_full(f), "tfd_mv<d=2>")
})
