test_that("tf_split works as expected", {
  x <- tf_rgp(3)
  x_i <- tf_jiggle(x) |> tf_sparsify()
  x_sp <- tfb(x, verbose = FALSE)
  x_pc <- tfb_fpc(x, verbose = FALSE)


  tfs <- tf_split(x, splits = c(0.3, 0.5))
  expect_list(tfs, types = "tfd_reg", len = 3)

  tfs_i <- tf_split(x_i, splits = c(0.3, 0.5))
  expect_list(tfs_i, types = "tfd_irreg", len = 3)

  tfs_sp <- tf_split(x_sp, splits = c(0.3, 0.5))
  expect_list(tfs_sp, types = "tfb_spline", len = 3)

  tfs_pc <- suppressWarnings(tf_split(x_pc, splits = c(0.3, 0.5)))
  expect_list(tfs_pc, types = "tfb_fpc", len = 3)

  expect_identical(tfs,
                   tf_split(x, splits = c(0, 0.3, 0.5, 1)))

  expect_error(tf_split(x, splits = c(0.3, 0.5, 2)),
               "<= 1")
  expect_error(tf_split(x, splits = c(-2, 0.3, 0.5)),
               ">= 0")

  tfs_l <- tf_split(x, splits = 0.3, include = "left")
  expect_identical(map(tfs_l, tf_domain), list(c(0, 0.299), c(0.3, 1)))

  tfs_r <- tf_split(x, splits = 0.3, include = "right")
  expect_identical(map(tfs_r, tf_domain), list(c(0, 0.3), c(0.301, 1)))
})


test_that("tf_combine works as expected", {
  x <- tf_rgp(3)
  x_i <- tf_jiggle(x) |> tf_sparsify()
  x_sp <- tfb(x, verbose = FALSE)
  x_pc <- tfb_fpc(x, verbose = FALSE)

  expect_identical(x,
                   do.call(tf_combine, tf_split(x, 0.3, "left")))
  expect_identical(x_i,
                   do.call(tf_combine, tf_split(x_i, 0.3, "left")))

  expect_equal(tfd(x_sp),
               do.call(tf_combine, tf_split(x_sp, 0.3, "left")),
               tolerance = 1e-3)

  expect_equal(tfd(x_pc),
               do.call(tf_combine, tf_split(x_pc, 0.3, "left") |>
                 suppressWarnings()),
               tolerance = 1e-5)

  #
  expect_identical(
    do.call(tf_combine, tf_split(x, 0.3)) |> suppressMessages(),
    do.call(tf_combine, rev(tf_split(x, 0.3))) |> suppressMessages())

  expect_error(
    do.call(tf_combine, c(tf_split(x, 0.3), strict = TRUE)),
    "multiple values")

  expect_class(tf_combine(x, tf_jiggle(x)),
               "tfd_irreg")

  expect_error(tf_combine(x, tf_jiggle(x), strict = TRUE),
               "not strictly ordered")

  tfs <- tf_split(x, splits = c(0.2, 0.6), include = "left")
  tfs2_sparse <- tf_sparsify(tfs[[2]])
  tfs3_spline <- tfb(tfs[[3]], verbose = FALSE)
  expect_class(tf_combine(tfs[[1]], tfs2_sparse, tfs3_spline), "tfd_irreg")
  expect_equal(tf_combine(tfs[[1]], tfs2_sparse, tfs3_spline) |> tf_domain(),
               c(0, 1))
})
