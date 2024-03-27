x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
names(x) <- letters[1:5]
l <- list(
  x = x,
  x_short = x |> tf_zoom(0.1, 0.4),
  x_short_longdom = tfd(x |> tf_zoom(0.1, 0.4), domain = tf_domain(x),
                        evaluator = tf_approx_linear),
  x_sp = tf_sparsify(x, dropout = .1),
  x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
  x_fake_ir = as.tfd_irreg(x |> tf_zoom(0.1, 0.4)),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp= .1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fp = tfb_fpc(x, pve = 1),
  fp_low = tfb_fpc(x, pve = .95)
)

test_that("vctrs basics & concatentation work for all subclasses", {
  for (i in seq_along(l)) {
    testthat::expect_identical(
      l[[i]],
      vec_restore(vec_proxy(l[[i]]), vec_ptype(l[[i]]))
    )
    testthat::expect_identical(
      vec_ptype2(l[[i]], l[[i]]),
      vec_ptype(l[[i]])
    )
    testthat::expect_identical(
      l[[i]],
      c(l[[i]], l[[i]])[1:length(l[[i]])]
    )
  }
  expect_s3_class(
    vec_ptype_common(l$x, l$x_sp, l$x_b) |> suppressWarnings(),
    "tfd_irreg")
  expect_s3_class(
    vec_ptype_common(l$x_ir, l$x_fp, l$b_u) |> suppressWarnings(),
    "tfd_irreg")
  expect_s3_class(
    vec_ptype2(l$x, l$fp) |> suppressWarnings(),
    "tfd_reg")
  expect_equal(
    vec_ptype2(l$fp, l$x) |> suppressWarnings(),
    vec_ptype2(l$x, l$fp) |> suppressWarnings())
  expect_equal(
    vec_ptype2(l$x, l$x_ir) |> suppressWarnings(),
    vec_ptype2(l$x_ir, l$x) |> suppressWarnings())
  # TODO: evaluator of result depends on order of args here - seems unavoidable?
  # expect_equal(
  #   vec_ptype2(l$x_short_longdom, l$x) |> suppressWarnings(),
  #   vec_ptype2(l$x, l$x_short_longdom) |> suppressWarnings())
})

test_that("concatentation of mixed tfs works as expected", {
  # tfd_reg - irreg
  expect_s3_class(c(l$x, l$x_ir) |> suppressWarnings(), "tfd_irreg")
  expect_warning(c(l$x, l$x_ir), "incompatible <tfd_reg> with <tfd_irreg>")
  expect_identical(c(l$x, l$x_ir) |> suppressWarnings() |> tf_evaluations(),
                   c(tf_evaluations(l$x), tf_evaluations(l$x_ir)))

  # tfd_reg - spline
  expect_s3_class(c(l$x, l$b) |> suppressWarnings(), "tfd_reg")
  expect_warning(c(l$x, l$b), "incompatible <tfd_reg> with <tfb_spline>")
  expect_identical(c(l$x, l$b) |> tf_evaluations() |> suppressWarnings(),
                   c(tf_evaluations(l$x), tf_evaluations(l$b)),
                   tolerance = 0.01)

  # tfd_irreg - fpc
  expect_s3_class(c(l$fp, l$x_sp) |> suppressWarnings(), "tfd_irreg")
  expect_warning(c(l$fp, l$x_sp), "incompatible <tfd_irreg> with <tfb_fpc>")
  expect_identical(c(l$fp, l$x_sp) |> tf_evaluations() |> suppressWarnings(),
                   c(tf_evaluations(l$fp), tf_evaluations(l$x_sp)),
                   tolerance = 0.01, ignore_attr = TRUE)

  # fpc - spline
  expect_s3_class(c(l$fp, l$b) |> suppressWarnings(), "tfd_reg")
  expect_warning(c(l$fp, l$b), "incompatible <tfb_fpc> with <tfb_spline>")
  expect_identical(c(l$fp, l$b) |> tf_evaluations() |> suppressWarnings(),
                   c(tf_evaluations(l$fp), tf_evaluations(l$b)),
                   tolerance = 0.01, ignore_attr = TRUE)

  all_c <- c(l[[1]], l[[2]], l[[3]], l[[4]], l[[5]], l[[6]], l[[7]], l[[8]],
             l[[9]], l[[10]], l[[11]], l[[12]]) |>  suppressWarnings()
  # TODO: y do.call(c, l) no work?
  incremental_c <- l[[1]]
  for (i in 2:length(l)) incremental_c <- c(incremental_c, l[[i]]) |> suppressWarnings()
  expect_identical(incremental_c, all_c)
})

# !! NA concatenating
# c(x, NA) works, c(NA, x) does not --
# but: vec_ptype2(NA, x) works
# and doing the concatenation manually with vec_init, vec_cast, vec_slice etc works as well.
# -- this is a vctrs issue - wontfix
