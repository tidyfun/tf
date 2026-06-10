x <- tf_rgp(5, arg = 301L) |>
  tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend) |>
  suppressMessages()
names(x) <- letters[1:5]
l <- list(
  x = x,
  x_short = x |> tf_zoom(0.1, 0.4),
  x_short_longdom = tfd(
    x |> tf_zoom(0.1, 0.4),
    domain = tf_domain(x),
    evaluator = tf_approx_linear
  ),
  x_sp = tf_sparsify(x, dropout = 0.1) |> tfd(evaluator = tf_approx_spline),
  x_ir = tf_sparsify(x, dropout = 0.1) |>
    tf_jiggle(amount = 0.2) |>
    tfd(evaluator = tf_approx_locf),
  x_fake_ir = as.tfd_irreg(x |> tf_zoom(0.1, 0.4)),
  x_short_sp = tf_zoom(x, 0.2, 0.7) |>
    tf_sparsify(dropout = 0.2) |>
    tfd(evaluator = tf_approx_none),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp = 0.1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fp = tfb_fpc(x, pve = 1),
  fp_low = tfb_fpc(x, pve = 0.95)
)

expect_cast_result <- function(
  x,
  to,
  irreg = FALSE,
  ignore = 1,
  tolerance = testthat_tolerance()
) {
  cast <- vec_cast(x, to)
  # values same as x, class & attributes same as to:
  expect_s3_class(cast, class(to)[1])
  expect_equal(
    tf_evaluations(cast),
    tf_evaluations(x),
    ignore_attr = TRUE,
    tolerance = tolerance
  )
  # for some reason names get shifted around in attributes list so check separately:
  expect_equal(names(cast), names(x))

  cast_attrs <- attributes(unname(cast))
  to_attrs <- attributes(unname(to))
  if (length(ignore)) {
    cast_attrs <- cast_attrs[-ignore]
    to_attrs <- to_attrs[-ignore]
  }
  expect_identical(cast_attrs, to_attrs)
}

test_that("vec_cast for tfd to tfd works/fails as expected", {
  # reg -> reg
  ## should fail:
  expect_error(vec_cast(l$x, l$x_short), "domain") # wrong domain
  ## should work:
  expect_cast_result(l$x, l$x_short_longdom, ignore = 1) #w/o args
  expect_cast_result(l$x_short_longdom, l$x, ignore = 1) #w/o args
  expect_cast_result(l$x_short, l$x, ignore = 1) #w/o args

  # reg -> irreg:
  ## should always work if domains compatible
  expect_cast_result(l$x, l$x_sp)
  expect_cast_result(l$x_short, l$x_sp)
  expect_cast_result(l$x_short_longdom, l$x_ir)
  expect_cast_result(l$x, l$x_ir)
  expect_cast_result(l$x_short, l$x_ir)
  expect_error(vec_cast(l$x, l$x_fake_ir), "domains")

  # irreg -> reg:
  ## should always fail if irreg is akshually_irregular
  expect_error(vec_cast(l$x_sp, l$x), "Can't convert")
  expect_error(vec_cast(l$x_ir, l$x), "Can't convert")
  expect_cast_result(l$x_fake_ir, l$x, ignore = 1) #w/o arg

  # irreg -> irreg
  ## should always work as long as domains compatible
  expect_cast_result(l$x_fake_ir, l$x_ir)
  expect_cast_result(l$x_short_sp, l$x_sp)
  expect_error(vec_cast(l$x_sp, l$x_short_sp), "domains")
})

test_that("vec_cast for tfb to tfb works/fails as expected", {
  # tfb -> tfb  should always fail unless bases are identical;
  # when it succeeds, all attributes (including `arg`) must match `to` exactly.
  expect_cast_result(l$b, l$b, ignore = integer(0))
  expect_cast_result(l$fp_low, l$fp_low, ignore = integer(0))
  expect_error(vec_cast(l$b, l$b2), "precision")
  expect_error(vec_cast(l$b, l$fp), "precision")
  expect_error(vec_cast(l$fp, l$fp_low), "precision")
})

test_that("tfb_spline -> tfb_spline cast actually exercises tf_rebase (#239)", {
  # Two tfb_spline vectors with *different* arg grids and different `k` so
  # `tf_basis(to)(tf_arg(x))` does not match x's basis matrix, the in-method
  # same_basis check at R/vctrs-cast.R:108 is FALSE, and vec_cast_tfb_tfb
  # actually runs tf_rebase (not the identity short-circuit at L113).
  set.seed(239239)
  x_src <- tf_rgp(3, arg = seq(0, 1, length.out = 21))
  x_to <- tf_rgp(3, arg = seq(0, 1, length.out = 41))
  b_src <- tfb(x_src, k = 7, verbose = FALSE)
  b_to <- tfb(x_to, k = 11, verbose = FALSE)
  expect_false(tf:::same_basis(b_src, b_to))

  cast <- vctrs::allow_lossy_cast(vec_cast(b_src, b_to))
  # invariant: result lives in to's basis (arg grid + basis_label + same_basis)
  expect_identical(tf_arg(cast), tf_arg(b_to))
  expect_identical(attr(cast, "basis_label"), attr(b_to, "basis_label"))
  expect_identical(attr(cast, "basis_args"), attr(b_to, "basis_args"))
  expect_true(tf:::same_basis(cast, b_to))
  # vec_ptype of the cast equals vec_ptype(to) on all attributes except the
  # `basis` closure (closure environments are not stable across constructors).
  cast_ptype_attrs <- attributes(vec_ptype(cast))
  to_ptype_attrs <- attributes(vec_ptype(b_to))
  cast_ptype_attrs$basis <- NULL
  to_ptype_attrs$basis <- NULL
  expect_identical(cast_ptype_attrs, to_ptype_attrs)
})

test_that("tfb_fpc -> tfb_fpc cast actually exercises tf_rebase (#239)", {
  # Different arg grids -> same_basis is FALSE so the rebase path runs.
  set.seed(2392392)
  x_src <- suppressMessages(tf_smooth(tf_rgp(8, arg = seq(0, 1, length.out = 21))))
  x_to <- suppressMessages(tf_smooth(tf_rgp(8, arg = seq(0, 1, length.out = 41))))
  f_src <- tfb_fpc(x_src, pve = 0.9)
  f_to <- tfb_fpc(x_to, pve = 0.9)
  expect_false(tf:::same_basis(f_src, f_to))

  cast <- vctrs::allow_lossy_cast(vec_cast(f_src, f_to))
  expect_identical(tf_arg(cast), tf_arg(f_to))
  expect_identical(attr(cast, "basis_label"), attr(f_to, "basis_label"))
  expect_true(tf:::same_basis(cast, f_to))
})

test_that("vec_cast for tfd to tfb fails as expected", {
  expect_error(vec_cast(l$x, l$b), "precision")
  expect_error(vec_cast(l$x_ir, l$fp) |> suppressWarnings(), "precision")
  expect_error(vec_cast(l$x_short, l$fp_low), "domains")
})

test_that("vec_cast for tfb to tfd works/fails as expected", {
  expect_cast_result(l$b, l$x, ignore = 3) #env(evaluator)
  expect_cast_result(l$b, l$x_ir, ignore = 3) #env(evaluator)
  expect_cast_result(l$fp, l$x, ignore = c(1, 3)) # arg , env(evaluator)
  expect_cast_result(l$fp, l$x_sp, ignore = 3) #env(evaluator)
  expect_cast_result(l$bu, l$x_short_longdom, ignore = c(1, 3)) # arg + evaluator
  expect_cast_result(l$fp_low, l$x_short_longdom, ignore = c(1, 3)) # arg + evaluator

  expect_error(vec_cast(l$fp, l$x_short), "domains") #env(evaluator)
  expect_error(vec_cast(l$bu, l$x_short), "domains") #env(evaluator)
})
