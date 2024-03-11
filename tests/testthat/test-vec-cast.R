x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
names(x) <- letters[1:5]
l <- list(
  x = x,
  x_short = x |> tf_zoom(0.1, 0.4),
  x_short_longdom = tfd(x |> tf_zoom(0.1, 0.4), domain = tf_domain(x),
                        evaluator = tf_approx_linear),
  x_sp = tf_sparsify(x, dropout = .1) |> tfd(evaluator = tf_approx_spline),
  x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2) |>
    tfd(evaluator = tf_approx_locf),
  x_fake_ir = as.tfd_irreg(x |> tf_zoom(0.1, 0.4)),
  x_short_sp = tf_zoom(x, 0.2, 0.7) |> tf_sparsify(dropout = .2) |>
    tfd(evaluator = tf_approx_none),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp= .1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fp = tfb_fpc(x, pve = 1),
  fp_low = tfb_fpc(x, pve = .95)
)

expect_cast_result <- function(x, to, irreg = FALSE, ignore = 1,
                               tolerance = testthat_tolerance()) {
  cast <- vec_cast(x, to)
  # values same as x, class & attributes same as to:
  expect_s3_class(cast, class(to)[1])
  expect_equal(tf_evaluations(cast), tf_evaluations(x),
                   ignore_attr = TRUE, tolerance = tolerance)
  expect_identical(attributes(cast)[-ignore],
                   attributes(to)[-ignore])
}

test_that("vec_cast for tfd to tfd works/fails as expected", {
  # reg -> reg
  ## should fail:
  expect_error(vec_cast(l$x, l$x_short), "domain") # wrong domain
  ## should work:
  expect_cast_result(l$x, l$x_short_longdom, ignore = 2) #w/o args
  expect_cast_result(l$x_short_longdom, l$x, ignore = 2) #w/o args
  expect_cast_result(l$x_short, l$x, ignore = 2) #w/o args

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
  expect_cast_result(l$x_fake_ir, l$x, ignore = 2) #w/o arg

  # irreg -> irreg
  ## should always work as long as domains compatible
  expect_cast_result(l$x_fake_ir, l$x_ir)
  expect_cast_result(l$x_short_sp, l$x_sp)
  expect_error(vec_cast(l$x_sp, l$x_short_sp), "domains")
})

test_that("vec_cast for tfb to tfb works/fails as expected", {
  # tfb -> tfb  should always fail unless bases are identical
  expect_cast_result(l$b, l$b)
  expect_cast_result(l$fp_low, l$fp_low)
  expect_error(vec_cast(l$b, l$b2), "precision")
  expect_error(vec_cast(l$b, l$fp), "precision")
  expect_error(vec_cast(l$fp, l$fp_low), "precision")
})

test_that("vec_cast for tfd to tfb fails as expected", {
  expect_error(vec_cast(l$x, l$b), "precision")
  expect_error(vec_cast(l$x_ir, l$fp) |> suppressWarnings(), "precision")
  expect_error(vec_cast(l$x_short, l$fp_low), "domains")
})

test_that("vec_cast for tfb to tfd works/fails as expected", {
  expect_cast_result(l$b, l$x, ignore = 4) #env(evaluator)
  expect_cast_result(l$b, l$x_ir, ignore = 4) #env(evaluator)
  expect_cast_result(l$fp, l$x, ignore = c(2, 4)) # arg names, env(evaluator)
  expect_cast_result(l$fp, l$x_sp, ignore = 4) #env(evaluator)
  expect_cast_result(l$bu, l$x_short_longdom, ignore = c(2, 4)) # arg + evaluator
  expect_cast_result(l$fp_low, l$x_short_longdom, ignore = c(2, 4)) # arg + evaluator

  expect_error(vec_cast(l$fp, l$x_short), "domains") #env(evaluator)
  expect_error(vec_cast(l$bu, l$x_short), "domains") #env(evaluator)
})
