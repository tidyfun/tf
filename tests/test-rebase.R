test_that("tf_rebase.tfd preserves args & evals and transfers attributes", {
  set.seed(11331)
  x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend)

  l <- list(
    x_sp = tf_sparsify(x, dropout = .1),
    x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
    b = tfb(x, k = 45),
    b2 = tfb(x, k = 15, bs = "tp", sp= .1),
    bu = tfb(x, k = 15, penalized = FALSE),
    bg = tfb(x, k = 5, global = TRUE),
    fp = tfb_fpc(x, pve = 1),
    fp_low = tfb_fpc(x, pve = .95)
  )
  for (i in seq_along(l)) {
    cat(i)
    x_rebase <- tf_rebase(x, l[[i]])
    expect_equal(
      x_rebase |> tf_evaluations(),
      l[[i]] |> tf_evaluations(),
      tolerance = .01
    )
    expect_equal(
      x_rebase |> tf_arg(),
      l[[i]] |> tf_arg(),
    )
    c(x_rebase, l[[i]])
  }
})


#!:  check idempotence, name preservation, warnings about bad bases
# what edge cases: ?!?
