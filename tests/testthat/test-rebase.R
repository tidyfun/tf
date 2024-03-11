test_that("tf_rebase.tfd preserves args & evals and transfers attributes", {
  set.seed(11331)
  x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
  names(x) <- letters[1:5]

  l <- list(
    x_2 = tfd(as.matrix(x), resolution = tf_resolution(x) * 2),
    x_sp = tf_sparsify(x, dropout = .1),
    x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .05),
    b = tfb(x, k = 45, verbose = FALSE),
    b2 = tfb(x, k = 15, bs = "tp", sp= .1, verbose = FALSE),
    bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
    bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
    fpc = tfb_fpc(x, pve = 1),
    fpc_low = tfb_fpc(x, pve = .95)
  )
  for (i in seq_along(l)) {
    # cat(i)
    x_rebase <- tf_rebase(x, l[[i]], verbose = FALSE)
    expect_equal(
       x_rebase |> tf_evaluations(),
       l[[i]] |> tf_evaluations(),
       tolerance = .01
    )
    expect_equal(
       x_rebase |> tf_arg(),
       l[[i]] |> tf_arg()
    )
    expect_true(
      compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
    )
    expect_equal(names(x_rebase), names(x))
   # try(c(x_rebase, l[[i]])) #!! see #77
  }
})

test_that("tf_rebase.tfb_spline preserves args & evals and transfers attributes", {
  set.seed(1133111)
  x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
  names(x) <- letters[1:5]
  b <- tfb(x, k = 45, verbose = FALSE)

  l <- list(
    x = x,
    x_sp = tf_sparsify(x, dropout = .1),
    x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
    b2 = tfb(x, k = 15, bs = "tp", sp = .1, verbose = FALSE),
    bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
    bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
    fpc = tfb_fpc(x, pve = 1),
    fpc_low = tfb_fpc(x, pve = .95)
  )
  for (i in seq_along(l)) {
    # cat(i)
    x_rebase <- tf_rebase(b, l[[i]], verbose = FALSE)
    expect_equal(
      x_rebase |> tf_evaluations(),
      l[[i]] |> tf_evaluations(),
      tolerance = .01
    )
    expect_equal(
      x_rebase |> tf_arg(),
      l[[i]] |> tf_arg()
    )
    expect_true(
      compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
    )
    expect_equal(names(x_rebase), names(x))
    # c(x_rebase, l[[i]]) # !! see #77
  }
})

test_that("tf_rebase.tfb_fpc preserves args & evals and transfers attributes", {
  set.seed(1133111)
  x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
  names(x) <- letters[1:5]
  fpc <- tfb_fpc(x, pve = 1)

  l <- list(
    x = x,
    x_sp = tf_sparsify(x, dropout = .1),
    x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
    b = tfb(x, k = 45, verbose = FALSE),
    b2 = tfb(x, k = 15, bs = "tp", sp= .1, verbose = FALSE),
    bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
    bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
    fpc_low = tfb_fpc(x, pve = .95)
  )
  for (i in seq_along(l)) {

    x_rebase <- tf_rebase(fpc, l[[i]], verbose = FALSE)
    expect_equal(
      x_rebase |> tf_evaluations(),
      l[[i]] |> tf_evaluations(),
      tolerance = .01
    )
    expect_equal(
      x_rebase |> tf_arg(),
      l[[i]] |> tf_arg()
    )
    expect_true(
      compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
    )
    expect_equal(names(x_rebase), names(x))
    # c(x_rebase, l[[i]]) # !! see #77
  }
})
