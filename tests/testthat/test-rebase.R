set.seed(11331)
x <- suppressMessages({
  tf_rgp(5, arg = 301L) |>
    tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend) |>
    setNames(letters[1:5])
})

l <- list(
  x_sp = tf_sparsify(x, dropout = 0.1),
  x_ir = tf_sparsify(x, dropout = 0.1) |> tf_jiggle(amount = 0.05),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp = 0.1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fpc = tfb_fpc(x, pve = 1),
  fpc_low = tfb_fpc(x, pve = 0.95)
)

for (i in seq_along(l)) {
  test_that("tf_rebase.tfd preserves args & evals and transfers attributes", {
    x_rebase <- tf_rebase(x, l[[i]], verbose = FALSE)
    expect_equal(
      x_rebase |> tf_evaluations(),
      l[[i]] |> tf_evaluations(),
      tolerance = 0.01
    )
    expect_equal(
      x_rebase |> tf_arg(),
      l[[i]] |> tf_arg()
    )
    expect_named(x_rebase, names(x))
    expect_valid_tf(x_rebase)
    skip_on_cran() # to avoid non-reproducible BS-error on Fedora 36 - MKL
    expect_true(
      compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
    )
  })
}

#-------------------------------------------------------------------------------

set.seed(1133111)
x <- tf_rgp(5, arg = 301L) |>
  tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend) |>
  suppressMessages()
names(x) <- letters[1:5]
b <- tfb(x, k = 45, verbose = FALSE)

l <- list(
  x = x,
  x_sp = tf_sparsify(x, dropout = 0.1),
  x_ir = tf_sparsify(x, dropout = 0.1) |> tf_jiggle(amount = 0.2),
  b2 = tfb(x, k = 15, bs = "tp", sp = 0.1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fpc = tfb_fpc(x, pve = 1),
  fpc_low = tfb_fpc(x, pve = 0.95)
)

for (i in seq_along(l)) {
  test_that(
    paste0(
      "tf_rebase.tfb_spline preserves args & evals and transfers attributes for l[",
      i,
      "]"
    ),
    {
      x_rebase <- tf_rebase(b, l[[i]], verbose = FALSE)
      expect_equal(
        x_rebase |> tf_evaluations(),
        l[[i]] |> tf_evaluations(),
        tolerance = 0.01
      )
      expect_equal(
        x_rebase |> tf_arg(),
        l[[i]] |> tf_arg()
      )
      expect_named(x_rebase, names(x))
      expect_valid_tf(x_rebase)
      skip_on_cran() # to avoid non-reproducible BS-error on Fedora 36 - MKL
      expect_true(
        compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
      )
    }
  )
}

#-------------------------------------------------------------------------------

set.seed(1133111)
x <- suppressMessages({
  tf_rgp(5, arg = 301L) |>
    tf_smooth() |>
    tfd(evaluator = tf_approx_fill_extend) |>
    setNames(letters[1:5])
})
fpc <- tfb_fpc(x, pve = 1)

l <- list(
  x = x,
  x_sp = tf_sparsify(x, dropout = 0.1),
  x_ir = tf_sparsify(x, dropout = 0.1) |> tf_jiggle(amount = 0.2),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp = 0.1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fpc_low = tfb_fpc(x, pve = 0.95)
)
for (i in seq_along(l)) {
  test_that("tf_rebase.tfb_fpc preserves args & evals and transfers attributes", {
    x_rebase <- tf_rebase(fpc, l[[i]], verbose = FALSE)
    expect_equal(
      x_rebase |> tf_evaluations(),
      l[[i]] |> tf_evaluations(),
      tolerance = 0.01
    )
    expect_equal(
      x_rebase |> tf_arg(),
      l[[i]] |> tf_arg()
    )
    expect_named(x_rebase, names(x))
    expect_valid_tf(x_rebase)
    skip_on_cran() # to avoid non-reproducible BS-error on Fedora 36 - MKL
    expect_true(
      compare_tf_attribs(x_rebase, l[[i]], check_attrib = FALSE) |> all()
    )
  })
}

#-------------------------------------------------------------------------------
# regression tests for #238, #239, #240

test_that("#238 tf_rebase.tfb.tfd accepts dots without double-splicing", {
  set.seed(238)
  b <- tfb(tf_rgp(3), verbose = FALSE)
  raw <- tf_rgp(3)
  # any dot collided with itself before the fix ("matched by multiple actual arguments")
  expect_silent(res1 <- tf_rebase(b, raw, evaluator = "tf_approx_spline"))
  expect_s3_class(res1, "tfd")
  expect_identical(attr(res1, "evaluator_name"), "tf_approx_spline")
})

test_that("tf_rebase with function-valued evaluator is documented as broken (see #261)", {
  # Tracked in #261: tfd.tf needs to accept function-valued evaluator.
  # When that issue is fixed, this test should be flipped to expect_no_error.
  xb <- tfb(tf_rgp(3), verbose = FALSE)
  x <- tf_rgp(3)
  expect_error(tf_rebase(xb, x, evaluator = tf_approx_spline))
})

test_that("#239 tf_rebase(tfd, tfb_spline) fits on the target spline grid", {
  set.seed(239)
  x <- tf_rgp(3, arg = seq(0, 1, length.out = 11))
  b <- tfb(tf_rgp(3, arg = seq(0, 1, length.out = 51)), k = 25, verbose = FALSE)
  res <- tf_rebase(x, b)
  expect_true(tf:::same_basis(res, b))
  # combining is now warning-free since both share the same basis
  expect_warning(res + b, NA)
})

test_that("tf_rebase(tfd, tfb_spline) honors custom arg", {
  set.seed(2391)
  x <- tf_rgp(3, arg = seq(0, 1, length.out = 11))
  b <- tfb(tf_rgp(3, arg = seq(0, 1, length.out = 51)), k = 15, verbose = FALSE)
  custom_arg <- seq(0, 1, length.out = 21)

  res <- suppressWarnings(tf_rebase(x, b, arg = custom_arg))

  expect_identical(tf_arg(res), custom_arg)
  expect_equal(attr(res, "basis_matrix"), attr(b, "basis")(custom_arg))
})

test_that("#269 tf_rebase(tfd, tfb_spline) projects directly w/o interpolation step", {
  # Per Fabian's review: rebase should project object's native evaluations onto
  # basis_from's basis, *not* interpolate first (which would compound errors).
  set.seed(269)
  arg_x <- seq(0, 1, length.out = 41)
  x <- tf_rgp(3, arg = arg_x) |>
    tf_smooth() |>
    suppressMessages()
  b <- tfb(
    tf_rgp(5, arg = seq(0, 1, length.out = 101)) |>
      tf_smooth() |>
      suppressMessages(),
    k = 25,
    verbose = FALSE
  )

  res <- tf_rebase(x, b)

  # Result lives in basis_from's basis (and arg) -- not lossy on subsequent ops
  expect_true(tf:::same_basis(res, b))
  expect_identical(tf_arg(res), tf_arg(b))
  expect_warning(res + b[1:3], NA)

  # Evaluating result at x's native grid should reproduce x to within basis
  # approximation error -- baseline = same projection performed directly using
  # basis_from's basis and unpenalized LS at arg_x; rebase result should be no
  # worse than this baseline (no additional interpolation-error budget).
  B_at_x <- attr(b, "basis")(arg_x)
  ev_x <- tf_evaluations(x)
  ev_res_at_x <- map(vec_data(res), \(coef) drop(B_at_x %*% coef))
  rebase_err <- map2_dbl(ev_x, ev_res_at_x, \(a, b) max(abs(a - b)))
  # Reference: unpenalized LS projection onto the same basis at arg_x.
  proj_err <- map_dbl(ev_x, \(y) {
    coef <- qr.coef(qr(B_at_x), y)
    max(abs(y - drop(B_at_x %*% coef)))
  })
  # Penalized projection is at most modestly worse than unpenalized LS
  # (small multiplicative slack for the smoothing penalty).
  expect_true(all(rebase_err <= pmax(proj_err * 3, 0.05)))
})

test_that("#269 tf_rebase(tfd, tfb_spline) under-determined fit works via penalty", {
  # object has fewer unique args than basis_from's k -- penalty must rescue.
  set.seed(2690)
  x <- tf_rgp(3, arg = seq(0, 1, length.out = 11)) |> suppressMessages()
  b <- tfb(tf_rgp(3, arg = seq(0, 1, length.out = 51)), k = 25, verbose = FALSE)
  # default tfb is penalized, so this should succeed (perhaps with sparse warning)
  expect_no_error(res <- suppressWarnings(tf_rebase(x, b)))
  expect_true(tf:::same_basis(res, b))
})

test_that("#240 default tfb_spline/tfb_fpc methods return length-0 prototypes", {
  proto_s <- suppressWarnings(tfb_spline())
  expect_s3_class(proto_s, "tfb_spline")
  expect_length(proto_s, 0)
  proto_f <- suppressMessages(tfb_fpc())
  expect_s3_class(proto_f, "tfb_fpc")
  expect_length(proto_f, 0)
})
