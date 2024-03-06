grid <- round(seq(0, 1, length.out = 21), 3)
lin <- 2 * grid
curve <- sin(3 * pi * grid)

f_lin <- tfd(data.frame(1, grid, lin))
f_curve <- tfd(data.frame(1, grid, curve))

new_grid <- round(seq(0, 1, length.out = 41), 3)

test_that("argval checking works", {
  expect_error(tf_evaluate(f_lin, min(grid) - 1), ">=")
  expect_error(tf_evaluate(f_lin, max(grid) + 1), "<=")
  expect_error(tf_evaluate(f_lin, 1 * NA), "missing")
  expect_error(tf_evaluate(f_lin, list(1, 2)), "length")
})

test_that("evaluator tf_approx_linear works", {
  expect_identical(
    lin,
    suppressWarnings(tf_evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  )
  expect_identical(
    2 * new_grid,
    suppressWarnings(
      tf_evaluator(f_lin)(new_grid, arg = grid, evaluations = lin)
    )
  )
  expect_identical(lin, tf_evaluate(f_lin, grid)[[1]])
  expect_equal(2 * new_grid, tf_evaluate(f_lin, new_grid)[[1]])
  expect_equal(curve, tf_evaluate(f_curve, new_grid)[[1]][new_grid %in% grid])
})

test_that("re-assigning & extracting evaluator works", {
  tf_evaluator(f_lin) <- tf_approx_spline
  tf_evaluator(f_curve) <- tf_approx_spline
  expect_equal(
    body(environment(tf_evaluator(f_lin))[["f"]]),
    body(environment(tf_approx_spline)[["f"]])
  )
  expect_equal(
    body(environment(tf_evaluator(f_lin))[["f"]]),
    body(environment(tf_evaluator(f_curve))[["f"]])
  )
})

tf_evaluator(f_lin) <- tf_approx_spline
tf_evaluator(f_curve) <- tf_approx_spline

test_that("evaluator tf_approx_spline works", {
  expect_identical(
    lin,
    suppressWarnings(tf_evaluator(f_lin)(grid, arg = grid, evaluations = lin))
  )
  expect_identical(
    2 * new_grid,
    suppressWarnings(
      tf_evaluator(f_lin)(new_grid, arg = grid, evaluations = lin)
    )
  )
  expect_identical(lin, tf_evaluate(f_lin, grid)[[1]])
  expect_equal(2 * new_grid, tf_evaluate(f_lin, new_grid)[[1]])
})

test_that("multiple arg-vectors work for tfb", {
  fb <- tfb(tf_rgp(3), verbose = FALSE)
  expect_equal(
    unlist(tf_evaluate(fb, as.list(c(0, 0.5, 1)))),
    unlist(c(
      tf_evaluate(fb[1], 0), tf_evaluate(fb[2], 0.5), tf_evaluate(fb[3], 1)
    ))
  )
})
