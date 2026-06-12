# tf_where / tf_anywhere on vector-valued (tf_mv) input: conditions are joint
# across components, referring to them by name (no `value` column).

grid <- seq(-1, 1, by = 0.2)
# x: curve 1 = identity, curve 2 = 2 * identity; y: curve 1 = -identity,
# curve 2 = identity -- simple lines with known sign patterns.
x_mv <- tfd(unname(rbind(grid, 2 * grid)), arg = grid)
y_mv <- tfd(unname(rbind(-grid, grid)), arg = grid)
fm <- tfd_mv(list(x = x_mv, y = y_mv))

test_that("tf_where on tf_mv evaluates joint conditions across components", {
  expect_equal(
    tf_where(fm, x > 0 & y > 0),
    list(numeric(0), seq(0.2, 1, by = 0.2)),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(tf_where(fm, x + y > 0, "first")),
    c(NA, 0.2)
  )
  expect_equal(
    unname(tf_where(fm, x + y > 0, "last")),
    c(NA, 1)
  )
  expect_equal(
    tf_where(fm, x <= 0 & y >= 0, "range"),
    data_frame(begin = c(-1, 0), end = 0),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(tf_where(fm, x^2 + y^2 > 4, "any")),
    c(FALSE, TRUE)
  )
  # `arg` is available in cond, as for univariate input
  expect_equal(
    tf_where(fm, arg > 0.5 & y > 0),
    list(numeric(0), c(0.6, 0.8, 1)),
    ignore_attr = TRUE
  )
})

test_that("tf_anywhere on tf_mv is tf_where(..., return = 'any')", {
  expect_identical(
    tf_anywhere(fm, x^2 + y^2 > 4),
    tf_where(fm, x^2 + y^2 > 4, "any")
  )
})

test_that("single-component conditions agree with univariate tf_where", {
  expect_identical(
    tf_where(fm, x > 0.5),
    tf_where(fm$x, value > 0.5)
  )
  expect_identical(
    tf_anywhere(fm, y > 0.5),
    tf_anywhere(fm$y, value > 0.5)
  )
})

test_that("`value` in cond on tf_mv aborts with a classed hint", {
  expect_error(
    tf_where(fm, value > 0),
    class = "tf_mv_where_value"
  )
  expect_error(
    tf_anywhere(fm, value > 0),
    class = "tf_mv_where_value"
  )
  # ... unless a component is actually named `value`
  fmv <- tfd_mv(list(value = x_mv, y = y_mv))
  expect_identical(
    tf_where(fmv, value > 0.5),
    tf_where(fmv$value, value > 0.5)
  )
})

test_that("tf_where on tf_mv requires a common grid unless arg is supplied", {
  grid_fine <- seq(-1, 1, by = 0.1)
  y_fine <- tfd(unname(rbind(-grid_fine, grid_fine)), arg = grid_fine)
  fmi <- tfd_mv(list(x = x_mv, y = y_fine))
  expect_error(
    tf_where(fmi, x > 0 & y > 0),
    class = "tf_mv_incommensurate_args"
  )
  expect_error(
    tf_anywhere(fmi, x > 0),
    class = "tf_mv_incommensurate_args"
  )
  # explicit arg picks the evaluation grid and lifts the requirement
  expect_equal(
    tf_where(fmi, x > 0 & y > 0, arg = grid),
    tf_where(fm, x > 0 & y > 0)
  )
  expect_equal(
    tf_anywhere(fmi, x^2 + y^2 > 4, arg = grid),
    tf_anywhere(fm, x^2 + y^2 > 4)
  )
  # ... but the per-component list that tf_arg() returns for such objects is
  # rejected -- it would be misread as per-curve grids
  expect_error(
    tf_where(fmi, x > 0, arg = tf_arg(fmi)),
    class = "tf_mv_incommensurate_args"
  )
})

test_that("'arg' is rejected as a component name (would shadow the grid column)", {
  expect_error(
    tfd_mv(list(arg = x_mv, y = y_mv)),
    "reserved"
  )
})

test_that("tf_where on zero-length tf_mv works for all return modes", {
  fm0 <- fm[0]
  expect_identical(unname(tf_where(fm0, x > 0)), list())
  expect_identical(unname(tf_where(fm0, x > 0, "first")), numeric(0))
  expect_identical(unname(tf_where(fm0, x > 0, "last")), numeric(0))
  expect_identical(unname(tf_where(fm0, x > 0, "any")), logical(0))
  expect_identical(
    tf_where(fm0, x > 0, "range"),
    data.frame(begin = numeric(0), end = numeric(0))
  )
})

test_that("NA curves in tf_mv propagate as in the univariate case", {
  fm_na <- fm
  fm_na[1] <- NA
  expect_true(is.na(tf_where(fm_na, x > 0)[[1]]))
  expect_identical(
    tf_where(fm_na, x > 0)[-1],
    tf_where(fm, x > 0)[-1]
  )
  expect_true(is.na(tf_where(fm_na, x > 0, "first")[[1]]))
  # partial NA: a curve missing in *any* component counts as missing
  fm_pna <- fm
  x_na <- x_mv
  x_na[1] <- NA
  fm_pna$x <- x_na
  expect_true(is.na(tf_where(fm_pna, y > 0)[[1]]))
  expect_identical(
    tf_where(fm_pna, y > 0)[-1],
    tf_where(fm, y > 0)[-1]
  )
})

test_that("tf_where works with more than two components", {
  fm3 <- tfd_mv(list(x = x_mv, y = y_mv, z = x_mv + y_mv))
  expect_identical(
    tf_where(fm3, x + y == z),
    tf_where(fm3, TRUE)
  )
  expect_equal(
    unname(tf_anywhere(fm3, z > x + y)),
    c(FALSE, FALSE)
  )
})

test_that("tf_where works on tfb_mv input", {
  set.seed(1212)
  fd <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  fb <- tfb_mv(fd, k = 15, verbose = FALSE)
  expect_true(is_tfb_mv(fb))
  res <- tf_where(fb, x > 0 & y > 0)
  expect_type(res, "list")
  expect_length(res, 3)
  expect_identical(
    res,
    tf_where(fb, x > 0 & y > 0, arg = tf_arg(fb))
  )
  # agrees with joint condition on the evaluated components
  expect_identical(
    unname(tf_anywhere(fb, x > y)),
    unname(map_lgl(tf_evaluations(fb), \(df) any(df$x > df$y)))
  )
  expect_error(tf_where(fb, value > 0), class = "tf_mv_where_value")
})
