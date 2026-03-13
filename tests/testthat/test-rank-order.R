grid <- round(seq(0, 10, length.out = 11), 3)
lin <- -3:3 * tfd(0.1 * grid, grid)
parallel <- -3:3 + tfd(0 * grid, grid)
names(lin) <- names(parallel) <- 1:7

lin_b <- tfb(lin, verbose = FALSE) |> suppressWarnings()
na <- 1 * NA + lin[1]

# ---- rank -------------------------------------------------------------------

test_that("rank.tf works with MHI (default)", {
  # For parallel non-crossing lines, MHI should rank lowest to highest
  r <- rank(parallel)
  expect_equal(r, setNames(1:7, 1:7))
  # Same for linear spread
  r_lin <- rank(lin)
  expect_equal(r_lin, setNames(1:7, 1:7))
})

test_that("rank.tf works with MBD", {
  r <- rank(parallel, depth = "MBD")
  # MBD is symmetric: -3 and 3 have equal depth, -2 and 2 have equal depth, etc.
  # Ranking should be symmetric around the center
  expect_equal(unname(r[1]), unname(r[7])) # most extreme pair
  expect_equal(unname(r[2]), unname(r[6]))
  expect_equal(unname(r[3]), unname(r[5]))
  expect_equal(unname(r[4]), 7) # center has highest depth -> highest rank
})

test_that("rank.tf works with custom depth function", {
  my_depth <- function(x, ...) tf_depth(x, depth = "MHI", ...)
  r_custom <- rank(parallel, depth = my_depth)
  r_mhi <- rank(parallel, depth = "MHI")
  expect_equal(r_custom, r_mhi)
})

test_that("rank.tf handles NAs", {
  x_na <- c(parallel, na)
  r <- rank(x_na, na.last = TRUE)
  expect_equal(unname(r[8]), 8)
  r_na <- rank(x_na, na.last = NA)
  # na.last = NA drops NA entries from rank result
  expect_length(r_na, 7)
})

test_that("rank.tf pads custom depth output for missing values", {
  my_depth <- function(x, ...) tf_depth(x, depth = "MHI", ...)
  x_mid_na <- c(parallel[1:3], na, parallel[4:7])

  r <- rank(x_mid_na, depth = my_depth, na.last = TRUE)

  expect_equal(unname(r[-4]), 1:7)
  expect_equal(unname(r[4]), 8)
})

test_that("rank.tf respects ties.method", {
  # Two identical functions
  x <- c(parallel[4], parallel[4], parallel[1])
  r_avg <- rank(x, ties.method = "average")
  expect_equal(unname(r_avg[1]), unname(r_avg[2]))
  r_min <- rank(x, ties.method = "min")
  expect_equal(unname(r_min[1]), unname(r_min[2]))
})

test_that("rank.tf works on tfb", {
  r <- rank(lin_b)
  expect_equal(unname(r), 1:7)
})

test_that("rank.default still works for numeric", {
  expect_equal(rank(c(3, 1, 2)), c(3, 1, 2))
  expect_error(rank(1:3, foo = 1), "unused")
})

# ---- order (via xtfrm) ------------------------------------------------------

test_that("order works on tf via xtfrm", {
  o <- order(parallel)
  expect_equal(o, 1:7)
  # Reversed:
  o_dec <- order(parallel, decreasing = TRUE)
  expect_equal(o_dec, 7:1)
})

test_that("xtfrm.tf returns MHI values", {
  xt <- xtfrm(parallel)
  expect_type(xt, "double")
  expect_length(xt, 7)
  # For parallel lines, xtfrm should increase
  expect_true(all(diff(xt) > 0))
})

# ---- sort --------------------------------------------------------------------

test_that("sort.tf sorts by MHI", {
  # Scramble then sort
  scrambled <- parallel[c(4, 2, 7, 1, 5, 3, 6)]
  sorted <- sort(scrambled)
  expect_length(sorted, 7)
  # After sorting by MHI, evaluations should be in ascending order
  evals <- tf_evaluations(sorted)
  for (i in seq_along(evals)[-1]) {
    expect_true(all(evals[[i]] >= evals[[i - 1]]))
  }
})

test_that("sort.tf decreasing works", {
  sorted_dec <- sort(parallel, decreasing = TRUE)
  evals <- tf_evaluations(sorted_dec)
  for (i in seq_along(evals)[-1]) {
    expect_true(all(evals[[i]] <= evals[[i - 1]]))
  }
})

test_that("sort.tf works with custom depth", {
  sorted <- sort(parallel, depth = "FM")
  expect_length(sorted, 7)
})

test_that("sort.tf respects na.last", {
  x_na <- c(parallel[2], na, parallel[1])

  sorted_keep <- sort(x_na, na.last = TRUE)
  expect_length(sorted_keep, 3)
  expect_true(is.na(sorted_keep[3]))

  sorted_drop <- sort(x_na, na.last = NA)
  expect_length(sorted_drop, 2)
})

# ---- min/max with depth ------------------------------------------------------

test_that("min/max still work pointwise by default", {
  pw_min <- min(parallel)
  pw_max <- max(parallel)
  expect_s3_class(pw_min, "tf")
  expect_s3_class(pw_max, "tf")
  # Pointwise min of parallel lines should equal the lowest line
  expect_equal(
    tf_evaluations(pw_min)[[1]],
    tf_evaluations(parallel)[[1]],
    ignore_attr = TRUE
  )
  expect_equal(
    tf_evaluations(pw_max)[[1]],
    tf_evaluations(parallel)[[7]],
    ignore_attr = TRUE
  )
})

test_that("min/max with depth select observations", {
  d_min <- min(parallel, depth = "MHI")
  d_max <- max(parallel, depth = "MHI")
  expect_length(d_min, 1)
  expect_length(d_max, 1)
  # MHI min should be the lowest function
  expect_equal(
    tf_evaluations(d_min)[[1]],
    tf_evaluations(parallel)[[1]],
    ignore_attr = TRUE
  )
  # MHI max should be the highest function
  expect_equal(
    tf_evaluations(d_max)[[1]],
    tf_evaluations(parallel)[[7]],
    ignore_attr = TRUE
  )
})

test_that("depth-based min/max handle missing values consistently", {
  x_mid_na <- c(parallel[1:3], na, parallel[4:7])

  expect_true(is.na(min(x_mid_na, depth = "MHI")))
  expect_true(is.na(max(x_mid_na, depth = "MHI")))

  expect_equal(
    tf_evaluations(min(x_mid_na, depth = "MHI", na.rm = TRUE))[[1]],
    tf_evaluations(parallel[1])[[1]],
    ignore_attr = TRUE
  )
  expect_equal(
    tf_evaluations(max(x_mid_na, depth = "MHI", na.rm = TRUE))[[1]],
    tf_evaluations(parallel[7])[[1]],
    ignore_attr = TRUE
  )
})

test_that("min/max with MBD depth", {
  d_min_mbd <- min(parallel, depth = "MBD")
  d_max_mbd <- max(parallel, depth = "MBD")
  # MBD min = most extreme, MBD max = most central
  expect_equal(
    tf_evaluations(d_max_mbd)[[1]],
    tf_evaluations(parallel)[[4]],
    ignore_attr = TRUE
  )
})

test_that("range with depth works", {
  r <- range(parallel, depth = "MHI")
  expect_length(r, 2)
  expect_s3_class(r, "tf")
})

# ---- median with various depths ---------------------------------------------

test_that("median.tf accepts all built-in centrality depths", {
  for (d in c("MBD", "FM", "FSD")) {
    m <- median(parallel, depth = d)
    expect_length(m, 1)
    # For symmetric parallel lines, centrality-based median = center function
    expect_equal(
      tf_evaluations(m)[[1]],
      tf_evaluations(parallel)[[4]],
      ignore_attr = TRUE
    )
  }
})

test_that("median.tf with MHI selects highest-depth (topmost) function", {
  m <- median(parallel, depth = "MHI")
  expect_length(m, 1)
  # MHI is directional: max depth = highest function
  expect_equal(
    tf_evaluations(m)[[1]],
    tf_evaluations(parallel)[[7]],
    ignore_attr = TRUE
  )
})

test_that("median.tf accepts custom depth function", {
  my_depth <- function(x, ...) tf_depth(x, depth = "MBD", ...)
  m <- median(parallel, depth = my_depth)
  expect_length(m, 1)
  expect_equal(
    tf_evaluations(m)[[1]],
    tf_evaluations(parallel)[[4]],
    ignore_attr = TRUE
  )
})

test_that("median.tf pointwise still works", {
  m <- median(parallel, depth = "pointwise")
  expect_s3_class(m, "tf")
})

# ---- fivenum ----------------------------------------------------------------

test_that("fivenum.tf works", {
  fn <- fivenum(parallel)
  expect_length(fn, 5)
  expect_named(fn, c("min", "lower_hinge", "median", "upper_hinge", "max"))
})

test_that("fivenum.tf orders correctly with MHI", {
  fn <- fivenum(parallel)
  evals <- tf_evaluations(fn)
  # min should be lowest, max should be highest
  expect_true(all(evals[["min"]] <= evals[["max"]]))
  expect_true(all(evals[["lower_hinge"]] <= evals[["upper_hinge"]]))
})

test_that("fivenum.tf handles NAs", {
  x_na <- c(parallel, na)
  fn_na <- fivenum(x_na, na.rm = FALSE)
  expect_true(is.na(fn_na))

  fn_rm <- fivenum(x_na, na.rm = TRUE)
  expect_length(fn_rm, 5)
})

test_that("fivenum.tf reuses order statistics for small samples", {
  fn_one <- fivenum(parallel[1])
  expect_length(fn_one, 5)
  expect_named(fn_one, c("min", "lower_hinge", "median", "upper_hinge", "max"))
  expect_true(all(vapply(
    tf_evaluations(fn_one),
    identical,
    logical(1),
    tf_evaluations(parallel[1])[[1]]
  )))

  fn_two <- fivenum(parallel[1:2])
  expect_length(fn_two, 5)
  expect_named(fn_two, c("min", "lower_hinge", "median", "upper_hinge", "max"))
})

test_that("fivenum.default still works for numeric", {
  expect_equal(fivenum(1:5), c(1, 2, 3, 4, 5))
})

# ---- summary with depth arg -------------------------------------------------

test_that("summary.tf accepts depth argument", {
  s_mbd <- summary(parallel)
  s_fm <- summary(parallel, depth = "FM")
  expect_length(s_mbd, 6)
  expect_length(s_fm, 6)
  expect_named(
    s_mbd,
    c("min", "lower_mid", "median", "mean", "upper_mid", "max")
  )
})

test_that("summary.tf handles all-NA input", {
  s_na <- summary(c(na, na))
  expect_length(s_na, 6)
  expect_named(
    s_na,
    c("min", "lower_mid", "median", "mean", "upper_mid", "max")
  )
  expect_true(all(is.na(s_na)))
})

# ---- validate_depth ----------------------------------------------------------

test_that("validate_depth rejects invalid depths", {
  expect_error(rank(parallel, depth = "INVALID"), "depth")
  expect_error(rank(parallel, depth = 42), "depth")
})
