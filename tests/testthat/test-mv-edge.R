# Edge cases, NA handling, and branches not exercised elsewhere.

# ---- empty / length-0 ---------------------------------------------------------

test_that("empty tf_mv prototype: accessors, ops, c(), tibble", {
  f0 <- tfd_mv(list())
  expect_length(f0, 0L)
  expect_identical(tf_ncomp(f0), 0L)
  expect_identical(length(tf_components(f0)), 0L)
  expect_identical(tf_arg(f0), numeric(0))
  expect_identical(tf_evaluations(f0), list())
  expect_identical(is.na(f0), logical(0))
  expect_identical(dim(as.matrix(f0)), c(0L, 0L, 0L))
  expect_identical(dim(tf_count(f0)), c(0L, 0L))
  # c() with empty stays length 0
  expect_length(c(f0, f0), 0L)
  expect_silent(format(f0))
  expect_invisible(print(f0))
  # tibble column with 0 rows
  skip_if_not_installed("tibble")
  expect_identical(nrow(tibble::tibble(traj = f0)), 0L)
})

test_that("Reduce-based ops on a zero-component prototype stay zero-length", {
  # d == 0 means the underlying component list is empty, so Reduce() has no
  # accumulator and would return NULL without an explicit guard.
  f0 <- tfd_mv(list())
  expect_identical(f0 == f0, logical(0))
  expect_identical(f0 != f0, logical(0))
  expect_s3_class(tf_norm(f0), "tfd")
  expect_length(tf_norm(f0), 0L)
  expect_s3_class(tf_inner(f0, f0), "tfd")
  expect_length(tf_inner(f0, f0), 0L)
  expect_s3_class(tf_distance(f0, f0), "tfd")
})

test_that("tfb_mv prototype is constructible and identifiable", {
  tb0 <- tfb_mv(list())
  expect_s3_class(tb0, "tfb_mv")
  expect_length(tb0, 0L)
  # default-method path
  tb0d <- tfb_mv(numeric(0))
  expect_s3_class(tb0d, "tfb_mv")
  expect_s3_class(tfb_mv(tfd_mv(list())), "tfb_mv")
})

test_that("zero-curve tf_mv with components has empty matrix/count shapes", {
  f0 <- tfd_mv(list(x = tfd(), y = tfd()))
  expect_length(f0, 0L)
  expect_identical(tf_ncomp(f0), 2L)
  expect_identical(dim(as.matrix(f0)), c(0L, 0L, 2L))
  counts <- tf_count(f0)
  expect_identical(dim(counts), c(0L, 2L))
  expect_identical(colnames(counts), c("x", "y"))
})

# ---- n = 1, d = 1 -------------------------------------------------------------

test_that("single-curve and single-component tf_mv work end-to-end", {
  set.seed(11)
  one <- tfd_mv(list(x = tf_rgp(1), y = tf_rgp(1)))
  expect_length(one, 1L)
  expect_length(c(one, one), 2L)
  expect_identical(dim(one[1, c(.2, .5)]), c(1L, 2L, 2L))

  single <- tfd_mv(list(only = tf_rgp(4)))
  expect_identical(tf_ncomp(single), 1L)
  expect_s3_class(single + single, "tfd_mv")
  # vec_ptype_full reports d = 1
  expect_match(vctrs::vec_ptype_full(single), "d=1")
})

# ---- NA handling --------------------------------------------------------------

test_that("NA in any component marks the curve as NA, ops propagate NAs", {
  set.seed(12)
  fx <- tf_rgp(4)
  fx[2] <- NA
  fy <- tf_rgp(4)
  fy[3] <- NA
  f <- tfd_mv(list(x = fx, y = fy))
  # any-component-NA => curve NA
  expect_equal(unname(is.na(f)), c(FALSE, TRUE, TRUE, FALSE))
  m_all <- suppressWarnings(mean(f))
  expect_length(m_all, 1L)
  m_complete <- mean(f, na.rm = TRUE)
  expect_equal(m_complete, mean(f[!is.na(f)]))
  # subset preserves NA status
  expect_true(is.na(f[2]))
  # arithmetic with NA curves: result NA at NA positions
  ff <- suppressWarnings(f + f)
  expect_equal(unname(is.na(ff)), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("all-NA mv curve is handled in tf_evaluations()", {
  set.seed(13)
  fx <- tf_rgp(3)
  fx[1] <- NA
  fy <- tf_rgp(3)
  fy[1] <- NA
  f <- tfd_mv(list(x = fx, y = fy))
  ev <- tf_evaluations(f)
  # new uniform shape: list of per-curve (arg, x, y) data.frames. For the
  # all-NA curve the arg grid is still well-defined (the union of the
  # components' native grids) but the value columns are NA-filled.
  expect_s3_class(ev[[1]], "data.frame")
  expect_identical(colnames(ev[[1]]), c("arg", "x", "y"))
  expect_true(all(is.na(ev[[1]]$x)) && all(is.na(ev[[1]]$y)))
  expect_s3_class(ev[[2]], "data.frame")
  expect_identical(colnames(ev[[2]]), c("arg", "x", "y"))
  expect_false(anyNA(ev[[2]]$x) || anyNA(ev[[2]]$y))
})

# ---- Summary group generic and stat methods ----------------------------------

test_that("Summary group generic on tf_mv is component-wise", {
  set.seed(14)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  s <- sum(f)
  expect_s3_class(s, "tfd_mv")
  expect_length(s, 1L)
  expect_equal(s$x, sum(f$x))
  expect_equal(s$y, sum(f$y))
  expect_equal(min(f)$x, min(f$x))
  expect_equal(max(f)$y, max(f$y))
})

test_that("var and sd on tf_mv are component-wise and return length-1 mv", {
  set.seed(15)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  v <- var(f)
  s <- sd(f)
  expect_s3_class(v, "tfd_mv")
  expect_length(v, 1L)
  expect_s3_class(s, "tfd_mv")
  expect_length(s, 1L)
  expect_equal(
    tf_evaluations(v$x)[[1]],
    tf_evaluations(var(f$x))[[1]]
  )
  expect_equal(tf_evaluations(s$y)[[1]], tf_evaluations(sd(f$y))[[1]])
})

# ---- Arithmetic edge cases ----------------------------------------------------

test_that("unary minus on tf_mv works (vec_arith.tf_mv.MISSING)", {
  set.seed(16)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  nf <- -f
  expect_s3_class(nf, "tfd_mv")
  expect_equal(
    tf_evaluations((nf + f)$x)[[1]],
    rep(0, length(tf_arg(f$x))),
    tolerance = 1e-9
  )
})

test_that("incompatible arithmetic op errors via vec_arith.tf_mv.default", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(vctrs::vec_arith("+", f, "abc"))
})

# ---- tfb_mv non-tf inputs and vctrs paths ------------------------------------

test_that("tfb_mv.list accepts a list of pre-built tfb components", {
  set.seed(17)
  tx <- tfb(tf_rgp(3), verbose = FALSE)
  ty <- tfb(tf_rgp(3), verbose = FALSE)
  tb <- tfb_mv(list(x = tx, y = ty))
  expect_s3_class(tb, "tfb_mv")
  expect_true(all(map_lgl(tf_components(tb), is_tfb)))
})

test_that("tfb_mv.list refits non-tfb components via tfd_mv", {
  set.seed(171)
  mx <- matrix(rnorm(33), nrow = 3)
  my <- matrix(rnorm(33), nrow = 3)
  tb <- suppressWarnings(suppressMessages(
    tfb_mv(list(x = mx, y = my))
  ))
  expect_s3_class(tb, "tfb_mv")
  expect_identical(tf_ncomp(tb), 2L)
})

test_that("c(tfb_mv, tfb_mv) and tibble column work for tfb_mv", {
  set.seed(18)
  tb1 <- tfb_mv(tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3))), verbose = FALSE)
  tb2 <- tfb_mv(tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2))), verbose = FALSE)
  cc <- suppressWarnings(c(tb1, tb2))
  expect_s3_class(cc, "tfb_mv")
  expect_length(cc, 5L)
  expect_match(vctrs::vec_ptype_abbr(tb1), "tfb_mv")
  expect_match(vctrs::vec_ptype_full(tb1), "tfb_mv<d=2>")
})

# ---- tfd_mv.tf_mv (re-evaluation on a new arg grid) --------------------------

test_that("tfd_mv(<tf_mv>, arg = ...) re-evaluates on a new grid", {
  set.seed(19)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  new_grid <- seq(0, 1, length.out = 21)
  g <- tfd_mv(f, arg = new_grid)
  expect_s3_class(g, "tfd_mv")
  expect_equal(tf_arg(g), new_grid)
  expect_equal(g$x, tfd(f$x, arg = new_grid))
  expect_equal(g$y, tfd(f$y, arg = new_grid))
})

# ---- tf_rebase with an mv basis_from -----------------------------------------

test_that("tf_rebase(mv, mv_basis) uses each component as its own basis", {
  set.seed(20)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  basis_from <- tfb_mv(
    tfd_mv(list(x = tf_rgp(1), y = tf_rgp(1))),
    k = 8,
    verbose = FALSE
  )
  r <- tf_rebase(f, basis_from)
  expect_s3_class(r, "tfb_mv")
  expect_equal(r$x, tf_rebase(f$x, basis_from$x))
  expect_equal(r$y, tf_rebase(f$y, basis_from$y))
})

# ---- tf_evaluate.tf_mv direct call -------------------------------------------

test_that("tf_evaluate(<tf_mv>) returns per-curve data.frames on requested arg", {
  set.seed(21)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  out <- tf_evaluate(f, arg = c(0.2, 0.5, 0.8))
  expect_length(out, 3L)
  # uniform shape: list of n per-curve data.frames (arg, comp1, ..., compd)
  expect_s3_class(out[[1]], "data.frame")
  expect_identical(colnames(out[[1]]), c("arg", "x", "y"))
  expect_identical(nrow(out[[1]]), 3L)
  expect_identical(out[[1]]$arg, c(0.2, 0.5, 0.8))
  expect_equal(out[[1]]$x, tf_evaluate(f$x, arg = c(0.2, 0.5, 0.8))[[1]])
  expect_equal(out[[3]]$y, tf_evaluate(f$y, arg = c(0.2, 0.5, 0.8))[[3]])
})

# ---- as.matrix(arg=...) and as.data.frame both modes -------------------------

test_that("as.matrix(<tf_mv>, arg = ...) re-evaluates on a new grid", {
  set.seed(22)
  f <- tfd_mv(list(x = tf_rgp(3, arg = 11L), y = tf_rgp(3, arg = 11L)))
  m <- as.matrix(f, arg = seq(0, 1, length.out = 5), interpolate = TRUE)
  expect_identical(dim(m), c(3L, 5L, 2L))
  expect_equal(
    m[,, "x"],
    as.matrix(f$x, arg = seq(0, 1, length.out = 5), interpolate = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    m[,, "y"],
    as.matrix(f$y, arg = seq(0, 1, length.out = 5), interpolate = TRUE),
    ignore_attr = TRUE
  )
})

test_that("as.matrix(<tf_mv>) uses the union grid for mixed regular component grids", {
  x <- tfd(matrix(1:3, nrow = 1), arg = 1:3)
  y <- tfd(matrix(11:14, nrow = 1), arg = 1:4)
  f <- tfd_mv(list(x = x, y = y))
  expect_warning(m <- as.matrix(f), "interpolate = FALSE")
  expect_identical(dim(m), c(1L, 4L, 2L))
  expect_true(is.na(m[1, "4", "x"]))
  expect_equal(unname(m[1, "4", "y"]), 14)
})

test_that("[.tf_mv(matrix = FALSE) uses per-curve union grids when j is missing", {
  x <- tfd(matrix(1:3, nrow = 1), arg = 1:3)
  y <- tfd(matrix(11:14, nrow = 1), arg = 1:4)
  f <- tfd_mv(list(x = x, y = y))
  expect_warning(
    out <- f[,, interpolate = FALSE, matrix = FALSE],
    "interpolate = FALSE"
  )
  expect_length(out, 1L)
  expect_equal(out[[1]]$arg, 1:4)
  expect_true(is.na(out[[1]]$x[4]))
  expect_equal(out[[1]]$y, 11:14)
})

test_that("as.data.frame(<tf_mv>) supports both unnested and 1-column forms", {
  set.seed(23)
  f <- tfd_mv(list(x = tf_rgp(2, arg = 5L), y = tf_rgp(2, arg = 5L)))
  d1 <- as.data.frame(f)
  expect_identical(nrow(d1), 2L)
  expect_named(d1, "data")
  # default schema is long: (id, arg, component, value), 2 * 5 * 2 = 20 rows
  d2 <- as.data.frame(f, unnest = TRUE)
  expect_named(d2, c("id", "arg", "component", "value"))
  expect_s3_class(d2$component, "factor")
  expect_identical(levels(d2$component), c("x", "y"))
  expect_identical(nrow(d2), 2L * 5L * 2L)
  # x values match the univariate as.data.frame on the x-component
  expect_equal(d2$value[d2$component == "x"],
               as.data.frame(f$x, unnest = TRUE)$value)
  expect_equal(d2$value[d2$component == "y"],
               as.data.frame(f$y, unnest = TRUE)$value)
  # opt-in to legacy wide schema
  d2w <- as.data.frame(f, unnest = TRUE, long = FALSE)
  expect_named(d2w, c("id", "arg", "x", "y"))
  expect_identical(nrow(d2w), 2L * 5L)
  expect_equal(d2w$x, as.data.frame(f$x, unnest = TRUE)$value)
  expect_equal(d2w$y, as.data.frame(f$y, unnest = TRUE)$value)
})

# ---- registration: ref_component = "norm" path -------------------------------

test_that("ref_component = 'norm' runs the norm-based registration path", {
  set.seed(24)
  t <- seq(0, 1, length.out = 51)
  shifts <- c(-0.05, 0, 0.05)
  # use a bump so the norm carries phase information (avoids sin/cos degeneracy)
  bump <- function(s) exp(-30 * (t - (0.5 + s))^2)
  x <- tfd(t(sapply(shifts, \(s) bump(s))), arg = t)
  y <- tfd(t(sapply(shifts, \(s) 0.5 * bump(s))), arg = t)
  f <- tfd_mv(list(x = x, y = y))
  w <- suppressWarnings(suppressMessages(
    tf_estimate_warps(
      f,
      method = "affine",
      type = "shift",
      ref_component = "norm"
    )
  ))
  aligned <- suppressWarnings(suppressMessages(tf_align(f, w)))
  meanvar <- function(mv) {
    mean(sapply(tf_components(mv), function(comp) {
      suppressWarnings(mean(tf_evaluations(var(comp))[[1]], na.rm = TRUE))
    }))
  }
  expect_s3_class(w, "tfd")
  expect_length(w, 3L)
  expect_equal(
    as.matrix(w),
    as.matrix(suppressWarnings(suppressMessages(
      tf_estimate_warps(tf_norm(f), method = "affine", type = "shift")
    )))
  )
  expect_lt(meanvar(aligned), 0.1 * meanvar(f))
})

# ---- tf_component<- adds new components, length mismatch errors --------------

test_that("tfb_mv distributes a component-named list ... per component", {
  set.seed(202)
  f <- tfd_mv(list(x = tf_rgp(3, arg = 101L), y = tf_rgp(3, arg = 101L)))
  # per-component k via a list keyed by component names
  tb <- tfb_mv(f, k = list(x = 5, y = 15), verbose = FALSE)
  expect_s3_class(tb, "tfb_mv")
  lab_x <- attr(tf_components(tb)$x, "basis_label")
  lab_y <- attr(tf_components(tb)$y, "basis_label")
  expect_match(lab_x, "k = 5")
  expect_match(lab_y, "k = 15")
  # mixing shared (bs) and per-component (k)
  tb2 <- tfb_mv(f, k = list(x = 5, y = 15), bs = "tp", verbose = FALSE)
  expect_match(attr(tf_components(tb2)$x, "basis_label"), 'bs = "tp"')
  expect_match(attr(tf_components(tb2)$y, "basis_label"), 'k = 15')
  # back-compat: scalar k is shared across components
  tb3 <- tfb_mv(f, k = 8, verbose = FALSE)
  expect_match(attr(tf_components(tb3)$x, "basis_label"), "k = 8")
  expect_match(attr(tf_components(tb3)$y, "basis_label"), "k = 8")
})

test_that("tfb_mv.list forwards basis arguments after tfd_mv conversion", {
  set.seed(203)
  mx <- matrix(rnorm(3 * 20), nrow = 3)
  my <- matrix(rnorm(3 * 20), nrow = 3)
  tb <- suppressWarnings(suppressMessages(
    tfb_mv(list(x = mx, y = my), k = 5, verbose = FALSE)
  ))
  expect_match(attr(tf_components(tb)$x, "basis_label"), "k = 5")
  expect_match(attr(tf_components(tb)$y, "basis_label"), "k = 5")
  expect_equal(
    tb$x,
    suppressWarnings(suppressMessages(tfb(tfd(mx), k = 5, verbose = FALSE)))
  )
  expect_equal(
    tb$y,
    suppressWarnings(suppressMessages(tfb(tfd(my), k = 5, verbose = FALSE)))
  )
})

test_that("tfb_mv.list distributes component-named basis arguments for tf inputs", {
  set.seed(204)
  f <- list(x = tf_rgp(3, arg = 101L), y = tf_rgp(3, arg = 101L))
  tb <- tfb_mv(f, k = list(x = 5, y = 15), verbose = FALSE)
  expect_match(attr(tf_components(tb)$x, "basis_label"), "k = 5")
  expect_match(attr(tf_components(tb)$y, "basis_label"), "k = 15")
  expect_equal(tb$x, tfb(f$x, k = 5, verbose = FALSE))
  expect_equal(tb$y, tfb(f$y, k = 15, verbose = FALSE))
})

test_that("tfb_mv: a non-component-named list is treated as a shared arg, not distributed", {
  f <- tfd_mv(list(x = tf_rgp(2, arg = 51L), y = tf_rgp(2, arg = 51L)))
  # list whose names don't match component names is NOT distributed (treated as
  # a single arg-value; even though mgcv rejects this particular shape, my
  # dispatcher must still treat both components identically -- both end up with
  # the same shared k.)
  tb <- suppressWarnings(suppressMessages(
    tfb_mv(f, k = 6, sp = list(foo = 0, bar = 0), verbose = FALSE)
  ))
  expect_match(attr(tf_components(tb)$x, "basis_label"), "k = 6")
  expect_match(attr(tf_components(tb)$y, "basis_label"), "k = 6")
})

test_that("mixed regular/irregular components work across the API", {
  set.seed(101)
  reg <- tf_rgp(3)
  irr <- tf_sparsify(tf_rgp(3))
  f <- tfd_mv(list(x = reg, y = irr))
  expect_s3_class(f, "tfd_mv")
  expect_true(is_reg(tf_components(f)$x))
  expect_true(is_irreg(tf_components(f)$y))
  # tf_arg returns a per-component list (vec for reg, list-of-vecs for irreg)
  a <- tf_arg(f)
  expect_named(a, c("x", "y"))
  expect_true(is.numeric(a$x))
  expect_true(is.list(a$y))
  # tf_evaluations[[i]] is a per-curve data.frame (arg, x, y) on the union grid;
  # NA-filled where a component lacks a native observation at that arg.
  ev <- tf_evaluations(f)[[1]]
  expect_s3_class(ev, "data.frame")
  expect_named(ev, c("arg", "x", "y"))
  expect_true(anyNA(ev$x) || anyNA(ev$y))
  # tf_count is n x d
  expect_identical(dim(tf_count(f)), c(3L, 2L))
  # subset preserves component classes
  g <- f[2:3]
  expect_true(is_reg(g$x) && is_irreg(g$y))
  # arithmetic preserves component classes
  s <- f + f
  expect_true(is_reg(s$x) && is_irreg(s$y))
  # c() preserves component classes
  cc <- c(f, f)
  expect_length(cc, 6L)
  expect_true(is_reg(cc$x) && is_irreg(cc$y))
  # as.data.frame(unnest = TRUE, long = FALSE) full-outer-joins on (id, arg)
  # so that rows where only the regular component has a value get NAs in the
  # irregular column (this used to error with a row-count mismatch).
  df <- as.data.frame(f, unnest = TRUE, long = FALSE)
  expect_named(df, c("id", "arg", "x", "y"))
  expect_true(anyNA(df$y)) # irregular y is NA at most reg-grid points
  expect_false(anyNA(df$x)) # regular x is observed everywhere
})

test_that("tf_component<- can add a new component and rejects mismatched length", {
  set.seed(25)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  f2 <- f
  tf_component(f2, "z") <- tf_rgp(3)
  expect_identical(tf_ncomp(f2), 3L)
  expect_named(tf_components(f2), c("x", "y", "z"))
  expect_error(tf_component(f, "x") <- tf_rgp(4), "length")
})

test_that("tf_mv constructors preserve and require compatible curve names", {
  x <- tf_rgp(2)
  y <- tf_rgp(2)
  names(x) <- c("a", "b")
  names(y) <- c("a", "b")
  f <- tfd_mv(list(x = x, y = y))
  expect_identical(names(f), c("a", "b"))

  names(y) <- c("c", "d")
  expect_error(tfd_mv(list(x = x, y = y)), "identical curve names")
})

test_that("tf_component() uses exact component names", {
  f <- tfd_mv(list(xpos = tf_rgp(2), ypos = tf_rgp(2)))
  expect_error(tf_component(f, "x"), "Unknown component")
  expect_s3_class(tf_component(f, "xpos"), "tfd")
})

test_that("tf_component validates the index/name selector", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(tf_component(f, 5), "between 1 and 2")
  expect_error(tf_component(f, 0), "between 1 and 2")
  expect_error(tf_component(f, 1.5), "between 1 and 2")
  expect_error(tf_component(f, c(1, 2)), "single component")
  expect_error(tf_component(42, 1), "tf_mv")
  # valid selectors still work
  expect_s3_class(tf_component(f, 2L), "tfd")
  expect_s3_class(tf_component(f, "y"), "tfd")
})

test_that("tf_component<- validates the index but still appends new names", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(
    {
      f2 <- f
      tf_component(f2, 7) <- tf_rgp(2)
    },
    "between 1 and 2"
  )
  # appending a brand-new component by name remains allowed
  f3 <- f
  tf_component(f3, "z") <- tf_rgp(2)
  expect_named(tf_components(f3), c("x", "y", "z"))
})

test_that("tf_estimate_warps validates max_iter, tol and ref_component", {
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_error(tf_estimate_warps(f, max_iter = -1), "max_iter")
  expect_error(tf_estimate_warps(f, tol = -0.1), "tol")
  expect_error(tf_estimate_warps(f, ref_component = 9), "between 1 and 2")
})

test_that("tf_component<- rejects multi-length and empty selectors cleanly", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  # multi-length character selector must not hit the `&&` length>1 crash
  expect_error(
    {
      f2 <- f
      tf_component(f2, c("a", "b")) <- tf_rgp(2)
    },
    "length 1"
  )
  # selecting from a component-less object gives a clear message
  e <- tfd_mv(list())
  expect_error(tf_component(e, 1), "no components")
})

test_that("tf_inner.tf_mv rejects a non-tf_mv second argument informatively", {
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(tf_inner(f, tf_rgp(2)), "tf_mv")
  expect_error(tf_inner(f, 1:2), "tf_mv")
})

# ---- bracket seams: NA index, broadcast, and var() y handling ----------------

test_that("[<-.tf_mv rejects a univariate tf and other non-NA scalars (#244)", {
  set.seed(244)
  g <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  # broadcasting a univariate tf would silently corrupt every component:
  expect_error(g[1] <- tf_rgp(1), "univariate tf")
  # bare numeric scalars are not a valid "all components NA" sentinel either
  expect_error(g[1] <- 0, "univariate tf")
  # NA still works (the documented broadcast use-case)
  g_na <- g
  g_na[1] <- NA
  expect_true(is.na(g_na$x[1]))
  expect_true(is.na(g_na$y[1]))
})

test_that("var.tf and var.tf_mv error on a non-NULL y (#245)", {
  set.seed(245)
  f <- tf_rgp(5)
  g <- tf_rgp(5)
  expect_error(var(f, g), "tf_crosscov|tf_crosscor")
  fm <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  gm <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  expect_error(var(fm, gm), "tf_crosscov|tf_crosscor")
  # var(x) without y still works
  expect_s3_class(var(f), "tf")
  expect_s3_class(var(fm), "tf_mv")
})
