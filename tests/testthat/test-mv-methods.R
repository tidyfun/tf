test_that("bracket evaluation returns a [curve, arg, component] array", {
  set.seed(1)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  arr <- f[1:3, c(0.2, 0.5, 0.8)]
  expect_identical(dim(arr), c(3L, 3L, 2L))
  expect_identical(dimnames(arr)[[3]], c("x", "y"))
  # consistent with the univariate component bracket
  expect_equal(
    arr[,, "x"],
    unclass(f$x[1:3, c(0.2, 0.5, 0.8)]),
    ignore_attr = TRUE
  )
})

test_that("matrix-index extraction returns one row per (function, arg) pair", {
  set.seed(2)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  idx <- cbind(1:3, c(0, 0.5, 1))
  out <- f[idx]
  expect_true(is.matrix(out))
  expect_identical(dim(out), c(3L, 2L))
  expect_identical(colnames(out), c("x", "y"))
  expect_equal(out[, "x"], f$x[idx], ignore_attr = TRUE)
  expect_equal(out[, "y"], f$y[idx], ignore_attr = TRUE)
})

test_that("component= drops to the univariate result", {
  set.seed(3)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  m <- f[1:2, c(0.1, 0.9), component = "x"]
  expect_true(is.matrix(m))
  expect_identical(dim(m), c(2L, 2L))
  expect_equal(m, f$x[1:2, c(0.1, 0.9)], ignore_attr = TRUE)
})

test_that("component= with multiple names returns a sub-tf_mv", {
  set.seed(31)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3), z = tf_rgp(3)))
  sub <- f[,, c("x", "y")]
  expect_s3_class(sub, "tfd_mv")
  expect_identical(tf_ncomp(sub), 2L)
  expect_identical(names(tf_components(sub)), c("x", "y"))
  expect_length(sub, length(f))
  expect_equal(tf_component(sub, "x"), f$x)
  expect_equal(tf_component(sub, "y"), f$y)
})

test_that("component= with multiple indices and curve subset returns sub-tf_mv", {
  set.seed(32)
  f <- tfd_mv(list(a = tf_rgp(4), b = tf_rgp(4), c = tf_rgp(4)))
  sub <- f[2:3, , c(1L, 3L)]
  expect_s3_class(sub, "tfd_mv")
  expect_identical(tf_ncomp(sub), 2L)
  expect_identical(names(tf_components(sub)), c("a", "c"))
  expect_length(sub, 2L)
})

test_that("component= with multiple names + j evaluates to a 3-d array", {
  set.seed(33)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3), z = tf_rgp(3)))
  arr <- f[, c(0.1, 0.5, 0.9), c("x", "z")]
  expect_true(is.array(arr))
  expect_identical(dim(arr), c(3L, 3L, 2L))
  expect_identical(dimnames(arr)[[3]], c("x", "z"))
})

test_that("multi-component selection rejects unknown names", {
  set.seed(34)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(f[,, c("x", "not_there")], "Unknown component")
})

test_that("multi-component selection on tfb_mv stays tfb_mv (no refit)", {
  set.seed(35)
  fb <- tfb_mv(
    tfd_mv(list(a = tf_rgp(3), b = tf_rgp(3), c = tf_rgp(3))),
    verbose = FALSE
  )
  sub <- fb[,, c("a", "b")]
  expect_s3_class(sub, "tfb_mv")
  expect_identical(tf_ncomp(sub), 2L)
  expect_equal(tf_component(sub, "a"), fb$a)
})

test_that("matrix = FALSE returns per-curve data.frames", {
  set.seed(4)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  out <- f[1:2, c(0.2, 0.7), matrix = FALSE]
  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out[[1]], c("arg", "x", "y"))
  expect_equal(out[[1]]$x, f$x[1, c(0.2, 0.7), matrix = FALSE][[1]]$value)
  expect_equal(out[[2]]$y, f$y[2, c(0.2, 0.7), matrix = FALSE][[1]]$value)
})

test_that("arithmetic is component-wise", {
  set.seed(5)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  s <- f + f
  expect_s3_class(s, "tfd_mv")
  expect_equal(tf_evaluations(s$x)[[1]], 2 * tf_evaluations(f$x)[[1]])
  d <- f - f
  # tf_evaluations now returns per-curve data.frames (arg, x, y);
  # the component columns should all be ~0.
  for (cdf in tf_evaluations(d)) {
    expect_true(all(abs(cdf$x) < 1e-9))
    expect_true(all(abs(cdf$y) < 1e-9))
  }
  scaled <- 3 * f
  expect_equal(tf_evaluations(scaled$y)[[2]], 3 * tf_evaluations(f$y)[[2]])
})

test_that("Math and Summary group generics are component-wise", {
  set.seed(6)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  el <- exp(log(abs(f) + 1)) - 1
  expect_s3_class(el, "tfd_mv")
  expect_equal(
    tf_evaluations(el$x)[[1]],
    abs(tf_evaluations(f$x)[[1]]),
    tolerance = 1e-6
  )
})

test_that("Summary group generics use all tf_mv inputs", {
  f <- tfd_mv(list(
    x = tfd(matrix(0, nrow = 1, ncol = 3), arg = 1:3),
    y = tfd(matrix(0, nrow = 1, ncol = 3), arg = 1:3)
  ))
  g <- tfd_mv(list(
    x = tfd(matrix(10, nrow = 1, ncol = 3), arg = 1:3),
    y = tfd(matrix(10, nrow = 1, ncol = 3), arg = 1:3)
  ))
  expect_equal(as.matrix(max(f, g)), as.matrix(g))
})

test_that("mean / median return a length-1 tf_mv", {
  set.seed(7)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  m <- mean(f)
  expect_s3_class(m, "tfd_mv")
  expect_length(m, 1)
  expect_equal(tf_evaluations(m$x)[[1]], tf_evaluations(mean(f$x))[[1]])
  # median.tf_mv is now a *joint* depth-median: one observed curve across all
  # components (not the old component-wise chimera). See #273.
  med <- suppressMessages(median(f))
  expect_length(med, 1)
  expect_identical(unname(med), unname(f[which.max(tf_depth(f))]))
  expect_true(any(vapply(
    seq_along(f),
    function(i) identical(unname(med), unname(f[i])),
    logical(1)
  )))
})

test_that("sd.tf_mv and var.tf_mv accept na.rm", {
  f <- suppressWarnings(tfd_mv(list(
    x = tfd(rbind(c(1, 2, 3), c(NA, NA, NA), c(4, 5, 6)), arg = 1:3),
    y = tfd(rbind(c(2, 3, 4), c(NA, NA, NA), c(5, 6, 7)), arg = 1:3)
  )))
  fsd <- sd(f, na.rm = TRUE)
  fvar <- var(f, na.rm = TRUE)
  complete <- f[!is.na(f)]
  expect_equal(fsd, sd(complete))
  expect_equal(fvar, var(complete))
})

test_that("equality is component-wise", {
  set.seed(8)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_true(all(f == f))
  expect_false(any(f != f))
})

test_that("rev.tf_mv reverses the vector", {
  set.seed(81)
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  names(f) <- c("a", "b", "c")
  out <- rev(f)
  expect_s3_class(out, "tfd_mv")
  expect_identical(names(out), c("c", "b", "a"))
  expect_equal(out$x, rev(f$x))
  expect_equal(out$y, rev(f$y))
})

test_that("as.matrix returns a [curve, arg, component] array", {
  set.seed(9)
  f <- tfd_mv(list(x = tf_rgp(3, arg = 11L), y = tf_rgp(3, arg = 11L)))
  m <- as.matrix(f)
  expect_identical(dim(m), c(3L, 11L, 2L))
  expect_identical(dimnames(m)[[3]], c("x", "y"))
  expect_equal(m[,, "x"], as.matrix(f$x), ignore_attr = TRUE)
  expect_equal(m[,, "y"], as.matrix(f$y), ignore_attr = TRUE)
})

test_that("plot/lines default to trajectory for d == 2 and facet otherwise", {
  set.seed(7)
  f2 <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  f3 <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3), z = tf_rgp(3)))
  expect_identical(mv_plot_type(NULL, tf_components(f2)), "trajectory")
  expect_identical(mv_plot_type(NULL, tf_components(f3)), "facet")
  # partial matching and explicit override both honoured
  expect_identical(mv_plot_type("traj", tf_components(f3)), "trajectory")
  expect_identical(mv_plot_type("facet", tf_components(f2)), "facet")

  # the plotting calls run without error, including per-curve `col` recycling
  pf <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(pf)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(f2, col = 1:3))
  expect_no_error(lines(f2, col = 1:3))
  expect_no_error(plot(f3))
  expect_error(plot(f3, type = "trajectory"), "requires exactly 2 components")
})

test_that("trajectory plotting handles components on different / irregular grids", {
  # components on different regular grids would give x/y matrices of differing
  # width: the trajectory must evaluate both on a common grid first.
  set.seed(8)
  mixed <- tfd_mv(list(
    x = tfd(matrix(rnorm(2 * 10), 2), arg = seq(0, 1, length.out = 10)),
    y = tfd(matrix(rnorm(2 * 25), 2), arg = seq(0, 1, length.out = 25))
  ))
  expect_warning(
    irr <- tfd_mv(list(
      x = tfd(list(c(0, 0.5, 1)), list(c(1, 2, 3))),
      y = tfd(list(c(0, 0.5, 1)), list(c(4, 5, 6)))
    )),
    "Widening domain"
  )
  pf <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(pf)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(mixed, col = 1:2))
  expect_no_error(lines(mixed, col = 1:2))
  expect_no_error(plot(irr))
})

test_that("points.tf_mv overlays without error in trajectory and facet modes", {
  set.seed(88)
  f2 <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  f3 <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3), z = tf_rgp(3)))
  pf <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(pf)
  on.exit(grDevices::dev.off(), add = TRUE)
  # trajectory (default for d == 2), incl. per-curve col recycling
  plot(f2)
  expect_no_error(points(f2, col = 1:3))
  # facet default for d > 2; points overlay onto current device
  plot(f3)
  expect_no_error(points(f3))
  # mirrors lines.tf_mv(): a trajectory request for d != 2 silently falls back
  # to per-component overlay (only plot.tf_mv() errors on that), no error here
  expect_no_error(points(f3, type = "trajectory"))
  # returns its input invisibly
  expect_identical(points(f2), f2)
})

test_that("quantile.tf_mv returns component-wise pointwise quantiles (oracle)", {
  set.seed(89)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  q <- suppressMessages(quantile(f))
  expect_s3_class(q, "tf_mv")
  expect_identical(tf_ncomp(q), 2L)
  # one curve per probability level; default probs has length 5
  expect_length(q, 5L)
  # the established oracle: mv result == univariate result per component
  expect_equal(q$x, suppressMessages(quantile(f$x)))
  expect_equal(q$y, suppressMessages(quantile(f$y)))
})

test_that("quantile.tf_mv honours a probs vector of length > 1", {
  set.seed(90)
  f <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
  probs <- c(0.1, 0.5, 0.9)
  q <- suppressMessages(quantile(f, probs = probs))
  expect_length(q, length(probs))
  expect_equal(q$x, suppressMessages(quantile(f$x, probs = probs)))
})

test_that("quantile.tf_mv passes na.rm through per component", {
  set.seed(91)
  f <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  f$x[2] <- NA
  # with na.rm = TRUE the NA curve is dropped from the pointwise quantiles
  q <- suppressMessages(quantile(f, na.rm = TRUE))
  expect_s3_class(q, "tf_mv")
  expect_equal(q$x, suppressMessages(quantile(f$x, na.rm = TRUE)))
  expect_equal(q$y, suppressMessages(quantile(f$y, na.rm = TRUE)))
})

test_that("print reports per-component grid / interpolator / basis info", {
  set.seed(9)
  # shared grid -> collapsed single info line
  f <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_output(print(f), "components based on \\d+ evaluations each")
  expect_output(print(f), "interpolation by tf_approx_linear")

  # different grids -> per-component lines
  mixed <- tfd_mv(list(
    x = tfd(matrix(rnorm(2 * 10), 2), arg = seq(0, 1, length.out = 10)),
    y = tfd(matrix(rnorm(2 * 25), 2), arg = seq(0, 1, length.out = 25))
  ))
  expect_output(print(mixed), "x: based on 10 evaluations each")
  expect_output(print(mixed), "y: based on 25 evaluations each")

  # basis representation reports the basis spec
  tb <- tfb_mv(f, k = 7, verbose = FALSE)
  expect_output(print(tb), "in basis representation")
  expect_output(print(tb), "k = 7")
})
