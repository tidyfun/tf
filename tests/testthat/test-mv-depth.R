# Multivariate (tf_mv) depth, median, summaries and ordering -- see #273.
# Design: centrality depths generalize as a weighted componentwise average;
# the up-down ordering index (MHI) does not; median/summary/fivenum select
# *observed* curves via the joint depth (one index across components); and
# sort/rank/xtfrm stay permanently undefined (no total order on R^d).

make_fm <- function(n = 7, seed = 273) {
  set.seed(seed)
  tfd_mv(list(x = tf_rgp(n), y = tf_rgp(n), z = tf_rgp(n)))
}

test_that("tf_depth.tf_mv equals the weighted componentwise average", {
  fm <- make_fm()
  per_comp <- vapply(
    tf_components(fm),
    function(comp) tf_depth(comp, depth = "MBD"),
    numeric(length(fm))
  )
  # equal weights
  expect_equal(
    unname(tf_depth(fm)),
    unname(rowMeans(per_comp))
  )
  # numeric weights (normalized to sum 1)
  w <- c(1, 2, 3)
  expect_equal(
    unname(tf_depth(fm, weights = w)),
    unname(as.numeric(per_comp %*% (w / sum(w))))
  )
  # inverse_variance: inverse of per-component mean pointwise variance
  mpv <- vapply(
    tf_components(fm),
    function(comp) mean(apply(as.matrix(comp), 2, var)),
    numeric(1)
  )
  wiv <- (1 / mpv) / sum(1 / mpv)
  expect_equal(
    unname(tf_depth(fm, weights = "inverse_variance")),
    unname(as.numeric(per_comp %*% wiv))
  )
})

test_that("tf_depth.tf_mv carries curve names and respects a non-default depth", {
  fm <- make_fm()
  names(fm) <- paste0("c", seq_along(fm))
  d <- tf_depth(fm, depth = "FM")
  expect_named(d, names(fm))
  per_comp <- vapply(
    tf_components(fm),
    function(comp) tf_depth(comp, depth = "FM"),
    numeric(length(fm))
  )
  expect_equal(unname(d), unname(rowMeans(per_comp)))
})

test_that("tf_depth.tf_mv weights validation errors", {
  fm <- make_fm()
  expect_error(tf_depth(fm, weights = c(1, 2)), "length") # wrong length (d = 3)
  expect_error(tf_depth(fm, weights = c(1, 2, -1)), "strictly positive")
  expect_error(tf_depth(fm, weights = "bogus"))
})

test_that("depth = 'MHI' aborts with the ordering-index message", {
  fm <- make_fm()
  expect_error(tf_depth(fm, depth = "MHI"), "ordering index")
  # the message should point users at tf_order
  expect_error(tf_depth(fm, depth = "MHI"), "tf_order")
})

test_that("median.tf_mv returns an OBSERVED curve (no chimera)", {
  fm <- make_fm()
  med <- suppressMessages(median(fm))
  expect_length(med, 1L)
  expect_true(is_tf_mv(med))
  # the regression assertion from #273: the joint median is one of the inputs
  expect_true(any(vapply(
    seq_along(fm),
    function(i) identical(unname(med), unname(fm[i])),
    logical(1)
  )))
})

test_that("median.tf_mv index == which.max(tf_depth)", {
  fm <- make_fm()
  med <- suppressMessages(median(fm))
  expect_identical(unname(med), unname(fm[which.max(tf_depth(fm))]))
})

test_that("median.tf_mv NA handling mirrors univariate", {
  fm <- make_fm(n = 5)
  fm$x[2] <- NA
  # na.rm = FALSE -> a single NA-valued curve
  mna <- median(fm)
  expect_length(mna, 1L)
  expect_true(all(is.na(mna)))
  # na.rm = TRUE -> observed curve among the complete ones
  med <- suppressMessages(median(fm, na.rm = TRUE))
  complete <- fm[!is.na(fm)]
  expect_true(any(vapply(
    seq_along(complete),
    function(i) identical(unname(med), unname(complete[i])),
    logical(1)
  )))
})

test_that("summary.tf_mv runs clean and satisfies its oracles", {
  fm <- make_fm()
  s <- summary(fm)
  expect_s3_class(s, "tf_mv")
  expect_named(
    s,
    c("min", "lower_mid", "median", "mean", "upper_mid", "max")
  )
  # pointwise entries agree with the componentwise pointwise summaries
  expect_equal(unname(s["mean"]$x), unname(mean(fm)$x))
  expect_equal(unname(s["min"]$x), unname(min(fm)$x))
  expect_equal(unname(s["max"]$y), unname(max(fm)$y))
  # the depth-based median entry is an observed curve
  me <- s["median"]
  expect_true(any(vapply(
    seq_along(fm),
    function(i) identical(unname(me), unname(fm[i])),
    logical(1)
  )))
})

test_that("fivenum.tf_mv runs clean and returns observed curves", {
  fm <- make_fm()
  fv <- suppressMessages(fivenum(fm))
  expect_s3_class(fv, "tf_mv")
  expect_length(fv, 5L)
  expect_named(
    fv,
    c("min", "lower_hinge", "median", "upper_hinge", "max")
  )
  # every five-number curve is one of the observed input curves
  observed <- function(g) {
    any(vapply(
      seq_along(fm),
      function(i) identical(unname(g), unname(fm[i])),
      logical(1)
    ))
  }
  expect_true(all(vapply(seq_len(5), function(k) observed(fv[k]), logical(1))))
})

test_that("tf_order.tf_mv reduces via norm or a named component", {
  fm <- make_fm()
  expect_identical(tf_order(fm, by = "norm"), tf_order(tf_norm(fm)))
  expect_identical(tf_order(fm, by = "x"), tf_order(fm$x))
  expect_identical(tf_order(fm, by = "z"), tf_order(fm$z))
  # default is "norm"
  expect_identical(tf_order(fm), tf_order(fm, by = "norm"))
})

test_that("tf_order.tf_mv errors on an invalid `by`", {
  fm <- make_fm()
  expect_error(tf_order(fm, by = "nope"), "component name")
  expect_error(tf_order(fm, by = 1L), "component name")
})

test_that("sort / rank / xtfrm stay permanently undefined for tf_mv", {
  fm <- make_fm()
  expect_error(sort(fm), class = "tf_mv_method_unimplemented")
  expect_error(rank(fm), class = "tf_mv_method_unimplemented")
  expect_error(xtfrm(fm), class = "tf_mv_method_unimplemented")
  # base order()/sort() route through xtfrm and must also fail fast
  expect_error(order(fm), class = "tf_mv_method_unimplemented")
})

test_that("d = 1 tf_mv depth equals the univariate depth of its component", {
  set.seed(11)
  u <- tf_rgp(8)
  fm1 <- tfd_mv(list(only = u))
  expect_equal(unname(tf_depth(fm1)), unname(tf_depth(u)))
  expect_equal(
    unname(tf_depth(fm1, depth = "FSD")),
    unname(tf_depth(u, depth = "FSD"))
  )
})

test_that("tf_depth / median / summary work on tfb_mv too", {
  fm <- make_fm(n = 6)
  fb <- suppressWarnings(tfb_mv(fm, k = 5, penalized = FALSE, verbose = FALSE))
  expect_type(tf_depth(fb), "double")
  expect_length(tf_depth(fb), length(fb))
  med <- suppressMessages(median(fb))
  expect_true(is_tf_mv(med))
  expect_length(med, 1L)
})
