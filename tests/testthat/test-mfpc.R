test_that("tfb_mfpc returns a tfb_mv with shared-score tfb_fpc components", {
  set.seed(1)
  g <- tfd_mv(list(x = tf_rgp(15), y = tf_rgp(15)))
  m <- tfb_mfpc(g, pve = 0.99)
  expect_s3_class(m, "tfb_mv")
  expect_true(is_tfb_mv(m))
  expect_true(is_tfb_mfpc(m))
  expect_false(is_tfb_mfpc(tfb_mv(g, basis = "fpc", verbose = FALSE)))
  expect_true(all(map_lgl(tf_components(m), is_tfb_fpc)))
  expect_length(m, 15)
  expect_identical(tf_ncomp(m), 2L)
})

test_that("MFPC scores are shared (identical) across all components", {
  set.seed(2)
  g <- tfd_mv(list(x = tf_rgp(12), y = tf_rgp(12), z = tf_rgp(12)))
  m <- tfb_mfpc(g, npc = 4)
  # the per-curve coefficient vectors (1, scores) are identical component-wise
  c1 <- unclass(tf_component(m, 1))
  c2 <- unclass(tf_component(m, 2))
  c3 <- unclass(tf_component(m, 3))
  expect_equal(c1, c2, ignore_attr = TRUE)
  expect_equal(c1, c3, ignore_attr = TRUE)
  S <- tf_mfpc_scores(m)
  expect_equal(dim(S), c(12L, 4L))
})

test_that("reconstruction is exact at full rank with equal weights", {
  set.seed(3)
  g <- tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10)))
  m <- tfb_mfpc(g, weights = "equal", uni_pve = 1, pve = 1)
  rec <- as.tfd_mv(m)
  expect_equal(as.matrix(rec), as.matrix(g), tolerance = 1e-10)
})

test_that("multivariate eigenfunctions are weighted-orthonormal", {
  set.seed(4)
  g <- tfd_mv(list(x = tf_rgp(40, nugget = .05), y = tf_rgp(40, nugget = .05)))
  m <- tfb_mfpc(g, weights = "inverse_variance", npc = 5)
  w <- attr(m, "mfpc")$weights
  ef <- tf_mfpc_efunctions(m) # tfd_mv, length M
  M <- length(ef)
  gram <- matrix(0, M, M)
  for (j in seq_len(tf_ncomp(ef))) {
    mat <- as.matrix(tf_component(ef, j))
    arg <- as.numeric(attr(mat, "arg"))
    qw <- trapezoid_weights(arg)
    gram <- gram + w[j] * (mat %*% (qw * t(mat)))
  }
  expect_equal(unname(gram), diag(M), tolerance = 1e-6)
})

test_that("multivariate eigenvalues account for the total weighted variance", {
  set.seed(5)
  g <- tfd_mv(list(x = tf_rgp(30, nugget = .05), y = tf_rgp(30, nugget = .05)))
  m <- tfb_mfpc(g, weights = "equal", uni_pve = 1, pve = 1)
  nu <- attr(m, "mfpc")$evalues
  w <- attr(m, "mfpc")$weights
  ind <- tfb_mv(g, basis = "fpc", pve = 1, verbose = FALSE)
  tv <- map_dbl(tf_components(ind), \(co) sum(attr(co, "score_variance")))
  expect_equal(sum(nu), sum(w * tv), tolerance = 1e-6)
  # eigenvalues are non-increasing and non-negative
  expect_true(all(nu >= -1e-8))
  expect_false(is.unsorted(rev(nu)))
})

test_that("weight schemes and user weights produce valid, normalized weights", {
  set.seed(6)
  g <- tfd_mv(list(x = tf_rgp(25), y = tf_rgp(25)))
  m_iv <- tfb_mfpc(g, weights = "inverse_variance", npc = 3)
  m_eq <- tfb_mfpc(g, weights = "equal", npc = 3)
  m_snr <- tfb_mfpc(g, weights = "snr", uni_pve = 0.9, npc = 3)
  m_num <- tfb_mfpc(g, weights = c(3, 1), npc = 3)
  # all schemes rescale to sum to d (= 2)
  for (m in list(m_iv, m_eq, m_snr, m_num)) {
    expect_equal(sum(attr(m, "mfpc")$weights), 2)
  }
  expect_equal(unname(attr(m_eq, "mfpc")$weights), c(1, 1))
  expect_equal(unname(attr(m_num, "mfpc")$weights), c(1.5, 0.5))
  # wrong-length numeric weights error
  expect_error(tfb_mfpc(g, weights = c(1, 2, 3)), "weights")
  # zero / negative weights error (would yield Inf eigenfunctions via 1/sqrt(w))
  expect_error(tfb_mfpc(g, weights = c(1, 0)), "strictly positive")
  expect_error(tfb_mfpc(g, weights = c(2, -1)), "strictly positive")
})

test_that("re-scoring rejects a custom arg and incompatible domains", {
  set.seed(15)
  g <- tfd_mv(list(x = tf_rgp(15), y = tf_rgp(15)))
  m <- tfb_mfpc(g, npc = 3)
  expect_error(tf_rebase(g, m, arg = seq(0, 1, length.out = 10)), "custom")
})

test_that("joint re-scoring round-trips the training data exactly", {
  set.seed(7)
  g <- tfd_mv(list(x = tf_rgp(20), y = tf_rgp(20)))
  m <- tfb_mfpc(g, pve = 0.99)
  S <- tf_mfpc_scores(m)
  # re-scoring the *training* data must recover the stored scores
  m_re <- tf_rebase(g, m)
  expect_true(is_tfb_mfpc(m_re))
  expect_equal(tf_mfpc_scores(m_re), S, tolerance = 1e-8)
  # the same via vec_cast
  m_cast <- vctrs::vec_cast(g, m)
  expect_equal(tf_mfpc_scores(m_cast), S, tolerance = 1e-8)
})

test_that("re-scoring new data is well-defined and component-compatible", {
  set.seed(8)
  g <- tfd_mv(list(x = tf_rgp(30), y = tf_rgp(30)))
  m <- tfb_mfpc(g, npc = 4)
  g_new <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
  proj <- tf_rebase(g_new, m)
  expect_s3_class(proj, "tfb_mv")
  expect_true(is_tfb_mfpc(proj))
  expect_length(proj, 5)
  expect_equal(dim(tf_mfpc_scores(proj)), c(5L, 4L))
  # mismatched component count errors
  g_bad <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5), z = tf_rgp(5)))
  expect_error(tf_rebase(g_bad, m))
})

test_that("uni_pve / per-component args control the univariate truncation", {
  set.seed(9)
  g <- tfd_mv(list(x = tf_rgp(30, nugget = .05), y = tf_rgp(30, nugget = .05)))
  m_lo <- tfb_mfpc(g, uni_pve = 0.5, pve = 1)
  m_hi <- tfb_mfpc(g, uni_pve = 0.99, pve = 1)
  # fewer univariate PCs => fewer available multivariate PCs
  expect_lte(attr(m_lo, "mfpc")$npc, attr(m_hi, "mfpc")$npc)
})

test_that("scoring a single MFPC component directly is an error", {
  set.seed(10)
  g <- tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10)))
  m <- tfb_mfpc(g, npc = 3)
  comp <- tf_component(m, 1)
  expect_s3_class(comp, "tfb_fpc")
  # the stored per-component scoring function refuses to run
  expect_error(
    attr(comp, "scoring_function")(),
    "single component"
  )
})

test_that("slicing and same-fit concatenation preserve the MFPC spec", {
  set.seed(11)
  g <- tfd_mv(list(x = tf_rgp(12), y = tf_rgp(12)))
  m <- tfb_mfpc(g, npc = 3)
  # subsetting keeps the curve-independent eigenbasis -> still re-scorable
  expect_true(is_tfb_mfpc(m[1:5]))
  expect_equal(dim(tf_mfpc_scores(m[1:5])), c(5L, 3L))
  # concatenation of slices of the *same* fit keeps the spec
  expect_true(is_tfb_mfpc(c(m[1:6], m[7:12])))
  expect_true(is_tfb_mfpc(c(m, m)))
})

# Capture warning messages from an expression without aborting on them.
collect_warnings <- function(expr) {
  ws <- character()
  val <- withCallingHandlers(
    expr,
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = ws)
}

test_that("multi-component subsetting preserves MFPC specs when possible", {
  set.seed(111)
  mf <- tfb_mfpc(
    tfd_mv(list(x = tf_rgp(12), y = tf_rgp(12), z = tf_rgp(12))),
    npc = 3
  )

  full <- mf[, component = c("x", "y", "z")]
  expect_true(is_tfb_mfpc(full))
  expect_identical(attr(full, "mfpc")$comp_names, c("x", "y", "z"))
  expect_no_error(tf_rebase(as.tfd_mv(full), full))

  reordered <- mf[, component = c("z", "x", "y")]
  expect_true(is_tfb_mfpc(reordered))
  expect_identical(attr(reordered, "mfpc")$comp_names, c("z", "x", "y"))
  expect_no_error(tf_rebase(as.tfd_mv(reordered), reordered))
  expect_equal(tf_mfpc_scores(reordered), tf_mfpc_scores(mf))

  rescored <- tf_rebase(as.tfd_mv(mf), mf)
  rescored_reordered <- tf_rebase(
    as.tfd_mv(mf)[, component = c("z", "x", "y")],
    reordered
  )
  expect_equal(tf_mfpc_scores(rescored_reordered), tf_mfpc_scores(rescored))

  cap <- collect_warnings(mf[, component = c("x", "z")])
  expect_true(any(grepl("demot|mfpc|MFPC|joint", cap$warnings)))
  expect_false(is_tfb_mfpc(cap$value))
  expect_no_error(suppressWarnings(cap$value + 1))
})

test_that("tf_interpolate preserves usable MFPC specs", {
  set.seed(112)
  mf <- tfb_mfpc(
    tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))),
    npc = 2
  )

  out <- tf_interpolate(mf, arg = seq(0, 1, length.out = 31))

  expect_true(is_tfb_mfpc(out))
  expect_identical(attr(out, "mfpc")$comp_names, attr(mf, "mfpc")$comp_names)
  expect_identical(tf_arg(out$x), seq(0, 1, length.out = 31))
  expect_no_error(tf_rebase(as.tfd_mv(out), out))
})

test_that("tfb_mfpc protects its joint spec", {
  set.seed(1)
  mf <- tfb_mfpc(tfd_mv(list(x = tf_rgp(20), y = tf_rgp(20))), pve = 0.95)
  # 1. Arithmetic demotes with a clear warning
  cap <- collect_warnings(mf + 1)
  expect_true(any(grepl("demot|mfpc|MFPC|joint", cap$warnings)))
  expect_false(is_tfb_mfpc(cap$value))
  # 2. $<- demotes with a clear warning
  cap <- collect_warnings({
    mf2 <- mf
    mf2$x <- mf$x
    mf2
  })
  expect_true(any(grepl("demot|mfpc|MFPC|joint", cap$warnings)))
  expect_false(is_tfb_mfpc(cap$value))
  # 3. c() of slices of the same fit preserves the spec
  expect_true(is_tfb_mfpc(c(mf[1:4], mf[5:8])))
})

test_that("post-demotion tfb_mv stays functional", {
  set.seed(42)
  mf <- tfb_mfpc(tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))), pve = 0.95)
  mf2 <- mf
  suppressWarnings(mf2$x <- mf$x)
  expect_false(is_tfb_mfpc(mf2))
  # evaluating the demoted object must not explode in the per-component
  # scoring stub
  expect_no_error(tf_evaluate(mf2))
  # arithmetic re-scores via the components' scoring_function, so it must not
  # hit the abort stub either -- this exercises the rebuilt standalone bases,
  # including the assigned `value` (which came from the same MFPC fit).
  expect_no_error(suppressWarnings(out <- mf2 + 1))
  expect_true(all(is.finite(unlist(tf_evaluations(out)))))
  expect_no_error(suppressWarnings(tf_rebase(mf2, mf2)))
})

test_that("both-mfpc arithmetic warns about demotion exactly once", {
  set.seed(21)
  mf <- tfb_mfpc(tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))), pve = 0.95) |>
    suppressWarnings()
  n_demotion_warnings <- 0L
  withCallingHandlers(
    mf + mf,
    tf_mfpc_demotion = function(w) {
      n_demotion_warnings <<- n_demotion_warnings + 1L
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_identical(n_demotion_warnings, 1L)
})

test_that("demoted tfb_mfpc supports chained Math/Ops", {
  set.seed(1)
  mf <- tfb_mfpc(tfd_mv(list(x = tf_rgp(20), y = tf_rgp(20))), pve = 0.95) |>
    suppressWarnings()
  expect_no_error(out1 <- suppressWarnings((mf + 1) - 1))
  expect_no_error(out2 <- suppressWarnings(log(mf + 2)))
  expect_no_error(out3 <- suppressWarnings(-mf))
  expect_no_error(out4 <- suppressWarnings(mf * 2 / 2))
  # reconstructed values are sane (finite) after a round-trip through ops
  expect_true(all(is.finite(unlist(tf_evaluations(out1)))))
})

test_that("demotion warning has class 'tf_mfpc_demotion'", {
  set.seed(8)
  mf <- tfb_mfpc(tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))), pve = 0.95) |>
    suppressWarnings()
  classes <- character()
  withCallingHandlers(
    mf + 1,
    tf_mfpc_demotion = function(w) {
      classes <<- c(classes, class(w))
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_true("tf_mfpc_demotion" %in% classes)
})

test_that("c() of MFPC fits with different specs demotes with a warning", {
  set.seed(1)
  mf1 <- tfb_mfpc(tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))), pve = 0.95)
  set.seed(2)
  mf2 <- tfb_mfpc(tfd_mv(list(x = tf_rgp(10), y = tf_rgp(10))), pve = 0.95)
  cap <- collect_warnings(c(mf1, mf2))
  expect_true(any(grepl("demot|mfpc|MFPC|joint", cap$warnings)))
  expect_false(is_tfb_mfpc(cap$value))
})

test_that("mixing a tfd_mv with an MFPC tfb_mv demotes (no spec carried)", {
  set.seed(12)
  g <- tfd_mv(list(x = tf_rgp(8), y = tf_rgp(8)))
  m <- tfb_mfpc(g, npc = 2)
  expect_false(is_tfb_mfpc(c(as.tfd_mv(m), g)))
})

test_that("npc beyond available components warns and is capped", {
  set.seed(13)
  g <- tfd_mv(list(x = tf_rgp(6), y = tf_rgp(6)))
  expect_warning(m <- tfb_mfpc(g, uni_pve = 0.5, npc = 1000), "exceeds")
  expect_lte(attr(m, "mfpc")$npc, length(g) * 2)
})

test_that("empty / degenerate inputs error informatively", {
  expect_error(tfb_mfpc(tfd_mv(list())), "no components")
})

test_that("tfb_mfpc aborts informatively on completely missing curves", {
  set.seed(21)
  g <- tfd_mv(list(x = tf_rgp(8), y = tf_rgp(8)))
  suppressWarnings(g[3] <- NA)
  expect_error(tfb_mfpc(g, npc = 2), "completely missing")
})

test_that("tfb_mfpc accepts raw list input with constructor arguments", {
  set.seed(22)
  t_grid <- seq(0, 1, length.out = 21)
  mx <- matrix(rnorm(6 * 21), nrow = 6)
  my <- matrix(rnorm(6 * 21), nrow = 6)
  m <- tfb_mfpc(list(x = mx, y = my), arg = t_grid, npc = 2)
  expect_true(is_tfb_mfpc(m))
  expect_length(m, 6L)
  expect_equal(tf_arg(m), t_grid)
})

test_that("re-scoring new data with NA curves yields NA scores, not zeros", {
  set.seed(23)
  g <- tfd_mv(list(x = tf_rgp(8), y = tf_rgp(8)))
  m <- tfb_mfpc(g, npc = 2)
  newdata <- g[1:3]
  suppressWarnings(newdata[2] <- NA)
  r <- tf_rebase(newdata, m)
  expect_length(r, 3L)
  expect_identical(unname(is.na(r)), c(FALSE, TRUE, FALSE))
  s <- tf_mfpc_scores(r)
  expect_true(all(is.na(s[2, ])))
  expect_false(anyNA(s[c(1, 3), ]))
})
