# Contract regressions: tf vs. tf_mv inheritance + coercion shape stability.
# These pin the behaviour the multivariate audit (June 2026 integration into
# tidyfun's ggplot layer) flagged as wrong-or-inconsistent.

# --- is_tf_1d() distinguishes univariate from multivariate -------------------

test_that("is_tf_1d() is TRUE for tfd/tfb and FALSE for tfd_mv/tfb_mv", {
  set.seed(1)
  fu <- tf_rgp(2)
  fb <- tfb(fu, k = 4, penalized = FALSE, verbose = FALSE)
  fm <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  fmb <- tfb_mv(fm, verbose = FALSE)
  expect_true(is_tf_1d(fu))
  expect_true(is_tf_1d(fb))
  expect_false(is_tf_1d(fm))
  expect_false(is_tf_1d(fmb))
  # raw scalars / data.frames are not tf
  expect_false(is_tf_1d(1:5))
  expect_false(is_tf_1d(data.frame(a = 1)))
})

# --- Summary on tf_mv dispatches to the multivariate method ------------------

test_that("Summary() / max() on a tf_mv dispatches to the tf_mv method", {
  set.seed(2)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  # Summary.tf_mv returns a tf_mv (length 1) -- the univariate path would
  # silently flatten to a single tfd, losing component structure.
  out <- max(fm)
  expect_s3_class(out, "tf_mv")
  expect_identical(tf_ncomp(out), 2L)
})

# --- tf_2_df delegates to .tf_mv path when given a tf_mv ---------------------

test_that("tf_2_df on a tf_mv delegates to as.data.frame.tf_mv", {
  set.seed(3)
  fm <- tfd_mv(list(x = tf_rgp(2, arg = 4L), y = tf_rgp(2, arg = 4L)))
  df <- tf_2_df(fm)
  # tf_2_df now returns the mv-aware long form: (id, arg, component, value)
  expect_named(df, c("id", "arg", "component", "value"))
  expect_s3_class(df$component, "factor")
  expect_identical(levels(df$component), c("x", "y"))
})

# --- as.data.frame.tf_mv long/wide + arg + interpolate -----------------------

test_that("as.data.frame.tf_mv defaults to long (id, arg, component, value)", {
  set.seed(4)
  fm <- tfd_mv(list(x = tf_rgp(2, arg = 4L), y = tf_rgp(2, arg = 4L)))
  d_long <- as.data.frame(fm, unnest = TRUE)
  expect_named(d_long, c("id", "arg", "component", "value"))
  expect_s3_class(d_long$component, "factor")
  expect_identical(levels(d_long$component), c("x", "y"))
  expect_identical(nrow(d_long), 2L * 4L * 2L)
  # opt-in wide schema
  d_wide <- as.data.frame(fm, unnest = TRUE, long = FALSE)
  expect_named(d_wide, c("id", "arg", "x", "y"))
  expect_identical(nrow(d_wide), 2L * 4L)
})

test_that("as.data.frame.tf_mv accepts a custom arg grid", {
  set.seed(5)
  fm <- tfd_mv(list(x = tf_rgp(2, arg = 11L), y = tf_rgp(2, arg = 11L)))
  arg <- c(0.1, 0.5, 0.9)
  d_long <- as.data.frame(fm, unnest = TRUE, arg = arg)
  # 2 curves * 3 arg * 2 components
  expect_identical(nrow(d_long), 2L * 3L * 2L)
  expect_setequal(unique(d_long$arg), arg)
  d_wide <- as.data.frame(fm, unnest = TRUE, long = FALSE, arg = arg)
  expect_identical(nrow(d_wide), 2L * 3L)
  expect_setequal(unique(d_wide$arg), arg)
})

test_that("as.data.frame.tf_mv on a zero-component tf_mv returns an empty df", {
  empty <- tfd_mv(list())
  d_long <- as.data.frame(empty, unnest = TRUE)
  expect_named(d_long, c("id", "arg", "component", "value"))
  expect_identical(nrow(d_long), 0L)
  d_wide <- as.data.frame(empty, unnest = TRUE, long = FALSE)
  expect_named(d_wide, c("id", "arg"))
  expect_identical(nrow(d_wide), 0L)
})

test_that("single component extraction without arg preserves tf class", {
  set.seed(51)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  out <- fm[, component = "x"]
  expect_s3_class(out, "tfd")
  expect_false(is_tf_mv(out))
  expect_equal(out, tf_component(fm, "x"))

  out_i <- fm[1:2, component = 1]
  expect_s3_class(out_i, "tfd")
  expect_equal(out_i, tf_component(fm, 1)[1:2])

  mat <- fm[, c(0.25, 0.5), component = "x"]
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(3L, 2L))

  fb <- tfb_mv(fm, k = 5, penalized = FALSE, verbose = FALSE)
  expect_s3_class(fb[, component = "x"], "tfb")
})

# --- tf_evaluate.tf_mv uniform per-curve data.frame --------------------------

test_that("tf_evaluate(<tf_mv>) returns uniform list-of-data.frames", {
  set.seed(6)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  out <- tf_evaluate(fm, arg = c(0.1, 0.5, 0.9))
  expect_length(out, 3L)
  for (df in out) {
    expect_s3_class(df, "data.frame")
    expect_identical(colnames(df), c("arg", "x", "y"))
    expect_identical(nrow(df), 3L)
  }
})

test_that("tf_evaluate(<mixed-grid tf_mv>) returns data.frames (no list-shape)", {
  set.seed(7)
  fx <- tf_rgp(2, arg = 5L)
  fy <- tf_jiggle(tf_rgp(2, arg = 7L)) # irregular
  fm <- tfd_mv(list(x = fx, y = fy))
  out <- tf_evaluate(fm) # native (union) grids per curve
  for (df in out) {
    expect_s3_class(df, "data.frame")
    expect_identical(colnames(df), c("arg", "x", "y"))
  }
})

# --- as.matrix.tf_mv stays 3-d -----------------------------------------------

test_that("as.matrix.tf_mv returns a 3-d array with named third dim", {
  set.seed(8)
  fm <- tfd_mv(list(x = tf_rgp(3, arg = 5L), y = tf_rgp(3, arg = 5L)))
  m <- as.matrix(fm, arg = seq(0, 1, length.out = 5), interpolate = TRUE)
  expect_identical(length(dim(m)), 3L)
  expect_identical(dim(m), c(3L, 5L, 2L))
  expect_identical(dimnames(m)[[3]], c("x", "y"))
})

test_that("as.matrix.tf_mv treats arg NULL like a missing arg", {
  set.seed(81)
  fm <- tfd_mv(list(x = tf_rgp(2, arg = 5L), y = tf_rgp(2, arg = 5L)))
  expect_equal(as.matrix(fm, arg = NULL), as.matrix(fm))
})

# --- mat_2_df id column is plain factor, not ordered -------------------------

test_that("as.data.frame on a tfd/tfb yields a plain-factor id column", {
  set.seed(9)
  f <- tf_rgp(3)
  d <- as.data.frame(f, unnest = TRUE)
  expect_s3_class(d$id, "factor")
  expect_false(inherits(d$id, "ordered"))
})

# --- tf_norm rebases components onto a paired grid before pointwise norm -----

test_that("tf_norm on components with different argument grids works", {
  # x evaluated at c(0, 1), y at c(0.5, 1.5); the univariate `+` path would
  # have errored or misaligned. The new norm path evaluates on each curve's
  # union grid and NA-fills outside each component's support.
  fx <- tfd(matrix(c(3, 3), nrow = 1), arg = c(0, 1))
  fy <- tfd(matrix(c(4, 4), nrow = 1), arg = c(0.5, 1.5))
  f <- tfd_mv(list(x = fx, y = fy), domain = c(0, 1.5))
  n <- tf_norm(f)
  expect_s3_class(n, "tfd")
  # at arg = 0.5, both components are well-defined (3, 4) -> norm = 5
  expect_equal(unlist(tf_evaluate(n, arg = 0.5)), 5, ignore_attr = TRUE)
})

# --- tf_integrate on zero-component tf_mv returns a shape-appropriate empty --

test_that("tf_integrate on a zero-component tf_mv returns an empty result", {
  empty <- tfd_mv(list())
  out <- tf_integrate(empty)
  expect_true(is.matrix(out))
  expect_identical(dim(out), c(0L, 0L))
})

# --- Fail-fast contract for un-stubbed generics ------------------------------
# For each S3 generic in NAMESPACE that has a univariate-tf method (tf / tfd /
# tfb / tfd_reg / tfd_irreg / tfb_spline / tfb_fpc), calling it on a `tf_mv`
# must either succeed (an explicit tf_mv method exists) or abort with the
# classed `tf_mv_method_unimplemented` condition -- never an obscure deep
# internal error, never NA garbage. See tidyfun/tf#255.

# small probe set with explicit invocation recipes for verbs whose signatures
# need a second argument or otherwise resist a generic walker call.
mv_probe_calls <- function(fm) {
  list(
    summary = function() summary(fm),
    fivenum = function() fivenum(fm),
    tf_depth = function() tf_depth(fm),
    tf_crosscov = function() tf_crosscov(fm, fm),
    tf_crosscor = function() tf_crosscor(fm, fm),
    tf_invert = function() tf_invert(fm),
    sort = function() sort(fm),
    rank = function() rank(fm),
    xtfrm = function() xtfrm(fm)
  )
}

test_that("explicitly probed unimplemented verbs abort with classed condition", {
  set.seed(255)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  probes <- mv_probe_calls(fm)
  for (nm in names(probes)) {
    expect_error(
      probes[[nm]](),
      class = "tf_mv_method_unimplemented",
      info = sprintf(
        "%s() on tf_mv should signal tf_mv_method_unimplemented",
        nm
      )
    )
  }
})

test_that("quantile() on a tf_mv now succeeds (implemented component-wise)", {
  set.seed(2551)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
  expect_no_condition(
    suppressMessages(quantile(fm)),
    class = "tf_mv_method_unimplemented"
  )
})

test_that("every univariate-tf generic either has a tf_mv method or aborts cleanly", {
  set.seed(2550)
  fm <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))

  # parse NAMESPACE: for each generic, collect the set of classes it's
  # registered for. For an installed package (R CMD check, test_dir() on a
  # library install) NAMESPACE sits at the package root; under
  # devtools::load_all() the pkgload shim resolves the same call to the
  # source-tree NAMESPACE.
  ns_path <- system.file("NAMESPACE", package = "tf")
  if (!nzchar(ns_path)) {
    # not installed at all: fall back to the source-tree NAMESPACE.
    ns_path <- testthat::test_path("..", "..", "NAMESPACE")
  }
  ns_lines <- readLines(ns_path)
  expect_true(any(grepl("^S3method\\(", ns_lines)))
  s3 <- grep("^S3method\\(", ns_lines, value = TRUE)
  m <- regmatches(s3, regexec("^S3method\\(([^,]+),(.+)\\)$", s3))
  pairs <- do.call(
    rbind.data.frame,
    lapply(m, function(x) {
      if (length(x) == 3) {
        data.frame(
          generic = gsub('"', "", trimws(x[2])),
          class = gsub('"', "", trimws(x[3])),
          stringsAsFactors = FALSE
        )
      }
    })
  )
  by_gen <- split(pairs$class, pairs$generic)

  univariate_classes <- c(
    "tf",
    "tfd",
    "tfb",
    "tfd_reg",
    "tfd_irreg",
    "tfb_spline",
    "tfb_fpc"
  )

  has_univariate <- function(classes) any(classes %in% univariate_classes)
  has_tf_mv <- function(classes) "tf_mv" %in% classes

  # restrict to single-arg generics we can blindly call; everything else is
  # already covered by the explicit probe table above.
  walkable <- c(
    "format",
    "print",
    "plot",
    "lines",
    "tf_arg",
    "tf_domain",
    "tf_evaluations",
    "tf_count",
    "as.data.frame",
    "as.matrix",
    "mean",
    "median",
    "sd",
    "var",
    "is.na",
    "tf_derive",
    "tf_integrate",
    "tf_inner",
    "tf_norm",
    "tf_tangent",
    "tf_smooth",
    "tf_evaluate",
    "tf_arclength",
    "tf_rebase",
    "tf_zoom",
    "tf_warp",
    "tf_align",
    "rev",
    "sort",
    "rank",
    "xtfrm",
    "summary",
    "fivenum",
    "quantile",
    "tf_depth",
    "tf_interpolate",
    "tf_invert"
  )

  # The contract: a walked single-argument generic must either *succeed*
  # (a real tf_mv method exists, just not registered under that name -- e.g.
  # group generics) or abort with the classed `tf_mv_method_unimplemented`
  # condition. Any other error -- classed or not -- means a stub is missing
  # and the call fell through to a univariate method.
  fails_cleanly <- function(call_expr) {
    tryCatch(
      {
        eval(call_expr)
        TRUE
      },
      tf_mv_method_unimplemented = function(e) TRUE,
      error = function(e) {
        structure(FALSE, message = conditionMessage(e))
      }
    )
  }

  checked <- character()
  for (gen in names(by_gen)) {
    classes <- by_gen[[gen]]
    if (!has_univariate(classes)) next
    if (has_tf_mv(classes)) next
    if (!(gen %in% walkable)) next
    # try a one-arg call; skip anything that genuinely needs >1 argument
    call_expr <- str2lang(sprintf("%s(fm)", gen))
    expect_true(
      fails_cleanly(call_expr),
      info = sprintf(
        "Generic %s(): tf_mv path must either succeed or abort with tf_mv_method_unimplemented; any other error means a fail-fast stub is missing.",
        gen
      )
    )
    checked <- c(checked, gen)
  }
  # Sanity: walker observed *some* generics with univariate methods (else the
  # NAMESPACE/test wiring is wrong).
  expect_true(any(has_univariate(unlist(by_gen))))
  # Sanity: the walkable list must actually overlap registered S3 generics --
  # otherwise the walker silently iterates over nothing and the test rots into
  # a no-op. (`checked` itself can legitimately be 0 once every univariate-tf
  # generic has an explicit tf_mv method; that's the desired end state.)
  expect_gt(length(intersect(walkable, names(by_gen))), 10)
})
