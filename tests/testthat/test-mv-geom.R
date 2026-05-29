# Tier-1 geometric primitives for tf_mv (tf_norm / tf_speed / tf_inner /
# tf_distance / tf_tangent / tf_reparam_arclength).

test_that("tf_norm is the pointwise Euclidean norm of the components", {
  t <- seq(0, 1, length.out = 51)
  f <- tfd_mv(list(
    x = tfd(matrix(rep(3, 51), nrow = 1), arg = t),
    y = tfd(matrix(rep(4, 51), nrow = 1), arg = t)
  ))
  # constant (3, 4) -> ||.|| == 5
  expect_s3_class(tf_norm(f), "tfd")
  expect_equal(tf_evaluations(tf_norm(f))[[1]], rep(5, 51))
})

test_that("tf_speed equals tf_norm(tf_derive(.))", {
  set.seed(11)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_equal(tf_speed(f), tf_norm(tf_derive(f)))
})

test_that("tf_inner is component-wise dot product, agrees with hand calc", {
  t <- seq(0, 1, length.out = 51)
  f <- tfd_mv(list(
    x = tfd(matrix(rep(2, 51), nrow = 1), arg = t),
    y = tfd(matrix(rep(3, 51), nrow = 1), arg = t)
  ))
  g <- tfd_mv(list(
    x = tfd(matrix(rep(4, 51), nrow = 1), arg = t),
    y = tfd(matrix(rep(5, 51), nrow = 1), arg = t)
  ))
  # <(2,3), (4,5)> = 2*4 + 3*5 = 23 everywhere
  expect_equal(tf_evaluations(tf_inner(f, g))[[1]], rep(23, 51))
  # incompatible structures -> error
  expect_error(
    tf_inner(f, tfd_mv(list(a = tf_rgp(1), b = tf_rgp(1), c = tf_rgp(1)))),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("tf_distance equals tf_norm(f - g)", {
  set.seed(12)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  g <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_equal(tf_distance(f, g), tf_norm(f - g))
  # distance to self is 0
  expect_equal(
    tf_evaluations(tf_distance(f, f))[[1]],
    rep(0, length(tf_arg(f))),
    tolerance = 1e-12
  )
})

test_that("tf_tangent has unit speed everywhere except where speed = 0", {
  # unit circle: tangent is (-sin(2pi t), cos(2pi t)), always unit length
  t <- seq(0, 1, length.out = 401)
  circ <- tfd_mv(list(
    x = tfd(matrix(cos(2 * pi * t), nrow = 1), arg = t),
    y = tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
  ))
  tan <- tf_tangent(circ)
  expect_s3_class(tan, "tfd_mv")
  expect_identical(tf_ncomp(tan), 2L)
  # speed of the tangent at the middle of the domain should be ~ 1
  expect_equal(
    unlist(tf_evaluate(tf_norm(tan), arg = 0.5)),
    1,
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
})

test_that("tf_reparam_arclength yields a (nearly) constant-speed curve", {
  # an unevenly-parameterized line: f(t) = (t^2, 0) on [0,1].
  # the curve is the segment from (0,0) to (1,0) (length 1), traversed
  # non-uniformly. After arclength reparameterization the speed should be
  # approximately constant (= 1).
  t <- seq(0, 1, length.out = 401)
  f <- tfd_mv(list(
    x = tfd(matrix(t^2, nrow = 1), arg = t),
    y = tfd(matrix(rep(0, 401), nrow = 1), arg = t)
  ))
  g <- tf_reparam_arclength(f)
  expect_s3_class(g, "tfd_mv")
  # sample speed on the interior of the domain (avoid boundary FD artefacts)
  sp <- tf_evaluate(tf_speed(g), arg = c(0.3, 0.5, 0.7))[[1]]
  expect_equal(sp, rep(1, 3), tolerance = 0.1)
})

test_that("tf_reparam_arclength preserves non-unit domains", {
  t <- seq(0, 10, length.out = 11)
  f <- tfd_mv(list(
    x = tfd(matrix(t, nrow = 1), arg = t),
    y = tfd(matrix(0, nrow = 1, ncol = length(t)), arg = t)
  ))
  r <- tf_reparam_arclength(f)
  m <- as.matrix(r, arg = t, interpolate = TRUE)
  expect_false(anyNA(m))
  expect_equal(unname(m[1, , "x"]), t, tolerance = 1e-8)
  expect_equal(unname(m[1, , "y"]), rep(0, length(t)), tolerance = 1e-8)
})

test_that("tf_reparam_arclength leaves zero-length (constant) curves unchanged", {
  t <- seq(0, 1, length.out = 30)
  # curve 1: a real (non-degenerate) curve; curve 2: constant in both components
  f <- tfd_mv(list(
    x = tfd(rbind(cos(2 * pi * t), rep(2, 30)), arg = t),
    y = tfd(rbind(sin(2 * pi * t), rep(3, 30)), arg = t)
  ))
  expect_warning(g <- tf_reparam_arclength(f), "zero/undefined arc length")
  expect_s3_class(g, "tfd_mv")
  expect_length(g, 2L)
  # degenerate curve returned untouched, well-defined curve free of NaN
  expect_equal(
    as.matrix(g[2]),
    as.matrix(f[2]),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_false(any(is.nan(as.matrix(g[1]))))
})
