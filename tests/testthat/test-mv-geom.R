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

test_that("tf_inner evaluates mixed component grids on the paired union grid", {
  x <- tfd(matrix(c(1, 2, 3), nrow = 1), arg = c(0, 0.5, 1))
  y <- tfd(matrix(c(10, 20, 30, 40), nrow = 1), arg = c(0, 0.25, 0.75, 1))
  f <- tfd_mv(list(x = x, y = y), domain = c(0, 1))
  grid <- c(0, 0.25, 0.5, 0.75, 1)

  paired <- tf_evaluate(f, arg = grid)[[1]]
  inner <- tf_inner(f, f)

  expect_equal(tf_arg(inner), grid)
  expect_equal(
    tf_evaluate(inner, arg = grid)[[1]],
    paired$x^2 + paired$y^2
  )
})

test_that("tf_inner restricts to the common domain on partial overlap", {
  # f(t) = (t, t) on [0, 1], g(t) = (t, t) on [0.5, 1.5]; overlap is [0.5, 1],
  # where <f, g> = t^2 + t^2 = 2 t^2.
  f <- tfd_mv(
    list(
      x = tfd(matrix(c(0, 0.5, 1), nrow = 1), arg = c(0, 0.5, 1)),
      y = tfd(matrix(c(0, 0.5, 1), nrow = 1), arg = c(0, 0.5, 1))
    ),
    domain = c(0, 1)
  )
  g <- tfd_mv(
    list(
      x = tfd(matrix(c(0.5, 1, 1.5), nrow = 1), arg = c(0.5, 1, 1.5)),
      y = tfd(matrix(c(0.5, 1, 1.5), nrow = 1), arg = c(0.5, 1, 1.5))
    ),
    domain = c(0.5, 1.5)
  )
  inner <- tf_inner(f, g)
  expect_equal(tf_domain(inner), c(0.5, 1))
  expect_equal(tf_arg(inner), c(0.5, 1))
  expect_equal(tf_evaluate(inner, arg = c(0.5, 1))[[1]], c(2 * 0.5^2, 2 * 1^2))
})

test_that("tf_inner errors on non-overlapping domains", {
  f <- tfd_mv(
    list(
      x = tfd(matrix(c(0, 1), nrow = 1), arg = c(0, 1)),
      y = tfd(matrix(c(0, 1), nrow = 1), arg = c(0, 1))
    ),
    domain = c(0, 1)
  )
  g <- tfd_mv(
    list(
      x = tfd(matrix(c(2, 3), nrow = 1), arg = c(2, 3)),
      y = tfd(matrix(c(2, 3), nrow = 1), arg = c(2, 3))
    ),
    domain = c(2, 3)
  )
  expect_error(tf_inner(f, g), "non-overlapping")
})

test_that("tf_inner of size-0 and size-1 operands is the empty tfd", {
  set.seed(7)
  f1 <- tfd_mv(list(x = tf_rgp(1), y = tf_rgp(1)))
  f0 <- f1[0]
  expect_equal(vec_size(tf_inner(f0, f1)), 0L)
  expect_equal(vec_size(tf_inner(f1, f0)), 0L)
  expect_s3_class(tf_inner(f0, f1), "tfd")
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

test_that("tf_tangent aligns mixed derivative grids before scaling", {
  x_arg <- c(0, 0.5, 1)
  y_arg <- c(0, 0.25, 0.75, 1)
  f <- tfd_mv(
    list(
      x = tfd(matrix(x_arg, nrow = 1), arg = x_arg),
      y = tfd(matrix(y_arg, nrow = 1), arg = y_arg)
    ),
    domain = c(0, 1)
  )
  grid <- sort(unique(c(x_arg, y_arg)))

  tan <- tf_tangent(f)
  expect_s3_class(tan, "tfd_mv")
  expect_equal(
    tf_evaluate(tf_norm(tan), arg = grid)[[1]],
    rep(1, length(grid)),
    tolerance = 1e-10
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

# Univariate generalization of the geometric primitives ------------------------

test_that("tf_norm/tf_inner/tf_distance reduce to scalar ops for univariate tf", {
  set.seed(21)
  u <- tf_rgp(3)
  v <- tf_rgp(3)
  # univariate norm is the pointwise absolute value
  expect_equal(tf_norm(u), abs(u))
  # univariate inner product is the pointwise product
  expect_equal(tf_inner(u, v), u * v)
  # univariate distance is |u - v|
  expect_equal(tf_distance(u, v), abs(u - v))
  # tf_speed and tf_tangent compose the same way
  expect_equal(tf_speed(u), abs(tf_derive(u)))
  expect_s3_class(tf_tangent(u), "tfd")
})

test_that("univariate geometric primitives also work for tfb", {
  set.seed(22)
  u <- tfb(tf_rgp(2), verbose = FALSE)
  # tfb arithmetic warns about the lossy round-trip through tfd; that documented
  # behaviour is not what we are testing here.
  expect_s3_class(suppressWarnings(tf_norm(u)), "tfb")
  expect_s3_class(suppressWarnings(tf_inner(u, u)), "tfb")
})

test_that("geometric primitives error informatively on non-tf input", {
  expect_error(tf_norm(1:3), "not defined for")
  expect_error(tf_tangent("a"), "not defined for")
  expect_error(tf_inner(1:3, 1:3), "not defined for")
  # univariate tf vs tf_mv is a mismatch
  set.seed(23)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(tf_inner(tf_rgp(2), f), "univariate")
})

test_that("tf_arclength rejects lower > upper", {
  set.seed(24)
  f <- tfd_mv(list(x = tf_rgp(2), y = tf_rgp(2)))
  expect_error(tf_arclength(f, lower = 0.8, upper = 0.2), "must not exceed")
})

test_that("tf_arclength handles irregular curves with sub-domain support", {
  # two curves on a [0, 10] domain, each observed on a different sub-interval:
  # curve A on [0, 3] -- straight line of length sqrt(2) * 3 in 2-D
  # curve B on [5, 10] -- straight line of length sqrt(2) * 5
  curveA <- data.frame(
    id = "A",
    t = c(0, 1, 2, 3),
    x = c(0, 1, 2, 3),
    y = c(0, 1, 2, 3)
  )
  curveB <- data.frame(
    id = "B",
    t = c(5, 6, 8, 10),
    x = c(0, 1, 3, 5),
    y = c(0, 1, 3, 5)
  )
  long <- rbind(curveA, curveB)
  trk <- tfd_mv(list(
    x = tfd(long, id = "id", arg = "t", value = "x", domain = c(0, 10)),
    y = tfd(long, id = "id", arg = "t", value = "y", domain = c(0, 10))
  ))
  al <- tf_arclength(trk)
  expect_equal(unname(al), c(3 * sqrt(2), 5 * sqrt(2)), tolerance = 1e-8)
})
