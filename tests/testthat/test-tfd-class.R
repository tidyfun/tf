test_that("tfd.numeric works", {
  # regular data
  set.seed(1234)
  x <- runif(100)
  f <- tfd(x)
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), list(1:100))
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # irregular data
  x[c(2, 4, 6)] <- NA
  f <- tfd(x)
  expect_s3_class(f, "tfd_irreg")
  expect_length(f, 1)
  expect_identical(attr(f, "arg"), numeric())
  expect_identical(attr(f, "domain"), c(1L, 100L))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # empty data: length-0 prototype with NA sentinel domain
  f <- tfd(numeric())
  expect_s3_class(f, "tfd_reg")
  expect_length(f, 0)
  expect_identical(attr(f, "arg"), list(integer()))
  expect_identical(attr(f, "domain"), c(NA_real_, NA_real_))
  expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
  expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")

  # single NA -> length-1 vector of NA functions (#241)
  for (x in list(NA_real_, NA_integer_)) {
    f <- suppressWarnings(tfd(x))
    expect_s3_class(f, "tfd_reg")
    expect_length(f, 1)
    expect_true(is.na(f))
    expect_function(attr(f, "evaluator"), args = c("x", "arg", "evaluations"))
    expect_identical(attr(f, "evaluator_name"), "tf_approx_linear")
  }
  # warn on NA-only input
  expect_warning(tfd(NA_real_), "NA")

  # evaluations must be inside the domain
  x <- 1:10
  expect_no_error(tfd(x, domain = c(1, 10)))
  expect_error(
    tfd(x, domain = c(2, 10)),
    "Evaluations must be inside the domain."
  )
  expect_error(
    tfd(x, domain = c(1, 9)),
    "Evaluations must be inside the domain."
  )
  expect_error(
    tfd(x, domain = c(2, 9)),
    "Evaluations must be inside the domain."
  )

  # constructor invariants
  expect_valid_tf(tfd(runif(100)))
  x_na <- runif(100); x_na[c(2, 4, 6)] <- NA
  expect_valid_tf(tfd(x_na))
  expect_valid_tf(tfd(numeric()))
})

test_that("tfd works consistently for partially missing data", {
  x <- tf_rgp(10)
  x_df <- x |> tf_2_df()
  x_df[x_df$id == "2", "value"] <- NA
  x_mat <- x |> as.matrix()
  x_mat[2, ] <- NA
  expect_warning(tfd(x_df), "NA")
  expect_class(tfd(x_df) |> suppressWarnings(), "tfd_reg")
  expect_equal(
    tfd(x_df) |> suppressWarnings(),
    tfd(x_mat) |> suppressWarnings()
  )

  x <- tf_rgp(10) |> tf_sparsify(0.8)
  x_df <- x |> tf_2_df()
  x_df[x_df$id == "2", "value"] <- NA
  x_mat <- x |> as.matrix() |> suppressWarnings()
  x_mat[2, ] <- NA
  expect_warning(tfd(x_df), "NA")
  expect_class(tfd(x_df) |> suppressWarnings(), "tfd_irreg")
  expect_equal(
    tfd(x_df) |> suppressWarnings(),
    tfd(x_mat) |> suppressWarnings()
  )

  expect_valid_tf(suppressWarnings(tfd(x_df)))
  expect_valid_tf(suppressWarnings(tfd(x_mat)))
})

test_that("NA creation warning uses singular/plural wording and lists indices", {
  x_one_na <- rbind(
    1:5,
    rep(NA_real_, 5)
  )
  expect_warning(
    tfd(x_one_na, arg = 1:5),
    "1 `NA` entry \\(empty function\\) created\\."
  )
  expect_warning(
    tfd(x_one_na, arg = 1:5),
    "Affected index: 2"
  )

  x_two_na <- rbind(
    rep(NA_real_, 5),
    1:5,
    rep(NA_real_, 5)
  )
  expect_warning(
    tfd(x_two_na, arg = 1:5),
    "2 `NA` entries \\(empty functions\\) created\\."
  )
  expect_warning(
    tfd(x_two_na, arg = 1:5),
    "Affected indices: 1, 3"
  )

  x_many_na <- rbind(
    1:5,
    matrix(NA_real_, nrow = 12, ncol = 5)
  )
  expect_warning(
    tfd(x_many_na, arg = 1:5),
    "Affected indices: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, \\.{3}"
  )
})

test_that("tfd.list errors on mismatched per-entry lengths (#235)", {
  # different lengths with shared arg -> must not silently return tfd_reg
  expect_error(
    tfd(list(1:3, 1:5), arg = 1:5),
    "arg"
  )
  # different lengths and arg list -> irregular path catches it
  expect_error(
    tfd(list(1:3, 1:5), arg = list(1:3, 1:4)),
    "do not match"
  )
})

test_that("all-NA input yields length-n NA functions, not length-0 (#241)", {
  # vector of NAs
  x <- rep(NA_real_, 3)
  expect_warning(f <- tfd(x), "NA")
  expect_s3_class(f, "tfd")
  expect_length(f, 1) # one curve, all NA evaluations
  expect_true(is.na(f))

  # matrix with all-NA rows -> one NA function per row
  m <- matrix(NA_real_, nrow = 3, ncol = 5)
  expect_warning(f <- tfd(m, arg = 1:5), "NA")
  expect_length(f, 3)
  expect_true(all(is.na(f)))
})

test_that("tfd() length-0 prototype uses NA sentinel domain (#241)", {
  f <- tfd(numeric())
  expect_identical(attr(f, "domain"), c(NA_real_, NA_real_))
  expect_length(f, 0)
})

test_that("tfd.list infers irregular when lengths differ but arg list matches (#235)", {
  f <- tfd(list(1:3, 1:5), arg = list(1:3, 1:5))
  expect_s3_class(f, "tfd_irreg")
  expect_length(f, 2)
  expect_equal(tf_evaluations(f)[[1]], 1:3)
  expect_equal(tf_evaluations(f)[[2]], 1:5)
})
