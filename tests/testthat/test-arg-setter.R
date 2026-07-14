test_that("tf_arg<-.tfd_irreg preserves values on round-trip (#234)", {
  set.seed(1234)
  x <- tf_sparsify(tf_rgp(2))
  evals_before <- tf_evaluations(x)
  arg_before <- tf_arg(x)
  suppressWarnings(tf_arg(x) <- tf_arg(x))
  expect_identical(tf_evaluations(x), evals_before)
  expect_identical(tf_arg(x), arg_before)
})

test_that("tf_arg<-.tfd_irreg keeps NA entries as NULL (#234)", {
  set.seed(99)
  x <- tf_sparsify(tf_rgp(3))
  # zero out one entry via arithmetic with NA to produce a NULL entry
  x_with_na <- x + c(0, NA_real_, 0)
  na_mask_before <- is.na(x_with_na)
  suppressWarnings(tf_arg(x_with_na) <- tf_arg(x_with_na))
  expect_identical(is.na(x_with_na), na_mask_before)
})

test_that("tf_arg<-.tfd_irreg accepts a single shared arg vector (#234)", {
  x <- tfd(
    list(c(1, 2, 3), c(4, 5, 6)),
    arg = list(c(0, 0.5, 1), c(0, 0.5, 1))
  )
  x <- as.tfd_irreg(x)
  new_arg <- c(0.1, 0.5, 0.9)
  suppressWarnings(tf_arg(x) <- new_arg)
  expect_identical(tf_arg(x)[[1]], new_arg)
  expect_identical(tf_arg(x)[[2]], new_arg)
  expect_equal(tf_evaluations(x)[[1]], c(1, 2, 3))
  expect_equal(tf_evaluations(x)[[2]], c(4, 5, 6))
})
