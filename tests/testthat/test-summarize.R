# Create test data
grid <- seq(0, 1, length.out = 21)
lin <- -3:3 * tfd(grid, grid)
parallel <- 0:4 + tfd(0 * grid, grid)

# Create irregular data
lin_irreg <- {
  tmp <- as.matrix(lin)
  tmp[1:3, 1] <- NA
  tfd(tmp)
}
lin_too_irreg <- lin_irreg |> tf_jiggle() #no common gridpoints --> most summaries will be empty

# NA values
lin_na <- c(lin, NA)

lin_b <- tfb(lin, k = 5) |> suppressMessages()
parallel_b <- tfb(parallel, k = 5) |> suppressMessages()

test_that("mean.tf works correctly", {
  mean_lin <- colMeans(as.matrix(lin))

  # Test regular tfd
  mean_lin_tf <- mean(lin)
  expect_s3_class(mean_lin_tf, "tfd")
  expect_equal(tf_evaluations(mean_lin_tf)[[1]], mean_lin, ignore_attr = TRUE)

  # Test irregular tfd
  expect_warning(mean(lin_irreg), "irregular")
  mean_lin_irreg <- mean(lin_irreg) |> suppressWarnings()
  expect_s3_class(mean_lin_irreg, "tfd_irreg")

  mean_lin_irreg <- mean(lin_irreg, na.rm = TRUE)
  expect_equal(
    tf_evaluations(mean_lin_irreg)[[1]],
    mean_lin,
    ignore_attr = TRUE
  )

  expect_warning(mean(lin_too_irreg), "NA")
  mean_lin_too_irreg <- mean(lin_too_irreg) |> suppressWarnings()
  expect_true(is.na(mean_lin_too_irreg))

  # Test tfd with NA
  expect_warning(mean(lin_na), "NA")
  mean_lin_na <- suppressWarnings(mean(lin_na))
  expect_s3_class(mean_lin_na, "tfd")
  expect_true(is.na(mean_lin_na))
  expect_equal(mean(lin_na, na.rm = TRUE), mean(lin))

  # Test tfb
  mean_lin_b <- mean(lin_b)
  expect_s3_class(mean_lin_b, "tfb")
  expect_equal(
    tf_evaluations(mean_lin_b)[[1]],
    mean_lin,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("summary.tf works correctly", {
  # Test on tfd
  parallel_summary <- summary(parallel)
  expect_s3_class(parallel_summary, "tfd")
  expect_length(parallel_summary, 6) # min, lower_mid, median, mean, upper_mid, max

  # Verify names
  expect_named(
    parallel_summary,
    c("min", "lower_mid", "median", "mean", "upper_mid", "max")
  )

  # Verify specific values
  # Min should be the first function (0 + grid)
  expect_equal(
    tf_evaluations(parallel_summary)[["min"]],
    tf_evaluations(parallel)[[1]],
    ignore_attr = TRUE
  )

  # Max should be the last function (4 + grid)
  expect_equal(
    tf_evaluations(parallel_summary)[["max"]],
    tf_evaluations(parallel)[[5]],
    ignore_attr = TRUE
  )

  # Mean should match manual calculation
  expect_equal(
    tf_evaluations(parallel_summary)[["mean"]],
    colMeans(as.matrix(parallel)),
    ignore_attr = TRUE
  )

  # Median should be the middle function (2 + grid)
  expect_equal(
    tf_evaluations(parallel_summary)[["median"]],
    tf_evaluations(parallel)[[3]],
    ignore_attr = TRUE
  )
  expect_equal(
    tf_evaluations(parallel_summary)[["lower_mid"]],
    tf_evaluations(parallel)[[2]],
    ignore_attr = TRUE
  )
  expect_equal(
    tf_evaluations(parallel_summary)[["upper_mid"]],
    tf_evaluations(parallel)[[4]],
    ignore_attr = TRUE
  )

  # NAs don't affect summary:
  expect_equal(summary(parallel), summary(c(parallel, NA)))

  # Test on tfb
  parallel_b_summary <- summary(parallel_b)
  expect_s3_class(parallel_b_summary, "tfb")
  expect_length(parallel_b_summary, 6)

  # Min should be the first function (0 + grid)
  expect_equal(
    tf_evaluations(parallel_b_summary)[["min"]],
    tf_evaluations(parallel_b)[[1]],
    ignore_attr = TRUE
  )

  # Max should be the last function (4 + grid)
  expect_equal(
    tf_evaluations(parallel_b_summary)[["max"]],
    tf_evaluations(parallel_b)[[5]],
    ignore_attr = TRUE
  )

  # Mean should match manual calculation
  expect_equal(
    tf_evaluations(parallel_b_summary)[["mean"]],
    colMeans(as.matrix(parallel_b)),
    ignore_attr = TRUE
  )

  # Median should be the middle function (2 + grid)
  expect_equal(
    tf_evaluations(parallel_b_summary)[["median"]],
    tf_evaluations(parallel_b)[[3]],
    ignore_attr = TRUE
  )
})

# see test-depth.R for median

test_that("cum* functions work for tfd objects", {
  # Test cumsum on tfd
  lin_cumsum <- cumsum(lin)
  expect_s3_class(lin_cumsum, "tfd")
  expect_length(lin_cumsum, length(lin))

  # Verify results match manual calculation
  lin_matrix <- as.matrix(lin)
  expected_cumsum <- apply(lin_matrix, 2, cumsum)
  for (i in seq_along(lin)) {
    expect_equal(
      tf_evaluations(lin_cumsum[i])[[1]],
      expected_cumsum[i, ],
      ignore_attr = TRUE
    )
  }

  # Test cummax on tfd
  lin_cummax <- cummax(lin)
  expect_s3_class(lin_cummax, "tfd")
  expect_length(lin_cummax, length(lin))

  # Verify results match manual calculation
  expected_cummax <- apply(lin_matrix, 2, cummax)
  for (i in seq_along(lin)) {
    expect_equal(
      tf_evaluations(lin_cummax[i])[[1]],
      expected_cummax[i, ],
      ignore_attr = TRUE
    )
  }

  # Test with NA values
  lin_na <- c(lin, NA)
  expect_warning(cumsum(lin_na))
  lin_na_cumsum <- suppressWarnings(cumsum(lin_na))

  # First elements should still be calculated
  for (i in seq_along(lin)) {
    expect_equal(
      tf_evaluations(lin_na_cumsum[i])[[1]],
      expected_cumsum[i, ],
      ignore_attr = TRUE
    )
  }
})

test_that("cum* functions work for tfb objects", {
  # Test cumsum on tfb
  lin_b_cumsum <- cumsum(lin_b)
  expect_s3_class(lin_b_cumsum, "tfb")
  expect_length(lin_b_cumsum, length(lin_b))

  # Verify results match manual calculation
  lin_b_matrix <- as.matrix(lin_b)
  expected_cumsum <- apply(lin_b_matrix, 2, cumsum)
  for (i in seq_along(lin_b)) {
    expect_equal(
      tf_evaluations(lin_b_cumsum[i])[[1]],
      expected_cumsum[i, ],
      ignore_attr = TRUE
    )
  }

  # Test cummax on tfd
  lin_b_cummax <- cummax(lin_b)
  expect_s3_class(lin_b_cummax, "tfb")
  expect_length(lin_b_cummax, length(lin_b))

  # Verify results match manual calculation
  expected_cummax <- apply(lin_b_matrix, 2, cummax)
  for (i in seq_along(lin_b)) {
    expect_equal(
      tf_evaluations(lin_b_cummax[i])[[1]],
      expected_cummax[i, ],
      ignore_attr = TRUE
    )
  }

  # # Test with NA values
  # lin_b_na <- c(lin_b, NA_real_*lin_b[1])
  # expect_warning(cumsum(lin_b_na))
  # lin_b_na_cumsum <- suppressWarnings(cumsum(lin_b_na))
  #
  #
  # # First elements should still be calculated
  # for (i in seq_along(lin_b)) {
  #   expect_equal(
  #     tf_evaluations(lin_b_na_cumsum[i])[[1]],
  #     expected_cumsum[i, ],
  #     ignore_attr = TRUE
  #   )
  # }
})
