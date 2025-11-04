test_that("NA entries are consistently NULL in concatenation operations", {
  set.seed(1234)
  x <- tf_rgp(3)

  # c(tfd, NA) - append NA
  x_with_na <- c(x, NA)
  expect_length(x_with_na, 4)
  expect_true(is.na(x_with_na)[4])
  expect_null(unclass(x_with_na)[[4]]) # NULL entry
  expect_no_error(print(x_with_na))

  # c(NA, tfd) - prepend NA (may not work due to vctrs issue, but test if it does)
  na_then_x <- suppressWarnings(try(c(NA, x), silent = TRUE))
  if (!inherits(na_then_x, "try-error")) {
    expect_true(is.na(na_then_x)[1])
    expect_null(unclass(na_then_x)[[1]])
  }

  # c(tfd, NA, tfd) - NA in middle
  x_sandwich <- c(x[1:2], NA, x[3])
  expect_length(x_sandwich, 4)
  expect_true(is.na(x_sandwich)[3])
  expect_null(unclass(x_sandwich)[[3]])
  expect_no_error(print(x_sandwich))

  # Multiple NAs
  multi_na <- c(x[1], NA, NA, x[2])
  expect_length(multi_na, 4)
  expect_equal(is.na(multi_na), c(FALSE, TRUE, TRUE, FALSE))
  expect_null(unclass(multi_na)[[2]])
  expect_null(unclass(multi_na)[[3]])
  expect_no_error(print(multi_na))
})

test_that("NA entries are consistently NULL in irregular tfd concatenation", {
  set.seed(1234)
  x <- tf_rgp(3) |> tf_sparsify(0.5)
  expect_s3_class(x, "tfd_irreg")

  # Concatenate with NA
  x_with_na <- c(x, NA)
  expect_length(x_with_na, 4)
  expect_true(is.na(x_with_na)[4])
  expect_null(unclass(x_with_na)[[4]]) # NULL entry for irregular
  expect_no_error(print(x_with_na))

  # Multiple NAs in irregular
  irreg_multi <- c(x[1], NA, x[2], NA)
  expect_length(irreg_multi, 4)
  expect_equal(is.na(irreg_multi), c(FALSE, TRUE, FALSE, TRUE))
  expect_null(unclass(irreg_multi)[[2]])
  expect_null(unclass(irreg_multi)[[4]])
})

test_that("NA entries are consistently NULL in subindexing/assignment operations", {
  set.seed(1234)
  x <- tf_rgp(5)

  # Assign single NA
  x1 <- x
  x1[3] <- NA
  expect_true(is.na(x1)[3])
  expect_null(unclass(x1)[[3]])
  expect_no_error(print(x1))

  # Assign multiple NAs
  x2 <- x
  x2[c(2, 4)] <- NA
  expect_equal(is.na(x2), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_null(unclass(x2)[[2]])
  expect_null(unclass(x2)[[4]])
  expect_no_error(print(x2))

  # Assign all to NA
  x3 <- x
  x3[1:5] <- NA
  expect_equal(is.na(x3), rep(TRUE, 5))
  for (i in 1:5) {
    expect_null(unclass(x3)[[i]])
  }
  expect_no_error(print(x3))
  # Check range is [NA, NA] not [Inf, -Inf]
  printed <- capture.output(print(x3))
  expect_true(any(grepl("\\[NA,NA\\]", printed)))
  expect_false(any(grepl("\\[Inf,-Inf\\]", printed)))
})

test_that("NA entries are consistently NULL in arithmetic with NA_real_", {
  set.seed(1234)
  x <- tf_rgp(3)

  # Subtraction with NA_real_
  x_sub <- x - NA_real_
  expect_length(x_sub, 3)
  expect_equal(is.na(x_sub), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_sub)[[i]])
  }
  expect_no_error(print(x_sub))

  # Addition with NA_real_
  x_add <- x + NA_real_
  expect_equal(is.na(x_add), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_add)[[i]])
  }

  # Multiplication with NA_real_
  x_mul <- x * NA_real_
  expect_equal(is.na(x_mul), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_mul)[[i]])
  }

  # Division with NA_real_
  x_div <- x / NA_real_
  expect_equal(is.na(x_div), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_div)[[i]])
  }

  # Power with NA_real_
  x_pow <- x^NA_real_
  expect_equal(is.na(x_pow), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_pow)[[i]])
  }
})

test_that("NA entries are consistently NULL in arithmetic NA_real_ with tfd", {
  set.seed(1234)
  x <- tf_rgp(3)

  # NA_real_ - tfd
  na_sub <- NA_real_ - x
  expect_equal(is.na(na_sub), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(na_sub)[[i]])
  }

  # NA_real_ + tfd
  na_add <- NA_real_ + x
  expect_equal(is.na(na_add), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(na_add)[[i]])
  }

  # NA_real_ * tfd
  na_mul <- NA_real_ * x
  expect_equal(is.na(na_mul), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(na_mul)[[i]])
  }

  # NA_real_ / tfd
  na_div <- NA_real_ / x
  expect_equal(is.na(na_div), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(na_div)[[i]])
  }
})

test_that("NA entries are consistently NULL in vectorized arithmetic with NA", {
  set.seed(1234)
  x <- tf_rgp(4)

  # Element-wise operations with vector containing NA
  v <- c(1, 2, NA_real_, 4)

  x_vec <- x + v
  expect_equal(is.na(x_vec)[3], TRUE)
  expect_null(unclass(x_vec)[[3]])
  expect_false(is.na(x_vec)[1])
  expect_false(is.na(x_vec)[2])
  expect_false(is.na(x_vec)[4])

  x_vec2 <- x * v
  expect_equal(is.na(x_vec2)[3], TRUE)
  expect_null(unclass(x_vec2)[[3]])

  # All NAs in vector
  v_all_na <- c(NA_real_, NA_real_, NA_real_, NA_real_)
  x_all_na <- x + v_all_na
  expect_equal(is.na(x_all_na), rep(TRUE, 4))
  for (i in 1:4) {
    expect_null(unclass(x_all_na)[[i]])
  }
})

test_that("NA entries are consistently NULL in tfd-tfd operations creating NAs", {
  set.seed(1234)
  x <- tf_rgp(3)
  y <- tf_rgp(3)

  # Create a tfd with NA
  y[2] <- NA
  expect_null(unclass(y)[[2]])

  # Operations between tfd with NA and tfd without
  result <- x + y
  expect_true(is.na(result)[2])
  expect_null(unclass(result)[[2]])

  result2 <- x * y
  expect_true(is.na(result2)[2])
  expect_null(unclass(result2)[[2]])

  # Both have NAs
  x[1] <- NA
  result3 <- x + y
  expect_equal(is.na(result3), c(TRUE, TRUE, FALSE))
  expect_null(unclass(result3)[[1]])
  expect_null(unclass(result3)[[2]])
})

test_that("NA entries are consistently NULL after irregular tfd operations", {
  set.seed(1234)
  x <- tf_rgp(3) |> tf_sparsify(0.6)
  expect_s3_class(x, "tfd_irreg")

  # Arithmetic creating NA in irregular
  x_na <- x - NA_real_
  expect_equal(is.na(x_na), rep(TRUE, 3))
  for (i in 1:3) {
    expect_null(unclass(x_na)[[i]])
  }
  expect_no_error(print(x_na))

  # Assignment in irregular
  x[2] <- NA
  expect_true(is.na(x)[2])
  expect_null(unclass(x)[[2]])
})

test_that("NA entries are consistently NULL in tfb operations", {
  set.seed(1234)
  x <- tf_rgp(3) |> tfb(k = 15)
  expect_s3_class(x, "tfb")

  # tfb arithmetic with NA_real_
  x_na <- suppressWarnings(x + NA_real_)  # May warn about lossy cast
  expect_equal(is.na(x_na), rep(TRUE, 3))
  # After tf_rebase, should still be NULL
  for (i in 1:3) {
    expect_null(unclass(x_na)[[i]])
  }

  # Assign NA to tfb
  x[2] <- NA
  expect_true(is.na(x)[2])
  expect_null(unclass(x)[[2]])
  expect_no_error(print(x))
})

test_that("NA entries are consistently NULL in class transformations", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[2] <- NA

  # tfd to tfb with NA
  x_tfb <- suppressWarnings(tfb(x, k = 15))
  expect_true(is.na(x_tfb)[2])
  expect_null(unclass(x_tfb)[[2]])

  # tfb to tfd with NA
  x_tfd <- tfd(x_tfb)
  expect_true(is.na(x_tfd)[2])
  expect_null(unclass(x_tfd)[[2]])

  # irregular to regular with NA
  y <- tf_rgp(3) |> tf_sparsify(0.6)
  y[1] <- NA
  y_reg <- suppressWarnings(tfd(y, arg = seq(0, 1, length.out = 51)))
  expect_true(is.na(y_reg)[1])
  expect_null(unclass(y_reg)[[1]])
})

test_that("NA entries are consistently NULL in constructor edge cases", {
  # Single NA_real_ input
  x <- suppressWarnings(tfd(NA_real_))
  expect_length(x, 0)  # Creates empty tfd

  # Vector of all NAs
  x_all_na <- suppressWarnings(tfd(c(NA_real_, NA_real_, NA_real_)))
  expect_length(x_all_na, 0)  # Creates empty tfd

  # Matrix with all-NA row
  mat <- matrix(1:15, nrow = 3, ncol = 5)
  mat[2, ] <- NA
  x_mat <- suppressWarnings(tfd(mat))
  expect_true(is.na(x_mat)[2])
  expect_null(unclass(x_mat)[[2]])

  # List with NULL entry
  lst <- list(1:10, NULL, 11:20)
  x_list <- suppressWarnings(tfd(lst))
  expect_true(is.na(x_list)[2])
  expect_null(unclass(x_list)[[2]])

  # List with all-NA entry
  lst2 <- list(1:10, rep(NA, 10), 11:20)
  x_list2 <- suppressWarnings(tfd(lst2, arg = 1:10))
  expect_true(is.na(x_list2)[2])
  expect_null(unclass(x_list2)[[2]])
})

test_that("NA entries are consistently NULL in data.frame constructor", {
  df <- data.frame(
    id = rep(1:3, each = 10),
    arg = rep(1:10, 3),
    value = c(1:10, rep(NA, 10), 21:30)
  )

  x <- suppressWarnings(tfd(df))
  expect_true(is.na(x)[2])
  expect_null(unclass(x)[[2]])
  expect_no_error(print(x))
})

test_that("NA entries print correctly without Inf/-Inf range", {
  set.seed(1234)
  x <- tf_rgp(3)
  x[1:3] <- NA

  printed <- capture.output(print(x))
  # Should show [NA, NA] not [Inf, -Inf]
  expect_true(any(grepl("\\[NA,NA\\]", printed)))
  expect_false(any(grepl("\\[Inf,-Inf\\]", printed)))
  expect_false(any(grepl("Inf", printed)))

  # Test with sparkline formatting
  formatted <- format(x, sparkline = TRUE)
  expect_true(all(formatted == "NA"))

  # Test with string formatting
  formatted_str <- format(x, sparkline = FALSE)
  expect_true(all(formatted_str == "NA"))
})

test_that("NA entries are consistently NULL in complex operation chains", {
  set.seed(1234)
  x <- tf_rgp(4)

  # Chain of operations that creates NAs
  result <- (x + 1) * 2 - NA_real_
  expect_equal(is.na(result), rep(TRUE, 4))
  for (i in 1:4) {
    expect_null(unclass(result)[[i]])
  }

  # Mix of NA and non-NA through operations
  y <- x
  y[2] <- NA
  result2 <- (y + 1) * 2
  expect_true(is.na(result2)[2])
  expect_null(unclass(result2)[[2]])
  expect_false(is.na(result2)[1])

  # Concatenate results with NAs
  z <- c(result2, NA, x[1])
  expect_true(is.na(z)[2])
  expect_true(is.na(z)[5])
  expect_null(unclass(z)[[2]])
  expect_null(unclass(z)[[5]])
})

test_that("NA entries maintain NULL through subsetting", {
  set.seed(1234)
  x <- tf_rgp(5)
  x[c(2, 4)] <- NA

  # Subset including NAs
  x_sub <- x[c(1, 2, 3)]
  expect_true(is.na(x_sub)[2])
  expect_null(unclass(x_sub)[[2]])

  # Subset only NAs
  x_only_na <- x[c(2, 4)]
  expect_equal(is.na(x_only_na), c(TRUE, TRUE))
  expect_null(unclass(x_only_na)[[1]])
  expect_null(unclass(x_only_na)[[2]])
  expect_no_error(print(x_only_na))

  # Negative subsetting
  x_neg <- x[-c(1, 5)]
  expect_equal(is.na(x_neg), c(TRUE, FALSE, TRUE))
  expect_null(unclass(x_neg)[[1]])
  expect_null(unclass(x_neg)[[3]])
})

test_that("NA entries are consistent in mixed regular/irregular scenarios", {
  set.seed(1234)
  x_reg <- tf_rgp(3)
  x_irreg <- tf_rgp(3) |> tf_sparsify(0.5)

  # Create NAs in both
  x_reg[2] <- NA
  x_irreg[2] <- NA

  expect_null(unclass(x_reg)[[2]])
  expect_null(unclass(x_irreg)[[2]])

  # Concatenate regular with irregular
  combined <- suppressWarnings(c(x_reg, x_irreg))
  expect_true(is.na(combined)[2])
  expect_true(is.na(combined)[5])
  expect_null(unclass(combined)[[2]])
  expect_null(unclass(combined)[[5]])
})

test_that("NA entries work with logical operations", {
  set.seed(1234)
  x <- tf_rgp(4)
  x[c(2, 4)] <- NA

  # is.na should work correctly
  na_mask <- is.na(x)
  expect_equal(na_mask, c(FALSE, TRUE, FALSE, TRUE))

  # Subsetting by NA mask
  non_na <- x[!na_mask]
  expect_length(non_na, 2)
  expect_false(any(is.na(non_na)))

  only_na <- x[na_mask]
  expect_length(only_na, 2)
  expect_true(all(is.na(only_na)))
  for (i in 1:2) {
    expect_null(unclass(only_na)[[i]])
  }
})

test_that("NA entries in edge cases with domain and evaluator", {
  set.seed(1234)
  x <- tf_rgp(3, arg = seq(0, 2, length.out = 51))
  x[2] <- NA

  # Change domain (doesn't change data structure)
  expect_null(unclass(x)[[2]])

  # Re-evaluate on different grid
  x_new <- suppressWarnings(tfd(x, arg = seq(0, 2, length.out = 101)))
  expect_true(is.na(x_new)[2])
  expect_null(unclass(x_new)[[2]])

  # Different evaluator
  x_spline <- suppressWarnings(tfd(x, evaluator = tf_approx_spline))
  expect_true(is.na(x_spline)[2])
  expect_null(unclass(x_spline)[[2]])
})
