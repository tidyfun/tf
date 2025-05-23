# TODO:implement & test array-index inputs! (see GH issue)

# Setup comprehensive test data for all tf classes
setup_bracket_test_data <- function() {
  set.seed(2345)

  # Regular tfd data with various patterns - use more points for tfb compatibility
  t_reg <- seq(0, 1, length.out = 51)
  data_reg <- lapply(1:8, function(i) sin(i * pi * t_reg) + 0.1 * rnorm(51))
  tfd_reg <- tfd(data_reg, arg = t_reg)
  names(tfd_reg) <- paste0("func_", letters[1:8])

  # Irregular tfd data
  tfd_irreg <- tf_jiggle(tfd_reg, amount = 0.05)

  # tfb_spline data - use penalized splines or fewer basis functions
  tfb_spl <- tfb(tfd_reg[1:5], k = 15, verbose = FALSE, penalized = FALSE)

  # tfb_fpc data
  tfb_fpc_obj <- tfb_fpc(tfd_reg[1:4], verbose = FALSE)

  # Edge case data
  single_tfd <- tfd_reg[1]
  empty_tfd <- tfd_reg[integer(0)]

  # Data with NA entries
  na_tfd <- tfd_reg
  na_tfd[[2]] <- NA
  na_tfd[[4]] <- NA

  # Very sparse irregular data
  sparse_irreg <- tf_sparsify(tfd_reg, dropout = 0.8)

  list(
    tfd_reg = tfd_reg,
    tfd_irreg = tfd_irreg,
    tfb_spl = tfb_spl,
    tfb_fpc = tfb_fpc_obj,
    single_tfd = single_tfd,
    empty_tfd = empty_tfd,
    na_tfd = na_tfd,
    sparse_irreg = sparse_irreg
  )
}

test_that("[.tf works with all indexing types", {
  data <- setup_bracket_test_data()

  # Test numeric indexing
  expect_equal(length(data$tfd_reg[1:3]), 3)
  expect_equal(length(data$tfd_reg[c(1, 3, 5)]), 3)
  expect:equal(names(data$tfd_reg[1:3]), names(data$tfd_reg)[1:3])

  # Test negative indexing
  expect_identical(data$tfd_reg[-1], data$tfd_reg[2:8])
  expect_identical(names(data$tfd_reg[-1]), names(data$tfd_reg)[-1])

  # Test logical indexing
  logical_idx <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expect_identical(data$tfd_reg[logical_idx], data$tfd_reg[which(logical_idx)])

  # Test character indexing (names)
  expect_identical(data$tfd_reg["func_a"], data$tfd_reg[1])

  # Test all classes
  expect_s3_class(data$tfd_reg[1:3], "tfd_reg")
  expect_s3_class(data$tfd_irreg[1:3], "tfd_irreg")
  expect_s3_class(data$tfb_spl[1:3], "tfb_spline")
  expect_s3_class(data$tfb_fpc[1:3], "tfb_fpc")
})

test_that("[.tf handles edge cases and errors", {
  data <- setup_bracket_test_data()

  # Test empty indexing
  expect_equal(length(data$tfd_reg[integer(0)]), 0)
  expect_s3_class(data$tfd_reg[integer(0)], "tfd_reg")

  # Test single element extraction
  expect_equal(length(data$tfd_reg[1]), 1)
  expect_equal(length(data$single_tfd[1]), 1)

  # Test zero indexing (should give empty result)
  expect_equal(length(data$tfd_reg[0]), 0)

  # Test out-of-bounds errors
  expect_error(data$tfd_reg[10], "past the end")
  expect_error(data$tfd_reg[-10], "past the end")

  # Test NA indexing (should error)
  expect_error(data$tfd_reg[NA], "missing")
  expect_error(data$tfd_reg[c(1, NA, 3)], "missing")

  # Test invalid name indexing
  expect_error(data$tfd_reg["nonexistent"], "doesn't exist")

  # Test logical indexing length mismatch
  expect_error(data$tfd_reg[c(TRUE, FALSE)], "must be size")

  # Test with NA entries in data
  expect_equal(length(data$na_tfd[1:4]), 4)
  expect_identical(data$na_tfd[2], data$na_tfd["func_b"])
})

test_that("[.tf works with j argument (evaluation)", {
  data <- setup_bracket_test_data()

  # Test basic evaluation with matrix=TRUE (default)
  eval_matrix <- data$tfd_reg[1:3, c(0.2, 0.5, 0.8)]
  expect_true(is.matrix(eval_matrix))
  expect_equal(dim(eval_matrix), c(3, 3))
  expect_equal(colnames(eval_matrix), c("0.2", "0.5", "0.8"))
  expect_equal(rownames(eval_matrix), paste0("func_", letters[1:3]))

  # Test evaluation with matrix=FALSE
  eval_list <- data$tfd_reg[1:2, c(0.2, 0.5), matrix = FALSE]
  expect_true(is.list(eval_list))
  expect_length(eval_list, 2)
  expect_true(all(sapply(eval_list, is.data.frame)))
  expect_equal(names(eval_list), c("func_a", "func_b"))

  # Test single function evaluation
  single_eval <- data$tfd_reg[1, seq(0, 1, 0.1)]
  expect_true(is.matrix(single_eval))
  expect_equal(nrow(single_eval), 1)
  expect_equal(ncol(single_eval), 11)

  # Test evaluation at domain boundaries
  boundary_eval <- data$tfd_reg[1:2, c(0, 1)]
  expect_true(is.matrix(boundary_eval))
  expect_equal(dim(boundary_eval), c(2, 2))
})

test_that("[.tf handles interpolation parameter", {
  data <- setup_bracket_test_data()

  # Test interpolate=TRUE (default)
  interp_true <- data$tfd_reg[1, c(0.15, 0.25)]
  expect_true(is.matrix(interp_true))
  expect_false(any(is.na(interp_true)))

  # Test interpolate=FALSE with exact grid points
  exact_points <- attr(data$tfd_reg, "arg")[[1]][c(1, 5, 10)]
  no_interp_exact <- data$tfd_reg[1, exact_points, interpolate = FALSE]
  expect_false(any(is.na(no_interp_exact)))

  # Test interpolate=FALSE with non-grid points (should create NAs)
  expect_warning(
    no_interp_new <- data$tfd_reg[1, c(0.15, 0.25), interpolate = FALSE],
    "no values present"
  )
  expect_true(all(is.na(no_interp_new)))

  # Test that tfb ignores interpolate parameter
  expect_message(
    data$tfb_spl[1, c(0.2, 0.5), interpolate = FALSE],
    "interpolate.*ignored.*basis"
  )
})

test_that("[.tf handles complex j arguments", {
  data <- setup_bracket_test_data()

  # Test list of j arguments
  j_list <- list(
    c(0.1, 0.3, 0.5),
    c(0.2, 0.6),
    c(0.0, 0.25, 0.5, 0.75, 1.0)
  )

  list_eval <- data$tfd_reg[1:3, j_list, matrix = FALSE]
  expect_length(list_eval, 3)
  expect_equal(nrow(list_eval[[1]]), 3)
  expect_equal(nrow(list_eval[[2]]), 2)
  expect_equal(nrow(list_eval[[3]]), 5)

  # Test error when matrix=TRUE with list j
  expect_error(
    data$tfd_reg[1:3, j_list, matrix = TRUE],
    "single vector-valued.*j.*matrix = TRUE"
  )

  # Test wrong length j list
  expect_error(
    data$tfd_reg[1:3, list(c(0.1, 0.5), c(0.2, 0.8)), matrix = FALSE],
    "must be a single vector or a list of length"
  )
})

test_that("[.tf preserves attributes correctly", {
  data <- setup_bracket_test_data()

  # Test that subsetting preserves class
  subset_reg <- data$tfd_reg[1:3]
  expect_s3_class(subset_reg, c("tfd_reg", "tfd", "tf"))

  subset_irreg <- data$tfd_irreg[1:3]
  expect_s3_class(subset_irreg, c("tfd_irreg", "tfd", "tf"))

  # Test domain preservation
  expect_equal(tf_domain(subset_reg), tf_domain(data$tfd_reg))
  expect_equal(tf_domain(subset_irreg), tf_domain(data$tfd_irreg))

  # Test evaluator preservation
  expect_identical(
    attr(subset_reg, "evaluator"),
    attr(data$tfd_reg, "evaluator")
  )

  # Test names preservation
  expect_equal(
    names(data$tfd_reg[c("func_a", "func_c")]),
    c("func_a", "func_c")
  )

  # Test basis information preservation for tfb
  subset_tfb <- data$tfb_spl[1:2]
  expect_identical(
    attr(subset_tfb, "basis_args"),
    attr(data$tfb_spl, "basis_args")
  )
})

test_that("[<-.tf basic subassignment works", {
  data <- setup_bracket_test_data()

  # Test simple element replacement - values should match
  x <- data$tfd_reg
  original_evals <- tf_evaluations(data$tfd_reg[1])[[1]]
  x[2] <- data$tfd_reg[1]
  expect_equal(tf_evaluations(x[2])[[1]], original_evals)
  expect_s3_class(x, "tfd_reg")

  # Test multiple element replacement
  x <- data$tfd_reg
  orig_6 <- tf_evaluations(data$tfd_reg[6])[[1]]
  orig_7 <- tf_evaluations(data$tfd_reg[7])[[1]]
  x[2:3] <- data$tfd_reg[6:7]
  expect_equal(tf_evaluations(x[2])[[1]], orig_6)
  expect_equal(tf_evaluations(x[3])[[1]], orig_7)

  # Test replacement with missing i (all elements) - test values match
  x <- data$tfd_reg[1:3]
  replacement_all <- data$tfd_reg[6:8]
  orig_vals <- unname(lapply(tf_evaluations(replacement_all), identity))
  x[] <- replacement_all
  new_vals <- unname(lapply(tf_evaluations(x), identity))
  expect_equal(new_vals, orig_vals)

  # Test named replacement - values should match
  x <- data$tfd_reg
  orig_h <- tf_evaluations(data$tfd_reg["func_h"])[[1]]
  x["func_b"] <- data$tfd_reg["func_h"]
  expect_equal(tf_evaluations(x["func_b"])[[1]], orig_h)
})

test_that("[<-.tf handles type casting and warnings", {
  data <- setup_bracket_test_data()

  # Test assignment within same class (should work silently)
  x_reg <- data$tfd_reg
  x_reg[1] <- data$tfd_reg[2]
  expect_s3_class(x_reg, "tfd_reg")

  # Test assignment with compatible but different subclass (should error in current implementation)
  x_reg <- data$tfd_reg[1:3]
  expect_error(
    x_reg[1] <- data$tfd_irreg[1],
    "Can't combine"
  )

  # Test assignment with tfb to tfd (may warn about lossy cast)
  x_reg <- data$tfd_reg[1:3]
  # This may or may not warn depending on the internal casting behavior
  expect_warning(x_reg[1] <- data$tfb_spl[1], "casting")
  x_reg[1] <- suppressWarnings(data$tfb_spl[1])
  expect_s3_class(x_reg, "tfd_reg")
})

test_that("[<-.tf handles incompatible types", {
  data <- setup_bracket_test_data()

  # Test completely incompatible assignment (should error)
  x <- data$tfd_reg
  expect_error(
    x[1] <- "not a tf object",
    "Can't combine.*character"
  )

  # Test assignment that would change the base type
  x_tfb <- data$tfb_spl
  # This should error because it would change tfb_spline to tfd
  expect_error(
    x_tfb[1] <- data$tfd_reg[1],
    "Can't combine"
  )
})

test_that("[<-.tf handles edge cases", {
  data <- setup_bracket_test_data()

  # Test assignment to empty object
  x_empty <- data$empty_tfd
  expect_error(
    x_empty[1] <- data$tfd_reg[1],
    "past the end"
  )

  # Test self-assignment
  x <- data$tfd_reg
  x[1] <- x[1]
  expect_identical(x, data$tfd_reg)

  # Test assignment with NA values
  x <- data$tfd_reg
  x[2] <- data$na_tfd[2] # assign NA function
  expect_true(is.na(x[2]))

  # Test out-of-bounds assignment
  x <- data$tfd_reg
  expect_error(
    x[10] <- data$tfd_reg[1],
    "past the end"
  )

  # Test assignment with wrong size - it may actually succeed with recycling
  x <- data$tfd_reg
  # Assignment of single element to multiple positions may work
  expect_no_error(x[1:2] <- data$tfd_reg[1])
  expect_equal(tf_evaluations(x[1])[[1]], tf_evaluations(x[2])[[1]])
})

test_that("bracket operations work with irregular data", {
  data <- setup_bracket_test_data()

  # Test indexing irregular data
  irreg_subset <- data$tfd_irreg[1:3]
  expect_s3_class(irreg_subset, "tfd_irreg")
  expect_length(irreg_subset, 3)

  # Test evaluation of irregular data
  irreg_eval <- data$tfd_irreg[1, c(0.2, 0.5, 0.8)]
  expect_true(is.matrix(irreg_eval))
  expect_equal(dim(irreg_eval), c(1, 3))

  # Test sparse irregular data
  sparse_subset <- data$sparse_irreg[1:3]
  expect_s3_class(sparse_subset, "tfd_irreg")

  # Test assignment to irregular data
  x_irreg <- data$tfd_irreg
  x_irreg[1] <- data$tfd_irreg[2]
  expect_s3_class(x_irreg, "tfd_irreg")
})

# test_that("bracket operations handle large datasets", {
#   # Create larger dataset for performance testing
#   set.seed(3456)
#   large_data <- tf_rgp(100, arg = seq(0, 1, length.out = 101))
#
#   # Test indexing large dataset
#   expect_equal(length(large_data[1:50]), 50)
#   expect_equal(length(large_data[seq(1, 100, by = 2)]), 50)
#
#   # Test evaluation on large dataset
#   eval_large <- large_data[1:10, seq(0, 1, length.out = 21)]
#   expect_equal(dim(eval_large), c(10, 21))
#
#   # Test subassignment on large dataset - compare values
#   large_copy <- large_data
#   orig_vals <- unname(lapply(tf_evaluations(large_data[96:100]), identity))
#   large_copy[1:5] <- large_data[96:100]
#   new_vals <- unname(lapply(tf_evaluations(large_copy[1:5]), identity))
#   expect_equal(new_vals, orig_vals)
# })

test_that("bracket operations work correctly with all tf subclasses", {
  data <- setup_bracket_test_data()

  # Test matrix evaluation works for all classes
  classes_to_test <- list(
    tfd_reg = data$tfd_reg[1:3],
    tfd_irreg = data$tfd_irreg[1:3],
    tfb_spl = data$tfb_spl[1:3],
    tfb_fpc = data$tfb_fpc[1:3]
  )

  eval_points <- c(0.2, 0.5, 0.8)

  for (class_name in names(classes_to_test)) {
    obj <- classes_to_test[[class_name]]

    # Test evaluation
    eval_result <- obj[, eval_points]
    expect_true(
      is.matrix(eval_result),
      info = paste("Matrix evaluation failed for", class_name)
    )
    expect_equal(
      dim(eval_result),
      c(3, 3),
      info = paste("Wrong dimensions for", class_name)
    )

    # Test subassignment within same class
    obj[1] <- obj[2]
    expect_true(
      inherits(obj, class(classes_to_test[[class_name]])),
      info = paste("Class not preserved for", class_name)
    )
  }
})
