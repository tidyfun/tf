# Test print and format methods for tf objects

# Setup test data for all tf classes
setup_test_data <- function() {
  set.seed(1234)

  # Regular tfd data
  t_reg <- seq(0, 1, length.out = 51)
  data_reg <- lapply(1:5, function(i) sin(i * pi * t_reg) + rnorm(51, 0, 0.1))
  tfd_reg <- tfd(data_reg, arg = t_reg)
  names(tfd_reg) <- paste0("f", 1:5)

  # Irregular tfd data
  tfd_irreg <- tf_jiggle(tfd_reg, amount = 0.1)

  # tfb_spline data
  tfb_spl <- tfb(tfd_reg, verbose = FALSE, penalized = FALSE)

  # tfb_fpc data
  tfb_fpc_obj <- tfb_fpc(tfd_reg, verbose = FALSE)

  # Edge case data
  empty_tfd <- tfd_reg[integer(0)]
  single_tfd <- tfd_reg[1]
  na_tfd <- tfd_reg
  na_tfd[[1]] <- NA

  # Data with extreme values
  extreme_data <- lapply(1:3, function(i) c(-1e6, 0, 1e6))
  extreme_tfd <- tfd(extreme_data, arg = 1:3)

  list(
    tfd_reg = tfd_reg,
    tfd_irreg = tfd_irreg,
    tfb_spl = tfb_spl,
    tfb_fpc = tfb_fpc_obj,
    empty_tfd = empty_tfd,
    single_tfd = single_tfd,
    na_tfd = na_tfd,
    extreme_tfd = extreme_tfd
  )
}

test_that("print.tf works for all tf classes", {
  data <- setup_test_data()

  # Test basic printing doesn't error
  expect_output(print(data$tfd_reg), "tfd\\[5\\]")
  expect_output(print(data$tfd_irreg), "irregular")
  expect_output(print(data$tfb_spl), "basis representation")
  expect_output(print(data$tfb_fpc), "basis representation")

  # Test print returns object invisibly
  expect_identical(print(data$single_tfd), data$single_tfd)

  # Test empty objects - they have range issues, so just check they don't error
  suppressWarnings(expect_output(print(data$empty_tfd), "tfd\\[0\\]"))

  suppressWarnings(expect_output(print(data$na_tfd), "tfd\\[5\\]"))

  # Test domain and range information is included
  expect_output(print(data$tfd_reg), "\\[0,1\\]")
  expect_output(print(data$extreme_tfd), "\\[-1e\\+06,1e\\+06\\]")
})

test_that("print.tfd_reg works correctly", {
  data <- setup_test_data()

  # Test evaluation count information
  expect_output(print(data$tfd_reg), "based on 51 evaluations each")

  # Test interpolation method display
  expect_output(print(data$tfd_reg), "interpolation by")

  # Test different n values
  expect_output(print(data$tfd_reg, n = 2), "3 not shown")
  output_n10 <- capture.output(print(data$tfd_reg, n = 10))
  expect_false(any(grepl("not shown", output_n10))) # shouldn't show "not shown"

  # Test with empty object - it produces warnings due to range calculations
  suppressWarnings(expect_output(print(data$empty_tfd), "tfd\\[0\\]"))

  # Test sparkline display
  if (cli::is_utf8_output()) {
    expect_output(print(data$tfd_reg), "▁▂▃▅▆▇█")
  }
})

test_that("print.tfd_irreg works correctly", {
  data <- setup_test_data()

  # Test irregular data specific output
  expect_output(print(data$tfd_irreg), "irregular")
  expect_output(print(data$tfd_irreg), "to.*evaluations each")
  expect_output(print(data$tfd_irreg), "mean:")

  # Test with mostly NA irregular data
  mostly_na <- data$tfd_irreg
  mostly_na[1:4] <- NA
  expect_output(print(mostly_na), "irregular")

  # Test completely NA irregular data
  all_na <- data$tfd_irreg
  all_na[] <- NA
  suppressWarnings(expect_output(print(all_na), "irregular"))
})

test_that("print.tfb works correctly", {
  data <- setup_test_data()

  # Test basis information display
  expect_output(print(data$tfb_spl), "basis representation")
  expect_output(print(data$tfb_spl), "using.*s\\(")

  # Test FPC basis display
  expect_output(print(data$tfb_fpc), "FPC")

  # Test empty tfb
  empty_tfb <- data$tfb_spl[integer(0)]
  suppressWarnings(expect_output(print(empty_tfb), "\\."))

  # Test truncation message
  long_tfb <- tfb(tf_rgp(10), verbose = FALSE)
  expect_output(print(long_tfb, n = 3), "not shown")
})

test_that("format.tf works with sparkline option", {
  data <- setup_test_data()

  # Test sparkline vs string representation
  if (cli::is_utf8_output()) {
    spark_format <- format(data$tfd_reg, sparkline = TRUE)
    expect_true(any(grepl("▁▂▃▅▆▇█", spark_format)))

    string_format <- format(data$tfd_reg, sparkline = FALSE)
    expect_true(any(grepl("\\(.*,.*\\)", string_format)))
    expect_false(any(grepl("▁▂▃▅▆▇█", string_format)))
  }

  # Test irregular data (should not use sparkline)
  irreg_format <- format(data$tfd_irreg, sparkline = TRUE)
  expect_true(any(grepl("\\(.*,.*\\)", irreg_format)))
})

test_that("format.tf works with different options", {
  data <- setup_test_data()

  # Test different bin counts
  format_5bins <- format(data$tfd_reg, bins = 5)
  format_20bins <- format(data$tfd_reg, bins = 20)
  if (cli::is_utf8_output()) {
    expect_true(nchar(format_5bins[1]) < nchar(format_20bins[1]))
  }

  # Test digits and nsmall - use regular data for more reliable comparison
  format_2dig <- format(data$tfd_reg, digits = 2, sparkline = FALSE)
  format_6dig <- format(data$tfd_reg, digits = 6, sparkline = FALSE)
  # With more digits, some numbers should be longer
  expect_true(any(nchar(format_6dig) >= nchar(format_2dig)))

  # Test width truncation
  format_narrow <- format(data$tfd_reg, width = 20)
  expect_true(any(nchar(format_narrow) <= 20))
  expect_true(any(grepl("\\.\\.\\.$", format_narrow)))

  # Test prefix option
  format_prefix <- format(data$tfd_reg, prefix = TRUE)
  expect_true(any(grepl("^f[0-9]+:", format_prefix)))

  format_no_prefix <- format(data$tfd_reg, prefix = FALSE)
  expect_false(any(grepl("^f[0-9]+:", format_no_prefix)))
})

test_that("format.tf handles edge cases", {
  data <- setup_test_data()

  # Test empty objects
  empty_format <- suppressWarnings(format(data$empty_tfd))
  expect_length(empty_format, 0)

  # Test single element
  single_format <- format(data$single_tfd)
  expect_length(single_format, 1)

  # Test NA functions
  na_format <- format(data$na_tfd, sparkline = FALSE)
  expect_true(any(grepl("NA", na_format)))

  # Test unnamed objects
  unnamed_data <- data$tfd_reg
  names(unnamed_data) <- NULL
  unnamed_format <- format(unnamed_data, prefix = TRUE)
  expect_true(any(grepl("^\\[[0-9]+\\]:", unnamed_format)))

  # Test with NULL names but some empty
  partial_names <- data$tfd_reg
  names(partial_names)[2:3] <- ""
  partial_format <- format(partial_names, prefix = TRUE)
  expect_true(any(grepl("^\\[[0-9]+\\]:", partial_format)))
})

test_that("string_rep_tf works correctly", {
  data <- setup_test_data()

  # Access internal function
  string_rep_tf <- tf:::string_rep_tf

  # Test basic string representation
  str_rep <- string_rep_tf(data$tfd_reg, show = 3)
  expect_true(all(grepl("\\(.*,.*\\)", str_rep)))
  expect_true(any(grepl(";.*\\.\\.\\.", str_rep))) # truncation indicator

  # Test with different show values
  str_rep_1 <- string_rep_tf(data$tfd_reg, show = 1)
  str_rep_all <- string_rep_tf(data$tfd_reg, show = 100)
  expect_true(all(nchar(str_rep_1) <= nchar(str_rep_all)))

  # Test with NA functions
  str_na <- string_rep_tf(data$na_tfd)
  expect_true(any(str_na == "NA"))

  # Test with extreme values
  str_extreme <- string_rep_tf(data$extreme_tfd, digits = 2)
  expect_true(any(grepl("-1e", str_extreme)))

  # Test signif_arg parameter
  str_signif <- string_rep_tf(data$tfd_reg, signif_arg = 3, digits = 3)
  expect_true(all(nchar(str_signif) > 0))
})

test_that("spark_rep_tf works correctly", {
  data <- setup_test_data()

  # Access internal function
  spark_rep_tf <- tf:::spark_rep_tf

  # Test basic sparkline generation
  if (cli::is_utf8_output()) {
    spark_rep <- spark_rep_tf(data$tfd_reg, bins = 10)
    expect_true(all(nchar(spark_rep) > 0))
    expect_true(any(grepl("▁▂▃▅▆▇█", spark_rep)))
  }

  # Test with different bin counts
  spark_5 <- spark_rep_tf(data$tfd_reg, bins = 5)
  spark_20 <- spark_rep_tf(data$tfd_reg, bins = 20)
  if (cli::is_utf8_output()) {
    expect_true(nchar(spark_5[1]) <= nchar(spark_20[1]))
  }

  # Test with bins = -1 (no binning)
  spark_full <- spark_rep_tf(data$tfd_reg, bins = -1)
  if (cli::is_utf8_output()) {
    expect_true(all(nchar(spark_full) > 10)) # should be longer
  }

  # Test with NA functions
  spark_na <- spark_rep_tf(data$na_tfd)
  expect_true(any(spark_na == "NA"))

  # Test custom scale_f
  custom_scale <- c(-2, 2)
  spark_scaled <- spark_rep_tf(data$tfd_reg, scale_f = custom_scale)
  if (cli::is_utf8_output()) {
    expect_true(all(nchar(spark_scaled) > 0))
  }
})

test_that("format_glimpse.tf works correctly", {
  data <- setup_test_data()

  # Access internal function
  format_glimpse.tf <- tf:::format_glimpse.tf

  # Test glimpse formatting
  glimpse_format <- format_glimpse.tf(data$tfd_reg)
  expect_length(glimpse_format, length(data$tfd_reg))

  # Test default bins (should be 8)
  if (cli::is_utf8_output()) {
    expect_true(all(nchar(glimpse_format) <= 20)) # compact display
  }

  # Test with custom options
  glimpse_custom <- format_glimpse.tf(data$tfd_reg, bins = 15, digits = 3)
  if (cli::is_utf8_output()) {
    expect_true(any(nchar(glimpse_custom) != nchar(glimpse_format)))
  }
})

test_that("print methods handle special numeric values", {
  # Create data with special values - avoid NaN as it creates NA entries
  special_data <- list(
    c(0, Inf, -Inf),
    c(1e-10, 1e10, 0),
    c(-1, 0, 1)
  )
  special_tfd <- suppressWarnings(tfd(special_data, arg = 1:3))

  # Test printing doesn't error with special values
  expect_output(print(special_tfd), "tfd\\[")

  # Test formatting with special values
  format_special <- format(special_tfd, sparkline = FALSE)
  expect_true(any(grepl("Inf", format_special)))
})

test_that("print methods work with different locales and options", {
  data <- setup_test_data()

  # Test with different digit options
  old_digits <- options(digits = 3)
  on.exit(options(old_digits))

  expect_output(print(data$extreme_tfd), "-1e\\+06")

  options(digits = 10)
  # Scientific notation is still used for large numbers
  expect_output(print(data$extreme_tfd), "1e\\+06")

  # Test format with different width options
  old_width <- options(width = 40)
  on.exit(options(old_width), add = TRUE)

  narrow_format <- format(data$tfd_reg)
  expect_true(all(nchar(narrow_format) <= 40))
})

test_that("print methods preserve object attributes", {
  data <- setup_test_data()

  # Test that printing doesn't modify the object
  original_attrs <- attributes(data$tfd_reg)
  printed_obj <- print(data$tfd_reg)
  expect_identical(attributes(printed_obj), original_attrs)

  # Test that formatting doesn't modify the object
  original_tfb_attrs <- attributes(data$tfb_spl)
  formatted <- format(data$tfb_spl)
  expect_identical(attributes(data$tfb_spl), original_tfb_attrs)
})
