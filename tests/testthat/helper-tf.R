# Test helper: expect that a tf / tf_mv object satisfies all internal
# invariants checked by validate_tf().
expect_valid_tf <- function(object, ...) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  res <- tryCatch(validate_tf(act$val), error = identity)
  if (inherits(res, "error")) {
    testthat::fail(
      paste0(act$lab, " is not a valid tf: ", conditionMessage(res))
    )
  } else {
    testthat::succeed()
  }
  invisible(act$val)
}
