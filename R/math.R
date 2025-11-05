# utility function for linear operations that can be done on coefs or
#   evaluations directly.
fun_math <- function(x, op) {
  attr_ret <- attributes(x)
  ret <- map(tf_evaluations(x), \(x) {
    if (is.null(x)) return(NULL)  # Preserve NULL for NA entries
    result <- do.call(op, list(x = x))
    # Convert all-NA/NaN results to NULL (NA.tf should be NULL entry)
    if (all(is.na(result))) NULL else result
  })
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, \(x, y) {
      if (is.null(y)) NULL else list(arg = x, value = y)
    })
  }
  attributes(ret) <- attr_ret
  ret
}
#------------------------------------------------------------------------------

#' @include ops.R
NULL

#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute functions
Math.tfd <- function(x, ...) {
  fun_math(x, .Generic)
}

#' @rdname tfgroupgenerics
#' @export
Math.tfb <- function(x, ...) {
  genericname <- .Generic
  cli::cli_warn(
    "Potentially lossy cast to {.cls tfd} and back in {genericname}({.cls {vec_ptype_full(x)}})"
  )
  basis_args <- attr(x, "basis_args")
  eval <- fun_math(tfd(x), .Generic)

  # Check for NULL entries (NA functions) - can't convert those to tfb
  na_entries <- is.na(eval)
  if (all(na_entries)) {
    # All entries are NA - return tfb with NULL entries
    result <- vector("list", length(eval))
    result[] <- list(NULL)
    names(result) <- names(eval)

    # Copy attributes from original tfb to maintain tfb structure
    attributes(result) <- c(attributes(result), attributes(x)[!names(attributes(x)) %in% c("names", "class")])
    class(result) <- class(x)

    return(result)
  }

  if (any(na_entries)) {
    # Some entries are NA - convert only non-NA entries to tfb
    non_na_eval <- eval[!na_entries]
    tfb_non_na <- do.call(
      "tfb",
      c(list(non_na_eval), basis_args, penalized = FALSE, verbose = FALSE)
    )

    # Insert NULLs back at NA positions
    result <- vector("list", length(eval))
    result[!na_entries] <- unclass(tfb_non_na)
    result[na_entries] <- list(NULL)
    names(result) <- names(eval)

    # Copy attributes from tfb_non_na and adjust class
    attributes(result) <- c(attributes(result), attributes(tfb_non_na)[!names(attributes(tfb_non_na)) %in% c("names", "class")])
    class(result) <- class(tfb_non_na)

    return(result)
  }

  # No NA entries, proceed normally
  do.call(
    "tfb",
    c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
  )
}

#-------------------------------------------------------------------------------
# TODO:
# inner product?
# `%*%.default` = .Primitive("%*%") # assign default as current definition
# `%*%` = function(x,...){ #make S3
#  UseMethod("%*%",x)
# }
# `%*%.tf(x, y) = [int x_i(t)*y_i(t) dt]
