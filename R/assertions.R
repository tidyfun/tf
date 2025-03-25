domain_contains <- function(x, to) {
  dom_x <- tf_domain(x)
  dom_to <- tf_domain(to)
  (dom_to[1] <= dom_x[1]) && (dom_to[2] >= dom_x[2])
}

assert_domain_x_in_to <- function(x, to) {
  # can (try to) cast losslessly if domain of 'to' contains domain of 'x'

  if (domain_contains(x, to)) return(TRUE)

  stop_incompatible_cast(
    x = x,
    to = to,
    x_arg = "",
    to_arg = "",
    details = "domains not compatible"
  )
}

assert_same_domains <- function(x, to) {
  if (all(tf_domain(x) == tf_domain(to))) {
    return(TRUE)
  }
  stop_incompatible_cast(
    x = x,
    to = to,
    x_arg = "",
    to_arg = "",
    details = "domains not identical"
  )
}

assert_arg <- function(arg, x, check_unique = TRUE) {
  if (is.list(arg)) {
    assert_true(length(arg) %in% c(1, length(x)))
    walk(arg, \(arg) assert_arg_vector(arg, x = x, check_unique = check_unique))
  } else {
    assert_arg_vector(arg, x, check_unique = check_unique)
  }
}

assert_arg_vector <- function(arg, x, check_unique = TRUE) {
  domain_x <- tf_domain(x)
  assert_numeric(
    arg,
    any.missing = FALSE,
    unique = check_unique,
    sorted = TRUE,
    lower = domain_x[1],
    upper = domain_x[2]
  )
}

assert_compatible_size <- function(op, x, y) {
  x_size <- vec_size(x)
  y_size <- vec_size(y)
  if (!(x_size == y_size || 1 %in% c(x_size, y_size))) {
    message <- cli::format_inline(
      "incompatible vector sizes in {.cls {vec_ptype_full(x)}}[1:{x_size}] {op} ",
      "{.cls {vec_ptype_full(y)}}[1:{y_size}] --",
      "\n{{tf}} does not recycle arguments."
    )
    stop_incompatible_op(op, x, y, message = message)
  }
}

assert_tf <- function(x) assert_class(x, "tf")

assert_tfd <- function(x) assert_class(x, "tfd")

assert_tfb <- function(x) assert_class(x, "tfb")

check_limit <- function(x, f) {
  domain <- tf_domain(f)
  res <- check_numeric(
    x,
    lower = domain[1],
    upper = domain[2],
    any.missing = FALSE
  )
  if (!isTRUE(res)) {
    "Integration limit must be numeric and within the domain"
  } else if (!length(x) %in% c(1, length(f))) {
    "Integration limit length must be 1 or equal to the number of functions"
  } else {
    TRUE
  }
}

assert_limit <- makeAssertionFunction(check_limit)
