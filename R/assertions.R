domain_contains <- function(x, to) {
  dom_x <- tf_domain(x)
  dom_to <- tf_domain(to)
  (dom_to[1] <= dom_x[1]) && (dom_to[2] >= dom_x[2])
}

assert_domain_x_in_to <- function(x, to) {
  # can (try to) cast losslessly if domain of 'to' contains domain of 'x'

  if (domain_contains(x, to)) {
    return(TRUE)
  }

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

assert_arg <- function(arg, x, check_unique = TRUE, null_ok = FALSE) {
  if (null_ok && is.null(arg)) {
    return()
  }
  if (is.list(arg)) {
    assert_true(length(arg) %in% c(1, length(x)))
    walk(arg, \(arg) {
      if (!is.null(arg)) {
        assert_arg_vector(arg, x = x, check_unique = check_unique)
      }
    })
  } else {
    assert_arg_vector(arg, x, check_unique = check_unique)
  }
}

assert_arg_vector <- function(arg, x, check_unique = TRUE) {
  domain_x <- tf_domain(x)
  assert_numeric(
    arg,
    lower = domain_x[1],
    upper = domain_x[2],
    any.missing = FALSE,
    unique = check_unique,
    sorted = TRUE
  )
}

assert_compatible_size <- function(op, x, y) {
  x_size <- vec_size(x)
  y_size <- vec_size(y)
  if (!(x_size == y_size || 1 %in% c(x_size, y_size))) {
    message <- cli::format_inline(
      "incompatible vector sizes in {.cls {vec_ptype_full(x)}}[1:{x_size}] {op} ",
      "{.cls {vec_ptype_full(y)}}[1:{y_size}] --",
      "\n{.pkg tf} does not recycle arguments."
    )
    stop_incompatible_op(op, x, y, message = message)
  }
}

assert_tf <- function(x, .var.name = vname(x)) {
  assert_class(x, "tf", .var.name = .var.name)
}

assert_tfd <- function(x, null_ok = FALSE, .var.name = vname(x)) {
  assert_class(x, "tfd", null.ok = null_ok, .var.name = .var.name)
}

assert_tfb <- function(x, .var.name = vname(x)) {
  assert_class(x, "tfb", .var.name = .var.name)
}

assert_tf_mv <- function(x, .var.name = vname(x)) {
  assert_class(x, "tf_mv", .var.name = .var.name)
}

# "strict" does not allow stretching/compressing or truncation of domain
# (i.e. strict allows only bijective time transformations)
assert_warp <- function(warp, x, strict = FALSE) {
  assert_tfd(warp)
  if (length(x) != length(warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same length.")
  }
  domain_x <- tf_domain(x)
  domain_warp <- tf_domain(warp)
  if (!all(domain_x == domain_warp)) {
    cli::cli_abort("{.arg x} and {.arg warp} must have the same domain.")
  }
  assert_monotonic(warp)
  if (strict) {
    bad_range <- !map_lgl(tf_frange(warp), \(x) all(x == domain_x))
    if (any(bad_range)) {
      cli::cli_abort(
        "{.arg warp} domain and range must be the same. Not equal at index: {.val {which(bad_range)}}."
      )
    }
  }
  invisible(warp)
}

assert_monotonic <- function(x, .var.name = vname(x)) {
  bad <- !map_lgl(tf_evaluations(x), is_monotonic)
  if (any(bad)) {
    cli::cli_abort(
      "{.arg { .var.name}} must be monotonic. Not monotonic at index: {.val {which(bad)}}."
    )
  }
  invisible(x)
}

# Internal validator for tf and tf_mv objects.
#
# Cheap, deep, recursive integrity check intended for use in tests, not in
# production code. Returns `TRUE` invisibly on success; aborts on the first
# failed invariant with a `cli::cli_abort` message naming the broken contract.
#
# @param x A `tf` or `tf_mv` object.
# @return `TRUE` invisibly on success.
# @keywords internal
validate_tf <- function(x) {
  if (inherits(x, "tf_mv")) {
    return(validate_tf_mv(x))
  }
  if (!inherits(x, "tf")) {
    cli::cli_abort(
      "{.arg x} is not a {.cls tf} object (class: {.val {class(x)}})."
    )
  }
  # ---- domain (all tf subclasses) -----------------------------------------
  domain <- attr(x, "domain")
  # length-0 prototypes legitimately carry a degenerate domain — c(0, 0) (tfb
  # path) or c(NA, NA) (tfd path, matching the S4 prototype); only require
  # the structural shape (length-2 numeric) for prototypes.
  is_proto <- length(unclass(x)) == 0L
  bad_domain <- !is.numeric(domain) || length(domain) != 2L ||
    (!is_proto && (anyNA(domain) || any(!is.finite(domain)) ||
       domain[1] > domain[2] || length(unique(domain)) != 2L))
  if (bad_domain) {
    cli::cli_abort(paste0(
      "Invalid {.field domain}: must be a finite, sorted length-2 numeric ",
      "with distinct endpoints; got {.val {domain}}."
    ))
  }
  # ---- element names ------------------------------------------------------
  nms <- names(x)
  if (!is.null(nms)) {
    if (length(nms) != length(unclass(x))) {
      cli::cli_abort(paste0(
        "{.field names(x)} length {.val {length(nms)}} does not match ",
        "data length {.val {length(unclass(x))}}."
      ))
    }
    if (anyNA(nms)) {
      cli::cli_abort("{.field names(x)} contains {.code NA}.")
    }
  }
  # ---- dispatch on subclass ----------------------------------------------
  if (inherits(x, "tfd_reg")) {
    validate_tfd_reg(x)
  } else if (inherits(x, "tfd_irreg")) {
    validate_tfd_irreg(x)
  } else if (inherits(x, "tfb_spline")) {
    validate_tfb_spline(x)
  } else if (inherits(x, "tfb_fpc")) {
    validate_tfb_fpc(x)
  } else {
    cli::cli_abort(
      "Unknown {.cls tf} subclass: {.val {class(x)}}."
    )
  }
  invisible(TRUE)
}

# helper: assert arg vector is sorted, finite, in domain
validate_arg_vector <- function(arg, domain, where, check_unique = TRUE) {
  if (!is.numeric(arg)) {
    cli::cli_abort("{where}: {.field arg} must be numeric.")
  }
  if (length(arg) == 0L) return(invisible(TRUE))
  if (anyNA(arg) || any(!is.finite(arg))) {
    cli::cli_abort("{where}: {.field arg} contains non-finite values.")
  }
  if (is.unsorted(arg)) {
    cli::cli_abort("{where}: {.field arg} is not sorted.")
  }
  if (check_unique && anyDuplicated(arg)) {
    cli::cli_abort("{where}: {.field arg} contains duplicates.")
  }
  if (min(arg) < domain[1] || max(arg) > domain[2]) {
    cli::cli_abort(paste0(
      "{where}: {.field arg} values outside {.field domain} ",
      "[{domain[1]}, {domain[2]}]."
    ))
  }
  invisible(TRUE)
}

validate_tfd_reg <- function(x) {
  # required attrs
  required <- c("evaluator", "evaluator_name", "arg", "domain")
  missing_attrs <- setdiff(required, names(attributes(x)))
  if (length(missing_attrs)) {
    cli::cli_abort(
      "{.cls tfd_reg}: missing required attribute(s) {.val {missing_attrs}}."
    )
  }
  if (!is.function(attr(x, "evaluator"))) {
    cli::cli_abort("{.cls tfd_reg}: {.field evaluator} must be a function.")
  }
  if (!is.character(attr(x, "evaluator_name"))) {
    cli::cli_abort(
      "{.cls tfd_reg}: {.field evaluator_name} must be character."
    )
  }
  arg_attr <- attr(x, "arg")
  if (!is.list(arg_attr) || length(arg_attr) != 1L) {
    cli::cli_abort(
      "{.cls tfd_reg}: {.field arg} attribute must be a length-1 list."
    )
  }
  domain <- attr(x, "domain")
  validate_arg_vector(arg_attr[[1]], domain, where = "tfd_reg")
  # element lengths match shared arg length (NULL entries = NA functions OK)
  data <- unclass(x)
  expected_len <- length(arg_attr[[1]])
  for (i in seq_along(data)) {
    el <- data[[i]]
    if (is.null(el)) next
    if (!is.numeric(el)) {
      cli::cli_abort(
        "{.cls tfd_reg}: element {.val {i}} is not numeric."
      )
    }
    if (length(el) != expected_len) {
      cli::cli_abort(paste0(
        "{.cls tfd_reg}: element {.val {i}} has length {.val {length(el)}}, ",
        "expected {.val {expected_len}} (= length of shared arg)."
      ))
    }
  }
  invisible(TRUE)
}

validate_tfd_irreg <- function(x) {
  required <- c("evaluator", "evaluator_name", "domain")
  missing_attrs <- setdiff(required, names(attributes(x)))
  if (length(missing_attrs)) {
    cli::cli_abort(
      "{.cls tfd_irreg}: missing required attribute(s) {.val {missing_attrs}}."
    )
  }
  if (!is.function(attr(x, "evaluator"))) {
    cli::cli_abort("{.cls tfd_irreg}: {.field evaluator} must be a function.")
  }
  if (!is.character(attr(x, "evaluator_name"))) {
    cli::cli_abort(
      "{.cls tfd_irreg}: {.field evaluator_name} must be character."
    )
  }
  domain <- attr(x, "domain")
  data <- unclass(x)
  for (i in seq_along(data)) {
    el <- data[[i]]
    if (is.null(el)) next
    if (!is.list(el)) {
      cli::cli_abort(paste0(
        "{.cls tfd_irreg}: element {.val {i}} must be a list (got ",
        "{.cls {class(el)}})."
      ))
    }
    expected_fields <- c("arg", "value")
    if (!all(expected_fields %in% names(el))) {
      cli::cli_abort(paste0(
        "{.cls tfd_irreg}: element {.val {i}} must be a list with fields ",
        "{.val arg} and {.val value}; got {.val {names(el)}}."
      ))
    }
    if (length(el$arg) != length(el$value)) {
      cli::cli_abort(paste0(
        "{.cls tfd_irreg}: element {.val {i}} has length(arg) = ",
        "{.val {length(el$arg)}} != length(value) = {.val {length(el$value)}}."
      ))
    }
    validate_arg_vector(
      el$arg, domain,
      where = paste0("tfd_irreg element ", i),
      check_unique = TRUE
    )
  }
  invisible(TRUE)
}

validate_tfb_spline <- function(x) {
  # length-0 prototypes from new_tfb_spline(numeric(0)) legitimately carry
  # only domain/arg/family, so basis attributes can't be required for them
  # (the universal domain/names checks in validate_tf() still apply).
  if (length(unclass(x)) == 0L) {
    return(invisible(TRUE))
  }
  required <- c(
    "basis", "basis_matrix", "basis_label", "basis_args",
    "arg", "family", "family_label", "domain"
  )
  missing_attrs <- setdiff(required, names(attributes(x)))
  if (length(missing_attrs)) {
    cli::cli_abort(paste0(
      "{.cls tfb_spline}: missing required attribute(s) ",
      "{.val {missing_attrs}}."
    ))
  }
  if (!is.function(attr(x, "basis"))) {
    cli::cli_abort("{.cls tfb_spline}: {.field basis} must be a function.")
  }
  bmat <- attr(x, "basis_matrix")
  arg <- attr(x, "arg")
  domain <- attr(x, "domain")
  # arg is a flat numeric vector for tfb_spline
  if (!is.numeric(arg)) {
    cli::cli_abort("{.cls tfb_spline}: {.field arg} must be numeric.")
  }
  validate_arg_vector(arg, domain, where = "tfb_spline")
  if (!is.matrix(bmat) || !is.numeric(bmat)) {
    cli::cli_abort(
      "{.cls tfb_spline}: {.field basis_matrix} must be a numeric matrix."
    )
  }
  if (nrow(bmat) != length(arg)) {
    cli::cli_abort(paste0(
      "{.cls tfb_spline}: nrow(basis_matrix) = {.val {nrow(bmat)}} != ",
      "length(arg) = {.val {length(arg)}}."
    ))
  }
  data <- unclass(x)
  expected_len <- ncol(bmat)
  for (i in seq_along(data)) {
    el <- data[[i]]
    if (is.null(el)) next
    if (!is.numeric(el)) {
      cli::cli_abort(
        "{.cls tfb_spline}: coefficient {.val {i}} is not numeric."
      )
    }
    if (length(el) != expected_len) {
      cli::cli_abort(paste0(
        "{.cls tfb_spline}: coefficient {.val {i}} has length ",
        "{.val {length(el)}}, expected ncol(basis_matrix) = ",
        "{.val {expected_len}}."
      ))
    }
  }
  invisible(TRUE)
}

validate_tfb_fpc <- function(x) {
  # length-0 prototypes from new_tfb_fpc(numeric(0)) legitimately carry only
  # domain/arg/score_variance, so basis attributes can't be required for them
  # (the universal domain/names checks in validate_tf() still apply).
  if (length(unclass(x)) == 0L) {
    return(invisible(TRUE))
  }
  required <- c(
    "basis", "basis_matrix", "basis_label",
    "arg", "score_variance", "scoring_function", "domain"
  )
  missing_attrs <- setdiff(required, names(attributes(x)))
  if (length(missing_attrs)) {
    cli::cli_abort(
      "{.cls tfb_fpc}: missing required attribute(s) {.val {missing_attrs}}."
    )
  }
  if (!is.function(attr(x, "basis"))) {
    cli::cli_abort("{.cls tfb_fpc}: {.field basis} must be a function.")
  }
  if (!is.function(attr(x, "scoring_function"))) {
    cli::cli_abort(
      "{.cls tfb_fpc}: {.field scoring_function} must be a function."
    )
  }
  bmat <- attr(x, "basis_matrix")
  arg <- attr(x, "arg")
  domain <- attr(x, "domain")
  if (!is.numeric(arg)) {
    cli::cli_abort("{.cls tfb_fpc}: {.field arg} must be numeric.")
  }
  validate_arg_vector(arg, domain, where = "tfb_fpc")
  if (!is.matrix(bmat) || !is.numeric(bmat)) {
    cli::cli_abort(
      "{.cls tfb_fpc}: {.field basis_matrix} must be a numeric matrix."
    )
  }
  if (nrow(bmat) != length(arg)) {
    cli::cli_abort(paste0(
      "{.cls tfb_fpc}: nrow(basis_matrix) = {.val {nrow(bmat)}} != ",
      "length(arg) = {.val {length(arg)}}."
    ))
  }
  data <- unclass(x)
  expected_len <- ncol(bmat)
  for (i in seq_along(data)) {
    el <- data[[i]]
    if (is.null(el)) next
    if (!is.numeric(el)) {
      cli::cli_abort(
        "{.cls tfb_fpc}: coefficient {.val {i}} is not numeric."
      )
    }
    if (length(el) != expected_len) {
      cli::cli_abort(paste0(
        "{.cls tfb_fpc}: coefficient {.val {i}} has length ",
        "{.val {length(el)}}, expected ncol(basis_matrix) = ",
        "{.val {expected_len}}."
      ))
    }
  }
  # score_variance should be ncol(basis_matrix) - 1 (one column is the mean)
  sv <- attr(x, "score_variance")
  if (!is.numeric(sv)) {
    cli::cli_abort("{.cls tfb_fpc}: {.field score_variance} must be numeric.")
  }
  expected_sv_len <- ncol(bmat) - 1L
  if (length(sv) != expected_sv_len) {
    cli::cli_abort(paste0(
      "{.cls tfb_fpc}: {.field score_variance} has length ",
      "{.val {length(sv)}}, expected ncol(basis_matrix) - 1 = ",
      "{.val {expected_sv_len}}."
    ))
  }
  invisible(TRUE)
}

validate_tf_mv <- function(x) {
  if (!inherits(x, "tf_mv")) {
    cli::cli_abort("{.arg x} is not a {.cls tf_mv} object.")
  }
  comps <- attr(x, "components")
  if (!is.list(comps)) {
    cli::cli_abort("{.cls tf_mv}: {.field components} must be a list.")
  }
  n <- length(unclass(x))
  # dummy payload length must match component count of curves (each component
  # has length n; payload is also length n). Spec phrased this as
  # "length(unclass(x)) == nrow(attr(x, 'components'))"; structurally the
  # invariant is `length(unclass(x)) == vec_size(components[[i]])` for each i,
  # which the constructor enforces.
  domain <- attr(x, "domain")
  is_proto <- length(unclass(x)) == 0L
  bad_domain <- !is.numeric(domain) || length(domain) != 2L ||
    any(!is.finite(domain)) || domain[1] > domain[2] ||
    (!is_proto && length(unique(domain)) != 2L)
  if (bad_domain) {
    cli::cli_abort(paste0(
      "{.cls tf_mv}: invalid {.field domain}: must be a finite sorted ",
      "length-2 numeric with distinct endpoints; got {.val {domain}}."
    ))
  }
  comp_names <- attr(x, "comp_names")
  if (length(comps)) {
    lens <- vapply(comps, length, integer(1))
    if (any(lens != n)) {
      cli::cli_abort(paste0(
        "{.cls tf_mv}: payload length {.val {n}} does not match component ",
        "lengths {.val {lens}}."
      ))
    }
    # all components valid tf, recursively
    for (i in seq_along(comps)) {
      if (!inherits(comps[[i]], "tf")) {
        cli::cli_abort(
          "{.cls tf_mv}: component {.val {i}} is not a {.cls tf} object."
        )
      }
      validate_tf(comps[[i]])
    }
    # all components share the same domain (the constructor enforces this
    # by widening, so verify the invariant holds)
    comp_domains <- lapply(comps, tf_domain)
    first_d <- comp_domains[[1]]
    for (i in seq_along(comp_domains)[-1]) {
      if (!isTRUE(all.equal(comp_domains[[i]], first_d))) {
        cli::cli_abort(paste0(
          "{.cls tf_mv}: component {.val {i}} domain {.val ",
          "{comp_domains[[i]]}} differs from component 1 domain ",
          "{.val {first_d}}."
        ))
      }
    }
    # also: tf_mv domain must agree with components' shared domain
    if (!isTRUE(all.equal(first_d, domain))) {
      cli::cli_abort(paste0(
        "{.cls tf_mv}: {.field domain} {.val {domain}} differs from ",
        "components' shared domain {.val {first_d}}."
      ))
    }
    # comp_names matches names(components)
    if (!identical(comp_names, names(comps))) {
      cli::cli_abort(paste0(
        "{.cls tf_mv}: {.field comp_names} {.val {comp_names}} does not ",
        "match names(components) {.val {names(comps)}}."
      ))
    }
  }
  invisible(TRUE)
}

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
