# compute derivatives of data in rows by centered finite differences.
# uses the second-order accurate formula for non-uniform grids from
# numpy.gradient (see Fornberg, 1988).
derive_matrix <- function(data, arg, order) {
  for (i in seq_len(order)) {
    n <- length(arg)
    if (n < 2) {
      cli::cli_abort("Must have at least 2 grid points for differentiation.")
    }
    deriv <- matrix(NA_real_, nrow = nrow(data), ncol = n)
    if (n == 2) {
      # only forward difference possible
      deriv[, 1] <- deriv[, 2] <- (data[, 2] - data[, 1]) / (arg[2] - arg[1])
    } else {
      # boundaries: 2nd-order accurate one-sided differences (non-uniform grid)
      # left boundary
      dx1 <- arg[2] - arg[1]
      dx2 <- arg[3] - arg[2]
      a <- -(2 * dx1 + dx2) / (dx1 * (dx1 + dx2))
      b <- (dx1 + dx2) / (dx1 * dx2)
      cc <- -dx1 / (dx2 * (dx1 + dx2))
      deriv[, 1] <- a * data[, 1] + b * data[, 2] + cc * data[, 3]
      # right boundary
      dx1 <- arg[n - 1] - arg[n - 2]
      dx2 <- arg[n] - arg[n - 1]
      a <- dx2 / (dx1 * (dx1 + dx2))
      b <- -(dx2 + dx1) / (dx1 * dx2)
      cc <- (2 * dx2 + dx1) / (dx2 * (dx1 + dx2))
      deriv[, n] <- a * data[, n - 2] + b * data[, n - 1] + cc * data[, n]
      # interior: 2nd-order accurate central differences (non-uniform grid)
      dx1 <- arg[2:(n - 1)] - arg[1:(n - 2)]
      dx2 <- arg[3:n] - arg[2:(n - 1)]
      a <- -dx2 / (dx1 * (dx1 + dx2))
      b <- (dx2 - dx1) / (dx1 * dx2)
      cc <- dx1 / (dx2 * (dx1 + dx2))
      nr <- nrow(data)
      deriv[, 2:(n - 1)] <-
        data[, 1:(n - 2)] * rep(a, each = nr) +
        data[, 2:(n - 1)] * rep(b, each = nr) +
        data[, 3:n] * rep(cc, each = nr)
    }
    data <- deriv
  }
  list(data = data, arg = arg)
}

# trapezoidal quadrature
quad_trapez <- function(arg, evaluations) {
  c(0, 0.5 * diff(arg) * (head(evaluations, -1) + evaluations[-1]))
}
#-------------------------------------------------------------------------------

#' Invert a `tf` vector
#'
#' Computes the functional inverse of each function in the `tf` vector, such that
#' if \eqn{y = f(x)}, then \eqn{x = f^{-1}(y)}.
#'
#' @param x a `tf` vector.
#' @param ... optional arguments for the returned object, see [tfd()] / [tfb()]
#' @returns a `tf` vector of the inverted functions.
#'
#' @export
#' @examples
#' arg <- seq(0, 2, length.out = 50)
#' x <- tfd(rbind(2 * arg, arg^2), arg = arg)
#' x_inv <- tf_invert(x)
#' layout(t(1:2))
#' plot(x, main = "original functions", ylab = "")
#' plot(x_inv, main = "inverted functions", ylab = "", points = FALSE)
tf_invert <- function(x, ...) {
  UseMethod("tf_invert")
}

#' @export
tf_invert.tfd <- function(x, ...) {
  assert_tfd(x)
  assert_monotonic(x)

  arg <- ensure_list(tf_arg(x))
  if (length(x) > 1 && length(arg) == 1) {
    arg <- rep(arg, length(x))
  }
  tfd(arg, arg = tf_evaluations(x), ...)
}

#' @export
tf_invert.tfb <- function(x, ...) {
  basis <- if (is_tfb_spline(x)) "spline" else "fpc"
  ret <- x |> as.tfd() |> tf_invert() |> as.tfb(basis = basis, ...)
  cli::cli_warn(
    message = "{.fn tf_invert} for {.cls tfb} returns object with default basis."
  )
  ret
}


# reinsert NULL entries for previously missing functions while preserving tf attrs
restore_na_entries <- function(tf_non_na, na_entries, names_out) {
  if (!any(na_entries)) {
    return(setNames(tf_non_na, names_out))
  }
  ret <- vector("list", length(na_entries))
  if (any(!na_entries)) {
    ret[!na_entries] <- unclass(tf_non_na)
  }
  ret[na_entries] <- list(NULL)
  attributes(ret) <- attributes(tf_non_na)
  names(ret) <- names_out
  ret
}

#-------------------------------------------------------------------------------

#' Differentiating functional data: approximating derivative functions
#'
#' Derivatives of `tf`-objects use finite differences of the evaluations for
#' `tfd` and finite differences of the basis functions for `tfb`.
#'
#' The derivatives of `tfd` objects use second-order accurate central differences
#' for interior points and second-order accurate one-sided differences at
#' boundaries, following the non-uniform grid formulas from `numpy.gradient`
#' with `edge_order=2` (Fornberg, 1988).
#' Domain and grid of the returned object are identical to the input. Unless the
#' `tfd` has a rather fine and regular grid, representing the data in a suitable
#' basis representation with [tfb()] and then computing the derivatives (or
#' integrals) of those is usually preferable.
#'
#' Note that, for spline bases like `"cr"` or `"tp"` which are constrained to
#' begin/end linearly, computing *second* derivatives will produce artefacts at
#' the outer limits of the functions' domain due to these boundary constraints.
#' Basis `"bs"` does not have this problem for sufficiently high orders (but
#' tends to yield slightly less stable fits).
#' @param f a `tf`-object
#' @param order order of differentiation. Maximal value for `tfb_spline` is 2.
#' For `tfb_spline`-objects, `order = -1` yields integrals (used internally).
#' @param arg grid to use for the finite differences.
#'   Not (exactly) the `arg` of the returned object for `tfd`-inputs, see details.
#' @param ... not used
#' @returns a `tf` (with slightly different `arg` or `basis` for the
#'   derivatives, see details).
#' @examples
#' arg <- seq(0, 1, length.out = 31)
#' x <- tfd(rbind(arg^2, sin(2 * pi * arg)), arg = arg)
#' dx <- tf_derive(x)
#' x
#' dx
#' tf_arg(dx)
#' @export
#' @family tidyfun calculus functions
tf_derive <- function(f, arg, order = 1, ...) UseMethod("tf_derive")

#' @export
tf_derive.default <- function(f, arg, order = 1, ...) .NotYetImplemented()

#' @export
#' @describeIn tf_derive row-wise finite differences
tf_derive.matrix <- function(f, arg, order = 1, ...) {
  if (missing(arg)) arg <- unlist(find_arg(f), use.names = FALSE)
  assert_numeric(
    arg,
    any.missing = FALSE,
    finite = TRUE,
    len = ncol(f),
    sorted = TRUE,
    unique = TRUE
  )
  ret <- derive_matrix(data = f, order = order, arg = arg)
  structure(ret[[1]], arg = ret[[2]])
}

#' @export
#' @describeIn tf_derive derivatives by finite differencing of function evaluations.
tf_derive.tfd <- function(f, arg = tf_arg(f), order = 1, ...) {
  assert_count(order)
  na_entries <- is.na(f)
  data <- as.matrix(f, arg, interpolate = TRUE)
  arg <- as.numeric(colnames(data))
  derived <- derive_matrix(data[!na_entries, , drop = FALSE], arg, order)
  ret <- tfd(
    derived$data,
    derived$arg,
    domain = tf_domain(f)
  )
  tf_evaluator(ret) <- attr(f, "evaluator_name")
  restore_na_entries(ret, na_entries, names(f))
}

#' @export
#' @describeIn tf_derive element-wise finite differencing for irregular grids.
#'   Falls back to `tf_derive.tfd` (interpolating to a common grid) if an
#'   explicit `arg` vector is supplied.
tf_derive.tfd_irreg <- function(f, arg, order = 1, ...) {
  if (!missing(arg)) {
    return(tf_derive.tfd(f, arg = arg, order = order, ...))
  }
  cli::cli_inform(c(
    x = "Differentiating over irregular grids can be unstable."
  ))
  assert_count(order)
  na_entries <- is.na(f)
  args <- tf_arg(f)
  evals <- tf_evaluate(f)
  derived_data <- vector("list", length(f))
  for (i in which(!na_entries)) {
    d <- derive_matrix(rbind(evals[[i]]), args[[i]], order)
    derived_data[[i]] <- d$data[1, ]
  }
  derived_data[na_entries] <- list(NULL)
  ret <- tfd(
    derived_data[!na_entries],
    args[!na_entries],
    domain = tf_domain(f)
  )
  tf_evaluator(ret) <- attr(f, "evaluator_name")
  restore_na_entries(ret, na_entries, names(f))
}

#' @export
#' @describeIn tf_derive derivatives by finite differencing of spline basis functions.
tf_derive.tfb_spline <- function(f, arg = tf_arg(f), order = 1, ...) {
  # TODO: make this work for iterated application tf_derive(tf_derive(fb))
  if (!is.null(attr(f, "basis_deriv"))) {
    cli::cli_abort(
      "Can't integrate or derive previously integrated or derived {.cls tfb_spline}."
    )
  }
  assert_choice(order, choices = c(-1, 1, 2))
  assert_arg(arg, f)
  if (attr(f, "family")$link != "identity") {
    cli::cli_inform(c(
      i = "Using {.cls tfd} fallback for calculus on {.cls tfb_spline} with non-identity link.",
      i = "Returning derivatives/integrals on response scale as {.cls tfd}."
    ))
    f_tfd <- tfd(f, arg = arg)
    if (order == -1) {
      dots <- list(...)
      return(tf_integrate(
        f_tfd,
        arg = arg,
        lower = dots$lower %||% min(arg),
        upper = dots$upper %||% max(arg),
        definite = FALSE
      ))
    }
    return(tf_derive(f_tfd, arg = arg, order = order))
  }
  s_args <- attr(f, "basis_args")
  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(
    s_spec,
    data = data_frame0(arg = arg),
    knots = NULL
  )
  eps <- min(diff(arg)) / 1000
  basis_constructor <- smooth_spec_wrapper(
    spec_object,
    deriv = order,
    eps = eps
  )
  attr(f, "basis") <- basis_constructor
  attr(f, "basis_label") <- deparse1(s_call, width.cutoff = 60)
  attr(f, "basis_args") <- s_args
  attr(f, "basis_matrix") <- basis_constructor(arg)
  attr(f, "basis_deriv") <- order
  attr(f, "domain") <- range(arg)
  f
}

#' @export
#' @describeIn tf_derive derivatives by finite differencing of FPC basis functions.
tf_derive.tfb_fpc <- function(f, arg = tf_arg(f), order = 1, ...) {
  efunctions <- environment(attr(f, "basis"))$efunctions
  environment(attr(f, "basis")) <- new.env()
  new_basis <- if (order > 0) {
    tf_derive(efunctions, arg = arg, order = order)
  } else {
    tf_integrate(efunctions, arg = arg, definite = FALSE, ...)
  }
  environment(attr(f, "basis"))$efunctions <- new_basis
  attr(f, "basis_matrix") <- t(as.matrix(new_basis))
  attr(f, "arg") <- tf_arg(new_basis)
  attr(f, "domain") <- range(tf_arg(new_basis))
  f
}

#-------------------------------------------------------------------------------

#' Integrals and anti-derivatives of functional data
#'
#' Integrals of `tf`-objects are computed by simple quadrature (trapezoid rule).
#' By default the scalar definite integral
#' \eqn{\int^{upper}_{lower}f(s)ds} is returned (option `definite = TRUE`),
#' alternatively for `definite = FALSE` the *anti-derivative* on
#' `[lower, upper]`, e.g. a `tfd` or `tfb` object representing \eqn{F(t) \approx
#' \int^{t}_{lower}f(s)ds}, for \eqn{t \in}`[lower, upper]`, is returned.
#' @inheritParams tf_derive
#' @param arg (optional) grid to use for the quadrature.
#' @param lower lower limits of the integration range. For `definite = TRUE`, this
#'   can be a vector of the same length as `f`.
#' @param upper upper limits of the integration range (but see `definite` arg /
#'   description). For `definite = TRUE`, this can be a vector of the same length
#'   as `f`.
#' @param definite should the definite integral  be returned (default) or the
#'   antiderivative. See description.
#' @returns For `definite = TRUE`, the definite integrals of the functions in
#'   `f`. For `definite = FALSE` and `tf`-inputs, a `tf` object containing their
#'   anti-derivatives
#' @examples
#' arg <- seq(0, 1, length.out = 11)
#' x <- tfd(rbind(arg, arg^2), arg = arg)
#' tf_integrate(x)
#' anti <- tf_integrate(x, definite = FALSE)
#' tf_arg(anti)
#' @export
#' @family tidyfun calculus functions
tf_integrate <- function(f, arg, lower, upper, ...) {
  UseMethod("tf_integrate")
}

#' @rdname tf_integrate
#' @export
tf_integrate.default <- function(f, arg, lower, upper, ...) .NotYetImplemented()

#' @rdname tf_integrate
#' @export
tf_integrate.tfd <- function(
  f,
  arg = tf_arg(f),
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  ...
) {
  assert_arg(arg, f)
  arg <- ensure_list(arg)
  # TODO: integrate is NA whenever arg does not cover entire domain!
  assert_limit(lower, f)
  assert_limit(upper, f)
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) {
    if (!definite) .NotYetImplemented() # needs vd-data
    limits <- limits |> split(seq_len(nrow(limits)))
  }
  arg <- map2(
    arg,
    ensure_list(limits),
    \(x, y) c(y[1], x[x > y[1] & x < y[2]], y[2])
  )
  na_entries <- is.na(f)

  if (definite && any(na_entries)) {
    arg_non_na <- if (length(arg) == 1) arg else arg[!na_entries]
    if (any(!na_entries)) {
      evaluations <- tf_evaluate(f[!na_entries], arg_non_na)
      quads <- map2(
        arg_non_na,
        evaluations,
        \(x, y) quad_trapez(arg = x, evaluations = y)
      )
    } else {
      quads <- list()
    }
    ret <- rep(NA_real_, length(f))
    if (any(!na_entries)) {
      ret[!na_entries] <- map_dbl(quads, sum)
    }
    return(setNames(ret, names(f)))
  }

  if (!definite && length(arg) == 1 && any(na_entries)) {
    arg_non_na <- arg
    if (any(!na_entries)) {
      evaluations <- tf_evaluate(f[!na_entries], arg_non_na)
      quads <- map(
        evaluations,
        \(y) quad_trapez(arg = arg_non_na[[1]], evaluations = y)
      )
      data_non_na <- map(quads, cumsum)
      names(data_non_na) <- names(f)[!na_entries]
    } else {
      data_non_na <- matrix(numeric(), nrow = 0, ncol = length(arg_non_na[[1]]))
    }
    ret <- tfd(
      data = data_non_na,
      arg = arg_non_na[[1]],
      domain = as.numeric(limits),
      evaluator = !!attr(f, "evaluator_name")
    )
    return(restore_na_entries(ret, na_entries, names(f)))
  }

  evaluations <- tf_evaluate(f, arg)
  quads <- map2(arg, evaluations, \(x, y) quad_trapez(arg = x, evaluations = y))
  if (definite) {
    map_dbl(quads, sum) |> setNames(names(f))
  } else {
    data_list <- map(quads, cumsum)
    names(data_list) <- names(f)
    tfd(
      data = data_list,
      arg = unlist(arg, use.names = FALSE),
      domain = as.numeric(limits),
      evaluator = !!attr(f, "evaluator_name")
    )
  }
  # this is too slow:
  # turn into functions, return definite integrals
  # (Why the hell does this not work without vectorize....?)
  # map(f, ~ possibly(stats::tf_integrate, list(value = NA))(
  #  Vectorize(as.function(.x)), lower = lower, upper = upper, ...)) |>
  # map("value")
}

#' @rdname tf_integrate
#' @export
tf_integrate.tfb <- function(
  f,
  arg = tf_arg(f),
  lower = tf_domain(f)[1],
  upper = tf_domain(f)[2],
  definite = TRUE,
  ...
) {
  assert_arg(arg, f)
  assert_limit(lower, f)
  assert_limit(upper, f)
  if (definite) {
    return(tf_integrate(
      tfd(f, arg = arg),
      lower = lower,
      upper = upper,
      arg = arg
    ))
  }
  limits <- cbind(lower, upper)
  if (nrow(limits) > 1) .NotYetImplemented() # needs vd-data
  arg <- c(
    limits[1],
    arg[arg > limits[1] & arg < limits[2]],
    limits[2]
  )
  tf_derive(f, order = -1, arg = arg, lower = lower, upper = upper)
}
