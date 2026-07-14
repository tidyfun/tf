#' Change (basis) representation of a `tf`-object
#'
#' Apply the representation of one `tf`-object to another; i.e. re-express it in
#' the other's basis, on its grid, etc.\cr
#' Useful for making different functional data objects compatible so they can
#' be combined, compared or computed with.
#'
#' This uses double dispatch (S3) internally, so the methods defined below are
#' themselves generics for methods `tf_rebase.tfd.tfd`,
#' `tf_rebase.tfd.tfb_spline`, `tf_rebase.tfd.tfb_fpc`, `tf_rebase.tfb.tfd`,
#' `tf_rebase.tfb.tfb` that dispatch on `object_from`.
#'
#' @param object a `tf` object whose representation should be changed.
#' @param basis_from  the `tf` object with the desired basis, `arg`, `evaluator`, etc.
#' @param arg optional new `arg` values, defaults to those of `basis_from`.
#' @param ... forwarded to the `tfb` or `tfd` constructors.
#' @returns a `tf`-vector containing the data of `object` in the same representation
#'   as `basis_from` (potentially modified by the arguments given in `...`).
#' @examples
#' x <- tf_rgp(3)
#' xb <- tfb(x, k = 8, penalized = FALSE, verbose = FALSE)
#' tf_rebase(tf_rgp(3), xb)
#'
#' @export
tf_rebase <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase", object)
}

#-------------------------------------------------------------------------------
#' @export
#' @method tf_rebase tfd
#' @describeIn tf_rebase re-express a `tfd`-vector in the same representation as
#'    some other  `tf`-vector
tf_rebase.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfd", basis_from)
}

#' @export
tf_rebase.tfd.tfd <- function(
  object,
  basis_from,
  arg = tf_arg(basis_from),
  ...
) {
  assert_domain_x_in_to(x = object, to = basis_from)

  default_arg <- identical(arg, tf_arg(basis_from))

  if (
    is_irreg(basis_from) &&
      vec_size(basis_from) != 0 &&
      vec_size(basis_from) != vec_size(object)
  ) {
    if (vec_size(object) != 1) {
      cli::cli_abort(
        "Can't rebase {.cls tfd} with irregular {.cls tfd} of incompatible size."
      )
    }
    # if <object> is length 1, extrapolate it to *all* eval points of
    # basis_from by default
    if (default_arg) {
      arg <- sort_unique(arg, simplify = TRUE)
      cli::cli_inform(
        "Using all {length(arg)} unique time points from {.arg basis_from} for new {.arg arg}."
      )
      default_arg <- FALSE
    }
  }

  if (!identical(tf_arg(object), arg)) {
    object <- tfd(object, arg, domain = tf_domain(basis_from), ...)
  }
  if (is_irreg(basis_from)) object <- as.tfd_irreg(object)
  attr(object, "domain") <- tf_domain(basis_from)
  attr(object, "evaluator") <- attr(basis_from, "evaluator")
  attr(object, "evaluator_name") <- attr(basis_from, "evaluator_name")
  object
}

#' @export
tf_rebase.tfd.tfb_spline <- function(
  object,
  basis_from,
  arg = tf_arg(basis_from),
  ...
) {
  assert_same_domains(object, basis_from)
  assert_arg(arg, basis_from)
  arg <- if (is.list(arg)) {
    if (length(arg) != 1L) {
      cli::cli_abort(
        "{.arg arg} must be a single evaluation grid for {.cls tfb_spline}."
      )
    }
    arg[[1L]]
  } else {
    arg
  }
  dots <- list(...)
  basis_args <- attr(basis_from, "basis_args")
  dots$penalized <- dots$penalized %||% !is.na(basis_args$sp)

  # Fit on object's NATIVE arg values, reusing basis_from's mgcv spec -- no
  # pre-interpolation (that would compound interpolation error on top of basis
  # approximation error). spec_override skips mgcv's unique-args-vs-k check
  # so the under-determined case (n_obs <= k) succeeds via the penalty.
  fit <- do.call(
    new_tfb_spline,
    c(
      list(
        data = as.data.frame(object, unnest = TRUE),
        domain = tf_domain(basis_from),
        sp = basis_args$sp,
        family = attr(basis_from, "family"),
        spec_override = environment(attr(basis_from, "basis"))$spec
      ),
      dots
    )
  )

  # Re-home onto the requested arg plus basis_from's closure / labels: the
  # coefficients ARE the spline function in basis-coordinate space; the stored
  # basis_matrix is just cached evaluation at the stored arg. With default arg,
  # this still makes `same_basis(result, basis_from)` TRUE so downstream
  # arithmetic stays warning-free; custom arg gets a correctly matched cache.
  attr(fit, "arg") <- arg
  attr(fit, "basis") <- attr(basis_from, "basis")
  attr(fit, "basis_matrix") <- attr(basis_from, "basis")(arg)
  attr(fit, "basis_args") <- attr(basis_from, "basis_args")
  attr(fit, "basis_label") <- attr(basis_from, "basis_label")
  attr(fit, "family_label") <- attr(basis_from, "family_label")
  fit
}

#' @export
tf_rebase.tfd.tfb_fpc <- function(
  object,
  basis_from,
  arg = tf_arg(basis_from),
  ...
) {
  assert_same_domains(object, basis_from)
  data <- tf_interpolate(object, arg = arg) |> as.data.frame(unnest = TRUE)
  new_tfb_fpc(
    data = data,
    basis_from = basis_from,
    domain = tf_domain(basis_from),
    arg = arg,
    ...
  )
}

#-------------------------------------------------------------------------------
#' @export
#' @method tf_rebase tfb
#' @describeIn tf_rebase re-express a `tfb`-vector in the same representation as
#'   some other `tf`-vector.
tf_rebase.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfb", basis_from)
}

#' @export
tf_rebase.tfb.tfd <- function(
  object,
  basis_from,
  arg = tf_arg(basis_from),
  ...
) {
  assert_domain_x_in_to(x = object, to = basis_from)
  tfd_args <- list(
    domain = tf_domain(basis_from),
    evaluator = attr(basis_from, "evaluator_name")
  )
  tfd_args <- modifyList(tfd_args, list(...))
  do.call(tfd, append(tfd_args, list(data = object, arg = arg)))
}

#' @export
tf_rebase.tfb.tfb <- function(
  object,
  basis_from,
  arg = tf_arg(basis_from),
  ...
) {
  assert_same_domains(object, basis_from) # no extrapolation of basis
  tf_rebase(tfd(object), basis_from, arg = arg, ...)
}
