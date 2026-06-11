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
  # Project object's native evaluations directly onto basis_from's basis,
  # reusing basis_from's spec/penalty/sp. No separate interpolation step,
  # which would compound interpolation error on top of basis approximation
  # error (see #269).
  dots <- list(...)
  verbose <- dots$verbose %||% FALSE
  global <- dots$global %||% FALSE

  data <- as.data.frame(object, unnest = TRUE)
  data$id <- factor(data$id, unique(as.character(data$id)))
  names_data <- levels(data$id)

  basis_args <- attr(basis_from, "basis_args")
  sp <- basis_args$sp
  family <- attr(basis_from, "family")

  # Build a spec object whose design matrix lives on object's native arg grid,
  # but whose penalty matrix S and basis dimension come from basis_from.
  basis_closure <- attr(basis_from, "basis")
  spec_from <- environment(basis_closure)$spec
  arg_u <- uniquecombs(data$arg, ordered = TRUE)
  spec_object <- spec_from
  # Predict.matrix from basis_from's spec evaluated at object's unique args:
  spec_object$X <- basis_closure(arg_u$x)

  arg_list <- split(data$arg, data$id)
  regular <- length(arg_list) > 1 && all(duplicated(arg_list)[-1])

  gam_args <- dots[names(dots) %in% c(formalArgs(gam), formalArgs(bam))]
  gam_args$family <- family
  gam_args$sp <- if (is.na(sp)) -1 else sp

  ls_fit <- family$family == "gaussian" && family$link == "identity"

  penalized <- !is.na(sp)
  n_evaluations <- table(data$id)
  underdetermined <- n_evaluations <= spec_object$bs.dim
  if (!penalized) {
    if (any(underdetermined)) {
      cli::cli_abort(
        c(
          "Can't compute spline coefficients for too sparse data: At least as many basis functions as evaluations for {sum(underdetermined)} of {vec_size(underdetermined)} entries.",
          ">" = "Rebase onto a penalized {.cls tfb_spline} or reduce {.arg k}.",
          "i" = "Affected entries: {names(n_evaluations[underdetermined])}"
        )
      )
    }
    fit <- fit_unpenalized(
      data = data,
      spec_object = spec_object,
      arg_u = arg_u,
      gam_args = gam_args,
      regular = regular,
      ls_fit = ls_fit
    )
  } else {
    fit <- fit_penalized(
      data = data,
      spec_object = spec_object,
      arg_u = arg_u,
      gam_args = gam_args,
      regular = regular,
      global = global,
      ls_fit = ls_fit
    )
  }

  if (verbose && isTRUE(min(fit$pve) < 0.5)) {
    cli::cli_warn(c(
      x = "Smooth fit captures less than half of input data variability for {sum(fit$pve < .5)} entries.",
      i = "Consider increasing basis dimension {.arg k} (or decreasing penalization {.arg sp}) of {.arg basis_from}."
    ))
  }

  ret <- new_vctr(
    fit[["coef"]],
    domain = tf_domain(basis_from),
    basis = basis_closure,
    basis_label = attr(basis_from, "basis_label"),
    basis_args = basis_args,
    basis_matrix = attr(basis_from, "basis_matrix"),
    arg = tf_arg(basis_from),
    family = family,
    family_label = attr(basis_from, "family_label"),
    class = c("tfb_spline", "tfb", "tf")
  )
  setNames(ret, names_data)
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
