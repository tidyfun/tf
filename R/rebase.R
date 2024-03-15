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
#' @param object a `tf` object whose representation should be changed
#' @param basis_from  the `tf` object with the desired basis, `arg`, `evaluator`, etc.
#' @param arg optional new `arg` values, defaults to those of `basis_from`
#' @param ... forwarded to the `tfb` or `tfd` constructors
#' @returns a `tf`-vector containing the data of `object` in the same representation
#'  as `basis_from` (potentially modified by the arguments given in `...`).
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
tf_rebase.tfd.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  assert_domain_x_in_to(x = object, to = basis_from)

  if (!identical(tf_arg(object),  arg)) {
    object <- tfd(object, arg, domain = tf_domain(basis_from), ...)
  }
  attr(object, "domain") <- tf_domain(basis_from)
  attr(object, "evaluator") <- attr(basis_from, "evaluator")
  attr(object, "evaluator_name") <- attr(basis_from, "evaluator_name")
  object
}
#'@export
tf_rebase.tfd.tfb_spline <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  assert_same_domains(object, basis_from)
  #extract evals from object
  data <- as.data.frame(object, unnest = TRUE)
  penalized <- !(is.na(attr(basis_from, "basis_args")$sp))
  basis_args <- attr(basis_from, "basis_args")
  basis_args <- basis_args[names(basis_args) != "sp"]
  do.call(new_tfb_spline,
          c(list(data = data,
                 domain = tf_domain(basis_from),
                 arg = arg,
                 penalized = penalized,
                 sp = attr(basis_from, "basis_args")$sp),
            basis_args, list(...))
         )
}
#'@export
tf_rebase.tfd.tfb_fpc <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  assert_same_domains(object, basis_from)
  data <- tf_interpolate(object, arg = arg) |> as.data.frame(unnest = TRUE)
  new_tfb_fpc(data = data, basis_from = basis_from,
              domain = tf_domain(basis_from),
              arg = arg, ...)
}

#-------------------------------------------------------------------------------
#'@export
#'@method tf_rebase tfb
#' @describeIn tf_rebase re-express a `tfb`-vector in the same representation as
#'    some other  `tf`-vector.
tf_rebase.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfb", basis_from)
}
#'@export
#'@importFrom utils modifyList
tf_rebase.tfb.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  assert_domain_x_in_to(x = object, to = basis_from)
  tfd_args <- list(domain = tf_domain(basis_from),
                   evaluator = attr(basis_from, "evaluator_name"))
  tfd_args <- modifyList(tfd_args, list(...))
  do.call(tfd, append(tfd_args, list(data = object, arg = arg, ...)))
}
#'@export
tf_rebase.tfb.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  assert_same_domains(object, basis_from) # no extrapolation of basis
  tf_rebase(tfd(object), basis_from, arg = arg, ...)
}
