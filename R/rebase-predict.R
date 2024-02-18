# double dispatch tf_rebase.tf_to.tf_from
#'@export
tf_rebase <- function(object, basis_from, arg, ...) {
  all.equal(tf_domain(object), tf_domain(basis_from)) |>
    assert_true()
  UseMethod("tf_rebase", object)
}

#-------------------------------------------------------------------------------
#'@export
#'@method tf_rebase tfd
tf_rebase.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfd", basis_from)
}
#'@export
tf_rebase.tfd.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  if (!identical(tf_arg(object),  arg)) {
    object <- tf_interpolate(object, arg,
                             resolution = tf_resolution(basis_from),
                             domain = tf_domain(basis_from),
                             ...)
  }
  attr(object, "evaluator") <- attr(basis_from, "evaluator")
  attr(object, "evaluator_name") <- attr(basis_from, "evaluator_name")
  object
}
#'@export
tf_rebase.tfd.tfb_spline <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  #extract evals from object
  data <- as.data.frame(object, unnest = TRUE)
  penalized <- !(is.na(attr(basis_from, "basis_args")$sp))
  basis_args <- attr(basis_from, "basis_args")
  basis_args <- basis_args[names(basis_args) != "sp"]
  do.call(new_tfb_spline,
          append(
            list(data = data,
                 domain = tf_domain(basis_from),
                 arg = arg,
                 resolution = tf_resolution(basis_from),
                 penalized = penalized,
                 sp = attr(basis_from, "basis_args")$sp),
            basis_args)
          )
}
#'@export
tf_rebase.tfd.tfb_fpc <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  data <- tf_interpolate(object, arg = arg) |> as.data.frame(unnest = TRUE)
  new_tfb_fpc(data = data, basis_from = basis_from,
              domain = tf_domain(basis_from),
              arg = arg,
              resolution = tf_resolution(basis_from))
}
#-------------------------------------------------------------------------------
#'@export
#'@method tf_rebase tfb
tf_rebase.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfb", basis_from)
}
#'@export
#'@importFrom utils modifyList
tf_rebase.tfb.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  tfd_args <- list(resolution = tf_resolution(basis_from),
                   evaluator = attr(basis_from, "evaluator_name"))
  tfd_args <- modifyList(tfd_args, list(...))
  do.call(tfd, append(tfd_args, list(data = object, arg = arg)))
}
#'@export
tf_rebase.tfb.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  tf_rebase(tfd(object), basis_from, arg = arg, ...)
}

