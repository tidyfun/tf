# double dispatch tf_rebase.tf_to.tf_from
tf_rebase <- function(object, basis_from, arg, ...) {
  assert_class(object, "tf")
  assert_class(basis_from, "tf")
  all.equal(tf_domain(object), tf_domain(basis_from)) |>
    assert_true()
  UseMethod("tf_rebase", object)
}

#-------------------------------------------------------------------------------
tf_rebase.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfd", basis_from)
}
tf_rebase.tfd.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  if (!identical(tf_arg(object),  arg)) {
    object <- tf_interpolate(object, arg, ...)
  }
  attr(object, "evaluator") <- attr(basis_from, "evaluator")
  attr(object, "evaluator_name") <- attr(basis_from, "evaluator_name")
  object
}
tf_rebase.tfd.tfb_spline <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  #extract evals from object
  #extract fit method from basis_from
  if (!identical(tf_arg(basis_from),  arg)) {
    basis_from <- tf_interpolate(basis_from, arg, ...) #no, just call basis on arg!
  }
  #create tfb
}
tf_rebase.tfd.tfb_fpc <-  function(object, basis_from, arg = tf_arg(basis_from), ...) {
  if (!identical(tf_arg(basis_from),  arg)) {
    basis_from <- tf_interpolate(basis_from, arg, ...)  #no, just call basis on arg!
  }
}
#-------------------------------------------------------------------------------
tf_rebase.tfb <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  UseMethod("tf_rebase.tfb", basis_from)
}
#' @importFrom utils modifyList
tf_rebase.tfb.tfd <- function(object, basis_from, arg = tf_arg(basis_from), ...) {
  tfd_args <- list(resolution = tf_resolution(basis_from),
                   evaluator = attr(basis_from, "evaluator_name"))
  tfd_args <- modifyList(tfd_args, list(...))
  do.call(tfd, append(tfd_args, list(data = object, arg = arg)))
}


  # turn object into data.frame
  # extract basis, hyperparams, fitting function from basis_from
  # needs spec_object hidden in basis_from$basis
  # apply to object


# things it should do:
# tf_rebase(object, basis_from = object) == object
#
if (FALSE) {
  v <- tf_rgp(3, arg = 101L) |> tfd(evaluator = tf_approx_spline)

  raw <- tf_rgp(5) |> tf_jiggle()
  tf_rebase(raw, v)


  b <- tfb(raw, penalized = FALSE)
  b2 <- tfb(exp(raw), sp = .01, family = "Gamma")
  pc <- tfb_fpc(raw)

  #TODO: irregulars!
}
