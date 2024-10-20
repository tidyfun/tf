#' Math, Summary and Ops Methods for `tf`
#'
#' These methods and operators mostly work `arg`-value-wise on `tf` objects, see
#' [vctrs::vec_arith()] etc. for implementation details.
#'
#' See examples below. Equality checks of functional objects are even more iffy
#' than usual for computer math and not very reliable. Note that `max` and `min`
#' are not guaranteed to be maximal/minimal over the entire domain, only on the
#' evaluation grid used for computation. With the exception of addition and
#' multiplication, operations on `tfb`-objects first evaluate the data on their
#' `arg`, perform computations on these evaluations and then convert back to an
#' `tfb`- object, so a loss of precision should be expected -- especially so for
#' small spline bases and/or very wiggly data.
#'
#'
#' @param x a `tf` or `numeric` object
#' @param y a `tf` or `numeric` object
#' @param op An arithmetic operator as a string
#' @param ... `tf`-objects (not used for `Math` group generic)
#' @param e1 an `tf` or a numeric vector
#' @param e2 an `tf` or a numeric vector
#' @returns a `tf`- or `logical` vector with the computed result
#' @seealso [tf_fwise()] for scalar summaries of each function in a `tf`-vector
#' @name tfgroupgenerics
#' @family tidyfun compute functions
#' @examples
#' set.seed(1859)
#' f <- tf_rgp(4)
#' 2 * f == f + f
#' sum(f) == f[1] + f[2] + f[3] + f[4]
#' log(exp(f)) == f
#' plot(f, points = FALSE)
#' lines(range(f), col = 2, lty = 2)
#'
#' f2 <- tf_rgp(5) |> exp() |> tfb(k = 25)
#' layout(t(1:3))
#' plot(f2, col = gray.colors(5))
#' plot(cummin(f2), col = gray.colors(5))
#' plot(cumsum(f2), col = gray.colors(5))
#'
#' # ?tf_integrate for integrals, ?tf_fwise for scalar summaries of each function
NULL

#' @rdname tfgroupgenerics
#' @export
`==.tfd` <- function(e1, e2) {
  assert_compatible_size("==", e1, e2)
  # not comparing names, as per convention...
  same <- all(compare_tf_attribs(e1, e2))
  if (!same) {
    rep(FALSE, max(e1_size, e2_size))
  } else {
    map2_lgl(e1, e2, \(x, y) isTRUE(all.equal(x, y)))
  }
}

#' @rdname tfgroupgenerics
#' @export
`!=.tfd` <- function(e1, e2) !(e1 == e2)

# need to copy instead of defining tf-method s.t. dispatch in Ops works
#' @rdname tfgroupgenerics
#' @export
`==.tfb` <- eval(`==.tfd`)

#' @rdname tfgroupgenerics
#' @export
`!=.tfb` <- eval(`!=.tfd`)

#' @rdname tfgroupgenerics
#' @export
#' @method vec_arith tfd
vec_arith.tfd <- function(op, x, y, ...) {
  UseMethod("vec_arith.tfd", y)
}

#' @export
#' @method vec_arith.tfd default
vec_arith.tfd.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.tfd tfd
vec_arith.tfd.tfd <- function(op, x, y, ...) {
  switch(op,
         `+`   = ,
         `-`   = ,
         `*`   = ,
         `/`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = tfd_op_tfd(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfd numeric
vec_arith.tfd.numeric <- function(op, x, y, ...) {
  switch(op,
         `+`   = ,
         `-`   = ,
         `*`   = ,
         `/`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = tfd_op_numeric(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric tfd
vec_arith.numeric.tfd <- function(op, x, y, ...) {
  switch(op,
         `+`   = ,
         `-`   = ,
         `*`   = ,
         `/`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = numeric_op_tfd(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfd MISSING
vec_arith.tfd.MISSING <- function(op, x, y, ...) {
  tf_op_missing(op, x, y, ...)
}

#' @rdname tfgroupgenerics
#' @export
#' @method vec_arith tfb
vec_arith.tfb <- function(op, x, y, ...) {
  UseMethod("vec_arith.tfb", y)
}

#' @export
#' @method vec_arith.tfb default
vec_arith.tfb.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.tfb tfb
vec_arith.tfb.tfb <- function(op, x, y, ...) {
  switch(op,
         `+` = ,
         `-` = tfb_plusminus_tfb(op, x, y),
         `*`   = ,
         `/`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = tfb_op_tfb(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfb numeric
vec_arith.tfb.numeric <- function(op, x, y, ...) {
  switch(op,
         `/`   = ,
         `*`   = tfb_multdiv_numeric(op, x, y),
         `+`   = ,
         `-`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = tfb_op_numeric(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric tfb
vec_arith.numeric.tfb <- function(op, x, y, ...) {
  switch(op,
         `*` = tfb_multdiv_numeric(op, y, x),
         `+`   = ,
         `-`   = ,
         `/`   = ,
         `^`   = ,
         `%%`  = ,
         `%/%` = numeric_op_tfb(op, x, y),
         stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfb MISSING
vec_arith.tfb.MISSING <- function(op, x, y, ...) {
  tf_op_missing(op, x, y, ...)
}

#-------------------------------------------------------------------------------

tfd_op_tfd <- function(op, x, y) {
  assert_compatible_size(op, x, y)
  # TODO: could be more lenient -- allow is one domain is subset of the other?
  same_domain <- all.equal(tf_domain(x), tf_domain(y), check.attributes = FALSE) |>
    isTRUE()
  same_arg <- all.equal(tf_arg(x), tf_arg(y), check.attributes = FALSE) |>
    isTRUE()
  if (!same_domain || !same_arg) {
    message <- c(
      glue::glue("<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>  not permitted for different",
                 "{ifelse(same_domain, '', ' domains')}",
                 "{ifelse(!same_domain & !same_arg, ' and', '')}",
                 "{ifelse(same_arg, '', ' argument values')}"),
        "-- use tf_rebase first.") |>
      paste(collapse = "\n")
    stop_incompatible_op(op, x, y, message = message)
  }

  attr_ret <- if (vec_size(x) >= vec_size(y)) attributes(x) else attributes(y)
  arg_ret <- tf_arg(y)
  x_ <- tf_evaluations(x)
  y_ <- tf_evaluations(y)
  ret <- map2(x_, y_, \(x, y) do.call(op, list(x, y)))

  if (attr(x, "evaluator_name") != attr(y, "evaluator_name")) {
    warning(
      "inputs have different evaluators, result has ", attr_ret$evaluator_name,
      call. = FALSE
    )
  }
  if ("tfd_irreg" %in% attr_ret$class) {
    ret <- map2(arg_ret, ret, \(x, y) list(arg = x, value = y))
  }
  attributes(ret) <- attr_ret
  ret
}

tfd_op_numeric <- function(op, x, y, ...) {
  assert_compatible_size(op, x, y)
  ret <- map2(tf_evaluations(x), y, \(x, y) do.call(op, list(x, y)))
  if (is_irreg(x)) {
    ret <- map2(tf_arg(x), ret, \(.arg, .ret) list(arg = .arg, value = .ret))
  }
  attributes(ret) <- attributes(x)
  if (vec_size(y) > 1) {
    names(ret) <- NULL
  }
  ret
}

# some code-duplication here, this makes non-commutative ops work for tfd and numeric
numeric_op_tfd <- function(op, x, y) {
  assert_compatible_size(op, x, y)
  ret <- map2(x, tf_evaluations(y), \(x, y) do.call(op, list(x, y)))
  if (is_irreg(y)) {
    ret <- map2(tf_arg(y), ret, \(.arg, .ret) list(arg = .arg, value = .ret))
  }
  attributes(ret) <- attributes(y)
  if (vec_size(x) > 1) {
    names(ret) <- NULL
  }
  ret
}

#-------------------------------------------------------------------------------

tfb_multdiv_numeric <- function(op, x, y) {
  # dispatch to general operator implementation (i.e. cast to tfd and back)
  # if link functions are involved:
  if (is_tfb_spline(x) && !attributes(x)$family$link == "identity") {
    return(tfb_op_numeric(op, x, y))
  }
  # if not, * and / can simply be applied to the basis coefficients:
  ret <- map2(vec_data(x), y, \(x, y) do.call(op, list(e1 = x, e2 = y)))
  attributes(ret) <- attributes(x)
  ret
}

tfb_op_numeric <- function(op, x, y) {
  warning(glue("potentially lossy cast to <tfd> and back in ",
               "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>"))
  eval <- tfd_op_numeric(op, tfd(x), y)
  tf_rebase(eval, x, penalized = FALSE, verbose = FALSE)
  #TODO: restore sp afterwards so all properties are preserved?
}

numeric_op_tfb <- function(op, x, y) {
  warning(glue("potentially lossy cast to <tfd> and back in ",
               "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>"))
  eval <- numeric_op_tfd(op, x, tfd(y))
  tf_rebase(eval, y, penalized = FALSE, verbose = FALSE) #TODO: see tfb_op_numeric
}

tfb_op_tfb <- function(op, x, y) {
  warning(glue::glue("potentially lossy casts to <tfd> and back for ",
               "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>"))
  eval <- tfd_op_tfd(op, tfd(x), tfd(y))
  ret_ptype <- if (vec_size(x) >= vec_size(y)) vec_ptype(x) else vec_ptype(y)
  tf_rebase(eval, ret_ptype, penalized = FALSE, verbose = FALSE) #TODO: see tfb_op_numeric
}

tfb_plusminus_tfb <- function(op, x, y) {
  assert_compatible_size(op, x, y)
  # rebase to basis of shorter or first argument if bases are not compatible
  # less computation to tf_rebase shorter input.
  # would be potentially more accurate to decide based on basis dimension and
  # tf_rebase the one with the smaller basis...
  if (!same_basis(x, y)) {
    if (vec_size(x) >= vec_size(y)) {
      warning(glue::glue("bases unequal -- potentially lossy tf_rebase for 2nd argument in ",
                         "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>"))
      y <- tf_rebase(y, x)
    } else {
      warning(glue::glue("bases unequal -- potentially lossy tf_rebase for 1st argument in ",
                         "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}>"))
      x <- tf_rebase(x, y)
    }
  }
  # dispatch to general operator implementation (i.e. cast to tfd and back)
  # if link functions are involved
  if (!all(c(attributes(x)$family$link, attributes(y)$family$link) == "identity")) {
    return(tfb_op_tfb(op, x, y))
  }
  ret <- map2(vec_data(x), vec_data(y), \(x, y) do.call(op, list(e1 = x, e2 = y)))
  attributes(ret) <- if (vec_size(x) >= vec_size(y)) attributes(x) else attributes(y)
  ret
}

#-------------------------------------------------------------------------------

tf_op_missing <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
