#' Math, Summary and Ops Methods for `tf`
#'
#' These methods and operators mostly work `arg`-value-wise on `tf` objects, see
#' `?groupGeneric` for implementation details.
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
#' @param x an `tf`
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

# *, / for tfs; and +, -, ^ for tfds
fun_op <- function(op, x, y, numeric = NA) {
  if (!is.na(numeric)) {
    # function-scalar-ops
    num <- list(x, y)[[numeric]]
    f <- list(x, y)[[3 - numeric]]
    assert_numeric(num)
    # any op with NULL or empty vectors always return empty vectors:
    if (vec_size(num) == 0) {
      return(vec_ptype(f))
    }
    # no args-"recycling"
    if (!((vec_size(f) == vec_size(num)) ||
          1 %in% c(vec_size(f), vec_size(num)))) {
      stop_incompatible_op(op, x, y)
    }
    attr_ret <- attributes(f)
    arg_ret <- tf_arg(f)
  } else {
    # function-function-ops
    stopifnot(
      # no "recycling" of args
      vec_size(x) == vec_size(y) || 1 %in% c(vec_size(x), vec_size(y)),
      isTRUE(all.equal(tf_domain(x), tf_domain(y), check.attributes = FALSE)),
      isTRUE(all.equal(tf_arg(x), tf_arg(y), check.attributes = FALSE))
    )
    attr_ret <- if (length(x) >= length(y)) {
      attributes(x)
    } else {
      attributes(y)
    }
    arg_ret <- tf_arg(y)
  }
  if (is_tfb(x)) x_ <- coef(x)
  if (is_tfd(x)) x_ <- tf_evaluations(x)
  if (isTRUE(numeric == 1)) x_ <- x
  if (is_tfb(y)) y_ <- coef(y)
  if (is_tfd(y)) y_ <- tf_evaluations(y)
  if (isTRUE(numeric == 2)) y_ <- y
  ret <- map2(x_, y_, \(x, y) do.call(op, list(e1 = x, e2 = y)))
  if ("tfd" %in% attr_ret$class) {
    if (is.na(numeric) && (attr(x, "evaluator_name") != attr(y, "evaluator_name"))) {
      warning(
        "inputs have different evaluators, result has ", attr_ret$evaluator_name,
        call. = FALSE
      )
    }
    if ("tfd_irreg" %in% attr_ret$class) {
      ret <- map2(arg_ret, ret, \(x, y) list(arg = x, value = y))
    }
  }
  attributes(ret) <- attr_ret
  if (anyNA(names(ret))) {
    names(ret) <- NULL
  }
  ret
}

#' @rdname tfgroupgenerics
#' @export
`==.tfd` <- function(e1, e2) {
  # no "recycling" of args
  stopifnot(
    length(e2) %in% c(1, length(e1)) || length(e1) %in% c(1, length(e2))
  )
  # not comparing names, as per convention...
  same <- all(compare_tf_attribs(e1, e2))
  if (!same) {
    return(rep(FALSE, max(length(e1), length(e2))))
  }
  map2_lgl(e1, e2, \(x, y) isTRUE(all.equal(x, y)))
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
    `+` = ,
    `-` = ,
    `*` = ,
    `/` = fun_op(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfd numeric
vec_arith.tfd.numeric <- function(op, x, y, ...) {
  switch(op,
    `+` = ,
    `-` = ,
    `/` = ,
    `*` = ,
    `^` = fun_op(op, x, y, numeric = 2),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric tfd
vec_arith.numeric.tfd <- function(op, x, y, ...) {
  switch(op,
    `+` = ,
    `-` = ,
    `/` = ,
    `*` = fun_op(op, x, y, numeric = 1),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfd MISSING
vec_arith.tfd.MISSING <- function(op, x, y, ...) {
  arith_tf_and_missing(op, x, y, ...)
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
  stopifnot(all(compare_tf_attribs(x, y)))
  switch(op,
    `+` = ,
    `-` = fun_op(op, x, y),
    `*` = ,
    `/` = {
      basis_args <- attr(x, "basis_args")
      eval <- fun_op(op, tfd(x), tfd(y))
      # TODO: this prob. needs to use tf_rebase() with vec_ptype2(x, y) (?!?)
      #   or similar to avoid casting to different bases etc?
      do.call(
        tfb, c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
      )
    },
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfb numeric
vec_arith.tfb.numeric <- function(op, x, y, ...) {
  switch(op,
    `+` = ,
    `-` = ,
    `/` = ,
    `*` = ,
    `^` = {
      basis_args <- attr(x, "basis_args")
      eval <- fun_op(op, tfd(x), y, numeric = 2)
      # TODO: this prob. needs to use tf_rebase() with vec_ptype(x)
      #   or similar to avoid casting FPCs to splines etc
      do.call(
        tfb, c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
      )
    },
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric tfb
vec_arith.numeric.tfb <- function(op, x, y, ...) {
  switch(op,
    `+` = ,
    `-` = ,
    `/` = ,
    `*` = {
      basis_args <- attr(y, "basis_args")
      eval <- fun_op(op, x, tfd(y), numeric = 1)
      # TODO: this prob. needs to use tf_rebase() with vec_ptype(y)
      #   or similar to avoid casting FPCs to splines etc
      do.call(
        tfb, c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
      )
    },
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfb MISSING
vec_arith.tfb.MISSING <- function(op, x, y, ...) {
  arith_tf_and_missing(op, x, y, ...)
}

arith_tf_and_missing <- function(op, x, y, ...) {
  switch(op,
    `-` = x * -1,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}
