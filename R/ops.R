# *, / for tfs; and +, -, ^ for tfds
fun_op <- function(x, y, op, numeric = NA) {
  if (!is.na(numeric)) {
  # function-scalar-ops
    num <- list(x, y)[[numeric]]
    f <- list(x, y)[[3 - numeric]]
    assert_numeric(num)
    # no "recycling" of args -- breaking a crappy R convention, proudly so.
    stopifnot(
      (length(num) > 0 & length(f) == 1) |
        length(num) %in% c(1, length(f))
    )
    attr_ret <- attributes(f)
    arg_ret <- tf_arg(f)
  } else {
  # function-function-ops
    stopifnot(
      # no "recycling" of args
      (length(x) %in% c(1, length(y))) | (length(y) %in% c(1, length(x))),
      all.equal(tf_domain(x), tf_domain(y), check.attributes = FALSE),
      all.equal(tf_arg(x), tf_arg(y), check.attributes = FALSE)
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
#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute functions
Ops.tf <- function(e1, e2) {
  not_defined <- switch(.Generic,
    `%%` = ,
    `%/%` = ,
    `&` = ,
    `|` = ,
    `!` = ,
    `<` = ,
    `<=` = ,
    `>=` = ,
    `>` = TRUE,
    FALSE
  )
  if (not_defined) {
    stop(sprintf("%s not defined for \"tf\" objects", .Generic), call. = FALSE)
  }
  if (nargs() == 1) {
    return(fun_op(0, e1, .Generic, numeric = 1))
  }
}

#' @rdname tfgroupgenerics
#' @export
`==.tfd` <- function(e1, e2) {
  # no "recycling" of args
  stopifnot((length(e1) %in% c(1, length(e2))) |
    (length(e2) %in% c(1, length(e1))))
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
Ops.tfd <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    if (is_tfd(e1) && is_tfd(e2)) {
      if (.Generic == "^") {
        stop("^ not defined for \"tfd\" objects", call. = FALSE)
      } else {
        return(fun_op(e1, e2, .Generic))
      }
    }
    if (is.logical(e1)) e1 <- as.numeric(e1)
    if (is.logical(e2)) e2 <- as.numeric(e2)
    if (is_tfd(e1) && is.numeric(e2)) {
      return(fun_op(e1, e2, .Generic, numeric = 2))
    }
    if (is_tfd(e2) && is.numeric(e1)) {
      return(fun_op(e1, e2, .Generic, numeric = 1))
    }
    stop(sprintf(
      "binary %s not defined for classes %s and %s", .Generic, class(e1)[1], class(e2)[1]
    ), call. = FALSE)
  }
  ret
}

#' @rdname tfgroupgenerics
#' @export
Ops.tfb <- function(e1, e2) {
  ret <- NextMethod()
  if (nargs() != 1) {
    both_funs <- is_tfb(e1) & is_tfb(e2)
    if (both_funs) {
      if (.Generic == "^") {
        stop("^ not defined for \"tfb\" objects", call. = FALSE)
      }
      stopifnot(all(compare_tf_attribs(e1, e2)))
    }
    if (both_funs && .Generic %in% c("+", "-")) {
      # just add/subtract coefs for identical bases
      return(fun_op(e1, e2, .Generic))
    } else {
      # ... else convert to tfd, compute, refit basis
      if (both_funs) {
        basis_args <- attr(e1, "basis_args")
        eval <- fun_op(tfd(e1), tfd(e2), .Generic)
      }
      if (is.logical(e1)) e1 <- as.numeric(e1)
      if (is.logical(e2)) e2 <- as.numeric(e2)
      if (is_tfb(e1) && is.numeric(e2)) {
        basis_args <- attr(e1, "basis_args")
        eval <- fun_op(tfd(e1), e2, .Generic, numeric = 2)
      }
      if (is_tfb(e2) && is.numeric(e1)) {
        basis_args <- attr(e2, "basis_args")
        eval <- fun_op(e1, tfd(e2), .Generic, numeric = 1)
      }
      return(do.call(
        "tfb",
        c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
      ))
    }
  }
  ret
}
