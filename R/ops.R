# *, / for tfs; and +, -, ^ for tfds
fun_op <- function(x, y, op, numeric = NA) {
  if (!is.na(numeric)) {
    # function-scalar-ops
    num <- list(x, y)[[numeric]]
    f <- list(x, y)[[3 - numeric]]
    assert_numeric(num)
    # no "recycling" of args -- breaking a crappy R convention, proudly so.
    stopifnot(
      (length(num) > 0 && length(f) == 1) || length(num) %in% c(1, length(f))
    )
    attr_ret <- attributes(f)
    arg_ret <- tf_arg(f)
  } else {
    # function-function-ops
    stopifnot(
      # no "recycling" of args
      length(x) %in% c(1, length(y)) || length(y) %in% c(1, length(x)),
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

#' @export
#' @method vec_arith tfd
vec_arith.tfd <- function(op, x, y, ...) {
  UseMethod("vec_arith.tfd", y)
}

#' @export
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
    `/` = fun_op(x, y, op),
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
    `^` = fun_op(x, y, op, numeric = 2),
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
    `*` = fun_op(x, y, op, numeric = 1),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfd MISSING
vec_arith.tfd.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x * -1,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith tfb
vec_arith.tfb <- function(op, x, y, ...) {
  UseMethod("vec_arith.tfb", y)
}

#' @export
vec_arith.tfb.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.tfb tfb
vec_arith.tfb.tfb <- function(op, x, y, ...) {
  stopifnot(all(compare_tf_attribs(x, y)))
  switch(op,
    `+` = ,
    `-` = fun_op(x, y, op),
    `*` = ,
    `/` = {
      basis_args <- attr(x, "basis_args")
      eval <- fun_op(tfd(x), tfd(y), op)
      do.call(
        "tfb",
        c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
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
      eval <- fun_op(tfd(x), y, op, numeric = 2)
      do.call(
        "tfb",
        c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
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
      eval <- fun_op(x, tfd(y), op, numeric = 1)
      do.call(
        "tfb",
        c(list(eval), basis_args, penalized = FALSE, verbose = FALSE)
      )
    },
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.tfb MISSING
vec_arith.tfb.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x * -1,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}
