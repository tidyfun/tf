#' Evaluate `tf`-vectors for given argument values
#'
#' Also used internally by the `[`-operator for `tf` data (see `?tfbrackets`) to
#' evaluate `object`, see examples.
#'
#' @param object a `tf`, or a `data.frame`-like object with `tf` columns.
#' @param arg optional evaluation grid (vector or list of vectors).
#'   Defaults to `tf_arg(object)`, implicitly.
#' @param evaluator optional. The function to use for inter/extrapolating the
#'  `tfd`. Defaults to `tf_evaluator(object)`.
#'  See e.g. [tf_approx_linear()] for details.
#' @param ... not used
#' @returns A list of numeric vectors containing the function
#'   evaluations on `arg`.
#' @export
#' @family tidyfun inter/extrapolation functions
#' @examples
#' f <- tf_rgp(3, arg = seq(0, 1, length.out = 11))
#' tf_evaluate(f) |> str()
#' tf_evaluate(f, arg = 0.5) |> str()
#' # equivalent, as matrix:
#' f[, 0.5]
#' new_grid <- seq(0, 1, length.out = 6)
#' tf_evaluate(f, arg = new_grid) |> str()
#' # equivalent, as matrix:
#' f[, new_grid]
tf_evaluate <- function(object, arg, ...) UseMethod("tf_evaluate")

#' @export
#' @rdname tf_evaluate
tf_evaluate.default <- function(object, arg, ...) .NotYetImplemented()

#' @export
#' @rdname tf_evaluate
tf_evaluate.tfd <- function(object, arg,
                            evaluator = tf_evaluator(object), ...) {
  if (missing(arg) || is.null(arg)) {
    return(tf_evaluations(object))
  }
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  ret <- pmap(
    list(arg, ensure_list(tf_arg(object)), tf_evaluations(object)),
    \(x, y, z) evaluate_tfd_once(
      new_arg = x, arg = y, evaluations = z,
      evaluator = evaluator
    )
  )

  setNames(ret, names(object))
}

evaluate_tfd_once <- function(new_arg, arg,
                              evaluations, evaluator, resolution) {
  if (isTRUE(all.equal(new_arg, arg))) return(evaluations)
  seen <- match(new_arg, arg)
  seen_index <- na.omit(seen)
  seen <- !is.na(seen)
  ret <- rep(NA, length(new_arg))
  ret[seen] <- evaluations[seen_index]
  ret[!seen] <-
    evaluator(new_arg[!seen], arg = arg, evaluations = evaluations)
  ret
}

#' @export
#' @rdname tf_evaluate
tf_evaluate.tfb <- function(object, arg, ...) {
  if (missing(arg) || is.null(arg)) {
    return(tf_evaluations(object))
  }
  arg <- ensure_list(arg)
  assert_arg(arg, object, check_unique = FALSE)
  if (length(arg) == 1) {
    arg <- unlist(arg)
    evals <- evaluate_tfb_once(
      x = arg,
      arg = tf_arg(object),
      coefs = do.call(cbind, coef(object)),
      basis = attr(object, "basis"),
      X = attr(object, "basis_matrix")
    )
    ret <- if (length(arg) == 1) {
      # avoid cast to simple vector for point evaluations
      split(evals, seq_along(evals))
    } else {
      split(evals, col(as.matrix(evals)))
    }
  } else {
    ret <- pmap(
      list(arg, ensure_list(tf_arg(object)), coef(object)),
      \(x, y, z) evaluate_tfb_once(
        x = x, arg = y, coefs = z,
        basis = attr(object, "basis"),
        X = attr(object, "basis_matrix")
      )
    )
  }
  if (!inherits(object, "tfb_fpc")) {
    ret <- map(ret, attr(object, "family")$linkinv)
  }
  setNames(ret, names(object))
}

evaluate_tfb_once <- function(x, arg, coefs, basis, X, resolution) {
  seen <- match(x, arg)
  seen_index <- na.omit(seen)
  seen <- !is.na(seen)
  if (all(seen)) return(drop(X[seen_index, , drop = FALSE] %*% coefs))
  Xnew <- X[rep(1, length(x)), ]
  if (any(seen)) Xnew[seen, ] <- X[seen_index, , drop = FALSE]
  Xnew[!seen, ] <- basis(x[!seen])
  drop(Xnew %*% coefs)
}

