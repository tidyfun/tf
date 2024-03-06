#' Gaussian Process random generator
#'
#' Generates `n` realizations of a zero-mean Gaussian process. The function also
#' accepts user-defined covariance functions (without "nugget" effect, see
#' `cov`), The implemented defaults with `scale` parameter \eqn{\phi}, `order`
#' \eqn{o} and `nugget` effect variance \eqn{\sigma^2} are:
#' - *squared exponential* covariance \eqn{Cov(x(t), x(t')) = \exp(-(t-t')^2)/\phi) + \sigma^2
#' \delta_{t}(t')}.
#' - *Wiener* process covariance \eqn{Cov(x(t), x(t')) =
#' \min(t',t)/\phi + \sigma^2 \delta_{t}(t')},
#' -  [*Matèrn* process](https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function#Definition)
#' covariance \eqn{Cov(x(t), x(t')) =
#' \tfrac{2^{1-o}}{\Gamma(o)} (\tfrac{\sqrt{2o}|t-t'|}{\phi})^o \text{Bessel}_o(\tfrac{\sqrt{2o}|t-t'|}{s})
#' + \sigma^2 \delta_{t}(t')}
#'
#' @param n how many realizations to draw
#' @param arg vector of evaluation points (`arg` of the return object). Defaults
#'   to (0, 0.02, 0.04, ..., 1). If given as a single **integer** (don't forget
#'   the **`L`**...), creates a  regular grid of that length over (0,1).
#' @param scale scale parameter (see Description). Defaults to the width of the
#'   domain divided by 10.
#' @param cov type of covariance function to use. Implemented defaults are
#'   `"squareexp"`, `"wiener"`, `"matern"`, see Description. Can also be any
#'   vectorized function returning \eqn{Cov(x(t), x(t'))} *without nugget
#'   effect* for pairs of inputs t and t'.
#' @param nugget nugget effect for additional white noise / unstructured
#'   variability. Defaults to `scale/200` (so: very little white noise).
#' @param order order of the Matèrn covariance (if used, must be >0), defaults
#'   to 1.5. The higher, the smoother the process. Evaluation of the covariance
#'   function becomes numerically unstable for large (>20) `order`, use
#'   "squareexp".
#' @returns an `tfd`-vector of length `n`
#' @importFrom mvtnorm rmvnorm
#' @export
#' @family tidyfun RNG functions
tf_rgp <- function(n, arg = 51L, cov = c("squareexp", "wiener", "matern"),
                   scale = diff(range(arg)) / 10, nugget = scale / 200, order = 1.5) {
  if (!is.function(cov)) {
    cov <- match.arg(cov)
    f_cov <- switch(cov,
      "wiener" = function(s, t) pmin(s, t) / scale,
      "squareexp" = function(s, t) exp(-(s - t)^2 / scale),
      "matern" = function(s, t) {
        r <- sqrt(2 * order) * abs(s - t) / scale
        cov <- 2^(1 - order) / gamma(order) * r^order *
          base::besselK(r, nu = order)
        cov[s == t] <- 1
        cov
      }
    )
  } else {
    assert_function(cov, nargs = 2)
    f_cov <- cov
  }

  if (length(arg) == 1) {
    assert_integerish(arg, lower = 1)
    arg <- seq(0, 1, length.out = arg)
  }
  assert_numeric(arg, any.missing = FALSE, unique = TRUE)
  assert_number(n, lower = 1)
  assert_number(scale, lower = 0)
  assert_number(nugget, lower = 0)

  cov <- outer(arg, arg, f_cov) + diag(0 * arg + nugget)
  y <- rmvnorm(n, mean = 0 * arg, sigma = cov)
  rownames(y) <- 1:n
  tfd(y, arg = arg)
}

#' Make a `tf` (more) irregular
#'
#' Randomly create some irregular functional data from regular ones.
#' **jiggle** it by randomly moving around its `arg`-values. Only for `tfd`.
#' **sparsify** it by setting (100*`dropout`)% of its values to `NA`.
#'
#' @param f a `tfd` object
#' @param amount how far away from original grid points can the new grid points
#'   lie, at most (relative to original distance to neighboring grid points).
#'   Defaults to at most 40% (0.4) of the original grid distances. Must be lower
#'   than 0.5
#' @returns an (irregular) `tfd` object
#' @importFrom stats runif
#' @export
#' @rdname tf_jiggle
#' @family tidyfun RNG functions
tf_jiggle <- function(f, amount = 0.4, ...) {
  stopifnot(is_tfd(f))
  assert_number(amount, lower = 0, upper = 0.5)
  f <- as.tfd_irreg(f)
  new_args <- map(tf_arg(f), tf_jiggle_args, amount = amount)
  evaluator <- attr(f, "evaluator_name")
  ret <- tfd(map2(new_args, tf_evaluations(f), cbind),
      domain = tf_domain(f), ...)
  tf_evaluator(ret) <- evaluator
  ret
}
tf_jiggle_args <- function(arg, amount) {
  diffs <- diff(arg)
  n <- length(arg)

  # push left/right at most (amount*100)% of distance to adjacent gridpoint
  push_left_right <- sample(c(-1, 1), n - 2, replace = TRUE)
  use_diffs <- ifelse(push_left_right == -1,
    diffs[1:(n - 2)],
    diffs[2:(n - 1)]
  )
  tf_jiggle <- runif(n - 2, 0, amount) * use_diffs * push_left_right
  new_args <- arg[2:(n - 1)] + tf_jiggle

  c(
    runif(1, arg[1], new_args[1]), new_args,
    runif(1, new_args[n - 2], arg[n])
  )
}

#' @rdname tf_jiggle
#' @param dropout how many values of `f` to drop, defaults to 50%.
#' @param ... additional args for the returned `tfd` in `tf_jiggle`
#' @export
#' @family tidyfun RNG functions
tf_sparsify <- function(f, dropout = 0.5, ...) {
  stopifnot(is_tf(f))
  nas <- map(tf_evaluations(f), \(x) runif(length(x)) < dropout)
  tf_evals <- map2(tf_evaluations(f), nas, \(x, y) x[!y])
  tf_args <- ensure_list(tf_arg(f))
  tf_args <- map2(tf_args, nas, \(x, y) x[!y])
  ret <- tfd.list(tf_evals, tf_args,
    domain = tf_domain(f)
  )
  if (is_tfd(f)) {
    evaluator <- attr(f, "evaluator_name")
    tf_evaluator(ret) <- evaluator
  }
  ret
}
