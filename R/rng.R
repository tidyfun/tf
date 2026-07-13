#' Gaussian Process random generator
#'
#' Generates `n` realizations of a zero-mean Gaussian process. The function also
#' accepts user-defined covariance functions (without "nugget" effect, see
#' `cov`), The implemented defaults with `scale` parameter \eqn{\phi}, `order`
#' \eqn{o} and `nugget` effect variance \eqn{\sigma^2} are:
#' - *squared exponential*: \eqn{Cov(x(t), x(t')) = \exp(-(t-t')^2)/\phi) + \sigma^2
#' \delta_{t}(t')}.
#' - *Wiener* process: \eqn{Cov(x(t), x(t')) =
#' \min(t',t)/\phi + \sigma^2 \delta_{t}(t')},
#' -  [*Matérn* process](https://en.wikipedia.org/wiki/Mat%C3%A9rn_covariance_function#Definition):
#' \eqn{Cov(x(t), x(t')) =
#' \tfrac{2^{1-o}}{\Gamma(o)} (\tfrac{\sqrt{2o}|t-t'|}{\phi})^o \text{Bessel}_o(\tfrac{\sqrt{2o}|t-t'|}{s})
#' + \sigma^2 \delta_{t}(t')}
#' -  [*Brownian Bridge* process](https://en.wikipedia.org/wiki/Brownian_bridge) for
#' \eqn{t, t' \in [a, b]}:
#' \eqn{Cov(x(t), x(t')) =  \frac{(b - \max(s,t))(\min(s, t) - a)}{\phi (b - a)} + \sigma^2 \delta_{t}(t')}
#'
#' @param n how many realizations to draw.
#' @param arg vector of evaluation points (`arg` of the return object). Defaults
#'   to (0, 0.02, 0.04, ..., 1). If given as a single **integer** (don't forget
#'   the **`L`**...), creates a  regular grid of that length over (0,1).
#'   If given as a `n`-long list of vectors, irregular functional data are created.
#' @param scale scale parameter (see description). Defaults to the width of the
#'   domain divided by 10.
#' @param cov type of covariance function to use. Implemented defaults are
#'   `"squareexp"`, `"wiener"`, `"matern"`, see description. Can also be any
#'   vectorized function returning \eqn{Cov(x(t), x(t'))} *without nugget
#'   effect* for pairs of inputs t and t'.
#' @param nugget nugget effect for additional white noise / unstructured
#'   variability. Defaults to `scale/200` (so: very little white noise).
#' @param order order of the Matérn covariance (if used, must be >0), defaults
#'   to 1.5. The higher, the smoother the process. Evaluation of the covariance
#'   function becomes numerically unstable for large (>20) `order`, use
#'   "squareexp".
#' @param domain of the generated functions. If not provided, the range of the
#'   supplied `arg` values.
#' @returns an `tfd`-vector of length `n`.
#' @importFrom stats rnorm
#' @export
#' @family tidyfun RNG functions
#' @examples
#' (x1 <- tf_rgp(10, cov = "squareexp", nugget = 0))
#  plot(x1)
#' tf_rgp(2, arg = list(sort(runif(25)), sort(runif(34))))
tf_rgp <- function(
  n,
  arg = 51L,
  cov = c("squareexp", "wiener", "matern", "brown_bridge"),
  scale = diff(domain) / 10,
  nugget = scale / 200,
  order = 1.5,
  domain = NULL
) {
  if (!is.function(cov)) {
    cov <- match.arg(cov)
    f_cov <-
      switch(
        cov,
        wiener = function(s, t) pmin(s, t) / scale,
        squareexp = function(s, t) exp(-(s - t)^2 / scale),
        matern = function(s, t) {
          r <- sqrt(2 * order) * abs(s - t) / scale
          cov <- 2^(1 - order) /
            gamma(order) *
            r^order *
            base::besselK(r, nu = order)
          cov[s == t] <- 1
          cov
        },
        brown_bridge = function(s, t) {
          r <- (domain[2] - pmax(s, t)) * (pmin(s, t) - domain[1]) / scale
          r / diff(domain)
        }
      )
  } else {
    assert_function(cov, nargs = 2)
    f_cov <- cov
  }
  assert_int(n, lower = 1)

  if (!is.list(arg) && length(arg) == 1) {
    assert_int(arg, lower = 1)
    arg <- seq(0, 1, length.out = arg)
  }
  if (!is.list(arg)) {
    assert_numeric(arg, any.missing = FALSE, unique = TRUE, sorted = TRUE)
    arg <- replicate(n, arg, simplify = FALSE)
  } else {
    assert_true(length(arg) == n)
    map(
      arg,
      \(x)
        assert_numeric(
          x,
          any.missing = FALSE,
          unique = TRUE,
          sorted = TRUE,
          .var.name = "arg"
        )
    )
  }

  if (is.null(domain)) {
    domain <- range(unlist(arg))
  } else {
    assert_numeric(
      domain,
      len = 2,
      any.missing = FALSE,
      unique = TRUE,
      sorted = TRUE
    )
    assert_true(domain[1] <= min(unlist(arg)))
    assert_true(domain[2] >= max(unlist(arg)))
  }

  assert_number(scale, lower = 0)
  assert_number(nugget, lower = 0)

  ret <- map(arg, \(.arg) {
    cov <- outer(.arg, .arg, f_cov) + diag(0 * .arg + nugget)
    # zero-mean multivariate normal draw via eigen-decomposition of the
    # covariance. Matches `mvtnorm::rmvnorm(method = "eigen")` semantics
    # exactly: rank-deficient kernels (e.g. squared-exp with zero nugget) are
    # sampled in their effective subspace; tiny negative eigenvalues from
    # numerical noise are clamped to zero. We use the symmetric square root
    # `V D^{1/2} V^T` so draws match the previous `mvtnorm` implementation
    # under `set.seed()`.
    e <- eigen(cov, symmetric = TRUE)
    d <- pmax(e$values, 0)
    R <- e$vectors %*% (sqrt(d) * t(e$vectors))
    sample <- as.numeric(R %*% rnorm(nrow(cov)))
    cbind(.arg, sample)
  }) |>
    tfd()
  names(ret) <- 1:n
  ret
}

#' Make a `tf` (more) irregular
#'
#' @description
#'  Randomly create some irregular functional data from regular ones.
#'
#' - **jiggle** it by randomly moving around its `arg`-values inside the intervals defined by its grid neighbors on the original argument grid.
#' - **sparsify** it by removing (100*`dropout`)% of the function values
#'
#' @param f a `tfd` object.
#' @param amount how far away from original grid points can the jiggled grid points
#'   lie, at most (relative to original distance to neighboring grid points).
#'   Defaults to at most 40% (0.4) of the original grid distances. Must be lower
#'   than 0.5.
#' @param same_arg for `tf_mv` objects, should all components receive the same
#'   random argument-grid changes? Defaults to `TRUE`; use `FALSE` to jitter or
#'   sparsify each component independently.
#' @param ... additional args for the returned `tfd` in `tf_jiggle`.
#' @returns an (irregular) `tfd` object.
#' @importFrom stats runif
#' @export
#' @rdname tf_jiggle
#' @family tidyfun RNG functions
#' @examples
#' set.seed(1)
#' (x <- tf_rgp(2, arg = 21L))
#' (x_jig <- tf_jiggle(x, amount = 0.2))
#' (x_sp <- tf_sparsify(x, dropout = 0.3))
#' c(is_irreg(x_jig), is_irreg(x_sp))
tf_jiggle <- function(f, amount = 0.4, ...) {
  UseMethod("tf_jiggle")
}

#' @export
tf_jiggle.default <- function(f, amount = 0.4, ...) {
  assert_tfd(f)
  assert_number(amount, lower = 0, upper = 0.5)
  if (!vec_size(f)) {
    return(as.tfd_irreg(f))
  }
  f <- as.tfd_irreg(f)
  new_args <- map(tf_arg(f), tf_jiggle_args, amount = amount)
  evaluator <- attr(f, "evaluator_name")
  ret <- tfd(
    map2(new_args, tf_evaluations(f), cbind),
    domain = tf_domain(f),
    ...
  )
  tf_evaluator(ret) <- evaluator
  ret
}

#' @export
#' @rdname tf_jiggle
tf_jiggle.tf_mv <- function(f, amount = 0.4, same_arg = TRUE, ...) {
  assert_flag(same_arg)
  if (!same_arg) {
    return(map_components(f, \(comp) tf_jiggle(comp, amount = amount, ...)))
  }
  tf_mv_assert_shared_arg(f, op = "tf_jiggle")
  tf_mv_map_same_rng(f, \(comp) tf_jiggle(comp, amount = amount, ...))
}

tf_jiggle_args <- function(arg, amount) {
  diffs <- diff(arg)
  g <- length(arg)

  # push left/right at most (amount*100)% of distance to adjacent gridpoint
  push_left_right <- sample(c(-1, 1), g - 2, replace = TRUE)
  use_diffs <- ifelse(push_left_right == -1, diffs[1:(g - 2)], diffs[2:(g - 1)])
  tf_jiggle <- runif(g - 2, 0, amount) * use_diffs * push_left_right
  new_args <- arg[2:(g - 1)] + tf_jiggle

  c(
    runif(1, arg[1], new_args[1]),
    new_args,
    runif(1, new_args[g - 2], arg[g])
  )
}

#' @rdname tf_jiggle
#' @param dropout what proportion of values of `f` to drop, on average. Defaults to half.
#' @export
tf_sparsify <- function(f, dropout = 0.5, ...) {
  UseMethod("tf_sparsify")
}

#' @export
tf_sparsify.default <- function(f, dropout = 0.5, ...) {
  rlang::check_dots_empty()
  assert_tf(f)
  if (!vec_size(f)) {
    return(as.tfd_irreg(f))
  }
  nas <- map(tf_evaluations(f), \(x) runif(length(x)) < dropout)
  tf_evals <- map2(tf_evaluations(f), nas, \(x, y) x[!y])
  tf_args <- ensure_list(tf_arg(f))
  tf_args <- map2(tf_args, nas, \(x, y) x[!y])
  ret <- tfd.list(tf_evals, tf_args, domain = tf_domain(f))
  if (is_tfd(f)) {
    evaluator <- attr(f, "evaluator_name")
    tf_evaluator(ret) <- evaluator
  }
  ret
}

#' @export
#' @rdname tf_jiggle
tf_sparsify.tf_mv <- function(f, dropout = 0.5, same_arg = TRUE, ...) {
  assert_flag(same_arg)
  if (!same_arg) {
    return(map_components(f, \(comp) tf_sparsify(comp, dropout = dropout, ...)))
  }
  tf_mv_assert_shared_arg(f, op = "tf_sparsify")
  tf_mv_map_same_rng(f, \(comp) tf_sparsify(comp, dropout = dropout, ...))
}

tf_component_arg_list <- function(comp, n = vec_size(comp)) {
  args <- ensure_list(tf_arg(comp))
  if (length(args) == 1L && n != 1L) {
    rep(args, n)
  } else {
    args
  }
}

tf_mv_assert_shared_arg <- function(f, op) {
  comps <- tf_components(f)
  n <- vec_size(f)
  if (!length(comps)) {
    return(invisible(TRUE))
  }
  comp_args <- map(comps, tf_component_arg_list, n = n)
  shared <- comp_args[[1]]
  compatible <- map_lgl(comp_args[-1], function(args) {
    length(args) == length(shared) &&
      all(map2_lgl(args, shared, \(x, y) isTRUE(all.equal(x, y))))
  })
  if (any(!compatible)) {
    cli::cli_abort(c(
      "{.fn {op}} with {.code same_arg = TRUE} requires all components to share argument values.",
      i = "Use {.code same_arg = FALSE} to change argument grids independently per component."
    ))
  }
  invisible(TRUE)
}

tf_mv_map_same_rng <- function(f, .f) {
  comps <- tf_components(f)
  if (!length(comps)) return(f)
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  comps <- map(comps, function(comp) {
    assign(".Random.seed", seed, envir = .GlobalEnv)
    .f(comp)
  })
  new_tf_mv(comps, domain = tf_domain(f))
}
