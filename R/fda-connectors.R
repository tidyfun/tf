#' Fourier basis for mgcv
#'
#' A `mgcv`-style smooth constructor for Fourier bases, used internally
#' by [tfb_spline()] when `bs = "fourier"`.
#'
#' @param object a smooth specification object from `mgcv`.
#' @param data a list containing the data vector.
#' @param knots not used.
#' @returns a smooth specification object with the Fourier basis matrix `X`
#'   and optional second-derivative penalty `S`.
#' @export
#' @name fourier.smooth
#' @keywords internal
#' @examples
#' \donttest{
#' # used internally via tfb_spline:
#' f <- c(sin(2 * pi * (0:100) / 100), cos(2 * pi * (0:100) / 100))
#' tf_smooth <- tfb_spline(f, bs = "fourier", k = 11)
#' }
smooth.construct.fourier.smooth.spec <- function(object, data, knots) {
  x <- data[[object$term]]
  n <- length(x)

  # enforce odd bs.dim
  bs.dim <- object$bs.dim
  if (bs.dim <= 0)
    cli::cli_abort("Fourier basis needs a positive {.arg bs.dim}.")
  if (bs.dim %% 2 == 0) {
    cli::cli_inform(
      "Fourier basis dimension is even, incrementing by 1 to enforce odd."
    )
    bs.dim <- bs.dim + 1
  }
  object$bs.dim <- bs.dim

  # figure out range, period
  #   store them so Predict.matrix can replicate
  rng <- object$xt$rangeval %||% range(x)
  period <- object$xt$period %||% diff(rng)

  # build design matrix
  # column 1 = constant, then sine/cosine pairs
  # k is odd, so M = (k - 1) / 2
  X <- matrix(0, n, bs.dim)

  # adapted from fda::fourier
  omega <- 2 * pi / period
  omega_x <- omega * x
  j <- seq(2, bs.dim - 1, 2)
  args <- outer(omega_x, j / 2)
  X[, j] <- sin(args)
  X[, j + 1] <- cos(args)
  # rescale as in fda
  X[, 1] <- 1 / sqrt(2)
  X <- X / sqrt(period / 2)

  # store design, no penalty
  object$X <- X # rescale as in fda

  if (!object$fixed) {
    # construct penalty
    S <- matrix(0, bs.dim, bs.dim)

    # for j=1..M, the second-derivative penalty coefficient is:
    #    (period/2) * (k_j^4)
    # where k_j = 2*pi*j / period
    # these go on the diagonal for sin and cos columns.
    for (j in seq_len((bs.dim - 1) / 2)) {
      kj <- (2 * pi * j / period)
      pen_val <- (period / 2) * (kj^4)
      # sin gets index: 2 + 2*(j-1)
      # cos gets index: 3 + 2*(j-1)
      sin_idx <- 2 + 2 * (j - 1)
      cos_idx <- sin_idx + 1
      S[sin_idx, sin_idx] <- pen_val
      S[cos_idx, cos_idx] <- pen_val
    }

    # store in object$S
    object$S <- list(S)
    # the rank is effectively k-1 because the constant isn't penalized
    object$rank <- bs.dim - 1
    # 1D null space from constant function
    object$null.space.dim <- 1
  } else {
    object$S <- list()
    object$rank <- 0
    object$null.space.dim <- bs.dim
  }
  # ensure mgcv doesn't impose sum-to-zero or other constraints
  object$C <- matrix(0, 0, bs.dim)

  # store info needed for predictions
  object$rangeval <- rng
  object$period <- period

  class(object) <- "fourier.smooth"
  object
}

#' @param object a fitted `fourier.smooth` object.
#' @param data a list containing the data vector for prediction.
#' @returns a design matrix evaluated at the new data points.
#' @rdname fourier.smooth
#' @export
Predict.matrix.fourier.smooth <- function(object, data) {
  x <- data[[object$term]]
  n <- length(x)

  # replicate transform
  rng <- object$rangeval
  period <- object$period
  bs.dim <- object$bs.dim

  X <- matrix(0, n, bs.dim)

  omega <- 2 * pi / period
  omega_x <- omega * x
  j <- seq(2, bs.dim - 1, by = 2)
  args <- outer(omega_x, j / 2)
  X[, j] <- sin(args)
  X[, j + 1] <- cos(args)
  # rescale as in fda
  X[, 1] <- 1 / sqrt(2)
  X <- X / sqrt(period / 2)
  X
}

#-------------------------------------------------------------------------------
