#' Constructors for functional data in basis representation
#'
#' Various constructors for `tfb`-vectors from different kinds of inputs.
#'
#' `tfb` is a wrapper for functions that set up spline-, principal component- or
#' wavelet-based representations of functional data. For all three, the input
#' data \eqn{x_i(t)} are represented as weighted sums of a set of common basis
#' functions \eqn{B_k(t); k = 1,\dots, K} identical for all observations and
#' weight or coefficient vectors \eqn{b_i = (b_{i1}, \dots, b_{iK})} estimated
#' for each observation: \eqn{x_i(t) \approx \sum_k B_k(t) b_{ik}}. Depending on
#' the value of `basis`, the basis functions \eqn{B(t)} will either be `spline`
#' functions or the first few estimated eigenfunctions of the covariance
#' operator of the \eqn{x(t)} (`fpc`) or wavelets (`wavelet`).
#'
#' See **[tfb_spline()]** for more details on spline basis representation (the
#' default). See **[tfb_fpc()]** for using an functional principal component
#' representation with an orthonormal basis estimated from the data instead.
#'
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another
#'   `tf`-object containing functional data.
#' @param basis either "`spline`" (see [tfb_spline()], the default) or "`fpc`"
#'   (see [tfb_fpc()]).
#'   (`wavelet` not implemented yet)
#' @param ... further arguments for [tfb_spline()] or [tfb_fpc()]
#' @returns a `tfb`-object (or a `data.frame`/`matrix` for the conversion
#'   functions, obviously.)
#' @rdname tfb
#' @family tfb-class
#' @export
tfb <- function(data = data.frame(), basis = c("spline", "fpc", "wavelet"), ...) {
  # nocov start
  basis <- match.arg(basis)
  if (vctrs::vec_size(data) == 0) {
    ret <-
      switch(basis,
           spline  = new_tfb_spline(numeric(0), ...),
           fpc     = new_tfb_fpc(numeric(0), ...)) #TODO: wavelet?
    return(ret)
  }
  switch(basis,
         spline  = tfb_spline(data, ...),
         fpc     = tfb_fpc(data, ...),
         wavelet = tfb_wavelet(data, ...))
  # nocov end
}

# nocov start
#' @rdname tfb
tfb_wavelet <- function(data, ...) .NotYetImplemented()

# ------------------------------------------------------------------------------

#' @rdname tfb
#' @export
#' @family tfb-class
as.tfb <- function(data, basis = c("spline", "fpc"), ...) tfb(data, basis, ...)
# nocov end
