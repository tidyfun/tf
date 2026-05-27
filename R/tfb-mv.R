#' @include tfd-mv.R
NULL

#' Vector-valued functional data in basis representation (`f: R -> R^d`)
#'
#' `tfb_mv` is the basis-representation analogue of [tfd_mv()]: it bundles `d`
#' univariate [tfb()] vectors (one per output dimension / component) into a
#' single vctrs vector of vector-valued functions \eqn{f: \mathbb{R} \to
#' \mathbb{R}^d}. Each component is fitted independently with the usual
#' univariate [tfb()] machinery (spline or FPC basis), so all of its arguments
#' (`k`, `bs`, `penalized`, `basis`, ...) apply per component.
#'
#' @param data a [tfd_mv()] / `tfb_mv` object, a (named) `list` of univariate
#'   `tf` vectors, or anything [tfd_mv()] accepts (it is converted to `tfd_mv`
#'   first and then each component is expanded into a basis).
#' @param basis spline (default) or fpc basis, see [tfb()].
#' @param ... forwarded to the univariate [tfb()] constructor.
#' @returns a `tfb_mv` object.
#' @family tf_mv-class
#' @examples
#' traj <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
#' tb <- tfb_mv(traj, k = 7, verbose = FALSE)
#' tb
#' tf_ncomp(tb)
#' @rdname tfb_mv
#' @export
tfb_mv <- function(data, ...) UseMethod("tfb_mv")

#' @rdname tfb_mv
#' @export
tfb_mv.tf_mv <- function(data, basis = c("spline", "fpc"), ...) {
  basis <- match.arg(basis)
  components <- map(tf_components(data), \(comp) tfb(comp, basis = basis, ...))
  new_tf_mv(components, domain = tf_domain(data))
}

#' @rdname tfb_mv
#' @export
tfb_mv.list <- function(data, basis = c("spline", "fpc"), ...) {
  basis <- match.arg(basis)
  if (all(map_lgl(data, is_tf))) {
    components <- map(data, \(comp) {
      if (is_tfb(comp)) comp else tfb(comp, basis = basis, ...)
    })
    return(new_tf_mv(components))
  }
  tfb_mv(tfd_mv(data), basis = basis, ...)
}

#' @rdname tfb_mv
#' @export
tfb_mv.default <- function(data, basis = c("spline", "fpc"), ...) {
  if (missing(data) || vec_size(data) == 0) {
    return(new_tf_mv(list(), class = "tfb_mv"))
  }
  tfb_mv(tfd_mv(data), basis = match.arg(basis), ...)
}

#------------------------------------------------------------------------------

#' @rdname tfb_mv
#' @export
as.tfb_mv <- function(data, ...) UseMethod("as.tfb_mv")

#' @rdname tfb_mv
#' @export
as.tfb_mv.default <- function(data, ...) tfb_mv(data, ...)
