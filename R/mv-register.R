# Registration: one shared time-warp per curve, applied to all components ------

# univariate signal used to estimate the (joint) warp for a multivariate curve
mv_registration_signal <- function(x, ref_component = 1L) {
  assert_tf_mv(x)
  if (is.function(ref_component)) {
    return(ref_component(x))
  }
  if (identical(ref_component, "norm")) {
    return(tf_norm(x))
  }
  # otherwise a component name or index -- tf_component() validates it.
  tf_component(x, ref_component)
}

#' @export
tf_warp.tf_mv <- function(x, warp, ...) {
  map_components(x, \(comp) tf_warp(comp, warp, ...))
}

#' @export
tf_align.tf_mv <- function(x, warp, ...) {
  map_components(x, \(comp) tf_align(comp, warp, ...))
}

#' @export
tf_estimate_warps.tf_mv <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "cc", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2,
  ref_component = 1L
) {
  method <- match.arg(method)
  assert_count(max_iter, positive = TRUE)
  assert_number(tol, lower = 0, finite = TRUE)
  signal <- mv_registration_signal(x, ref_component)
  tmpl <- if (is_tf_mv(template)) {
    mv_registration_signal(template, ref_component)
  } else {
    template
  }
  warps <- tf_estimate_warps(
    signal,
    ...,
    template = tmpl,
    method = method,
    max_iter = max_iter,
    tol = tol
  )
  # drop the (univariate) template attribute so tf_register() derives a
  # multivariate template via mean() of the aligned components instead.
  attr(warps, "template") <- NULL
  warps
}
