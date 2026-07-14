#' @include tfd-mv.R tfb-mv.R
NULL

# vctrs integration for vector-valued functional data --------------------------
#
# The proxy of a `tf_mv` is a data.frame with `n` rows and `d` columns, one
# per component (each column is itself a univariate `tf` vector). This is the
# only piece of genuinely new vctrs machinery: it makes `vec_slice()`,
# `vec_c()`, casting and tibble-column behaviour all fall out of the existing
# *univariate* vctrs methods applied column-/component-wise. `vec_restore()`
# rebuilds the multivariate wrapper from the (sliced / concatenated) proxy.

#' @export
vec_proxy.tf_mv <- function(x, ...) {
  components <- attr(x, "components")
  # NB: must NOT call vec_size(x) here -- that would recurse through vec_proxy.
  if (!length(components)) {
    return(vctrs::new_data_frame(n = length(unclass(x))))
  }
  vctrs::new_data_frame(unclass(components), n = vec_size(components[[1]]))
}

#' @export
vec_restore.tf_mv <- function(x, to, ...) {
  components <- as.list(x)
  # An MFPC fit carries a curve-independent joint spec. Slicing reuses the
  # original object as `to`, so the spec (and the ability to re-score) is
  # preserved. Concatenation (`vec_c`/`c`) builds `to` via `tf_mv_ptype2()`,
  # which forwards an `identical()`-matching spec (same fit, e.g. `c(mf[1:4],
  # mf[5:8])`) and otherwise demotes with a warning -- so reading the spec off
  # `to` here is correct for both paths.
  mfpc <- attr(to, "mfpc")
  if (!length(components)) {
    return(new_tf_mv(
      list(),
      domain = attr(to, "domain"),
      class = class(to)[1],
      mfpc = mfpc
    ))
  }
  new_tf_mv(components, check_curve_names = FALSE, mfpc = mfpc)
}

#-------------------------------------------------------------------------------

check_compatible_mv <- function(x, y) {
  if (tf_ncomp(x) != tf_ncomp(y)) {
    stop_incompatible_type(
      x,
      y,
      x_arg = "",
      y_arg = "",
      details = "different number of components"
    )
  }
  if (!identical(attr(x, "comp_names"), attr(y, "comp_names"))) {
    stop_incompatible_type(
      x,
      y,
      x_arg = "",
      y_arg = "",
      details = "different component names"
    )
  }
  invisible(TRUE)
}

tf_mv_ptype2 <- function(x, y, ...) {
  check_compatible_mv(x, y)
  comps <- map2(tf_components(x), tf_components(y), \(a, b) vec_ptype2(a, b))
  # Carry the joint MFPC spec through `vec_c()` when *all* inputs are the same
  # fit, i.e. carry an `identical()` `mfpc` attribute. `vec_ptype2()` is called
  # pairwise so an identical match here -- combined with the matching pairwise
  # checks performed by `vec_c()` -- implies all inputs share the spec.
  # The result of `vec_ptype2` becomes `to` in `vec_restore.tf_mv`, where the
  # spec is then re-stamped onto the concatenation. When specs differ (or one
  # side is a plain `tfb_mv` / `tfd_mv` with no spec) we warn and demote -- the
  # downgrade matters because a `dplyr::bind_rows` round-trip otherwise
  # silently strips the spec.
  mfpc_x <- attr(x, "mfpc")
  mfpc_y <- attr(y, "mfpc")
  proto <- new_tf_mv(comps)
  same_spec <- identical(mfpc_x, mfpc_y) && !is.null(mfpc_x)
  if (same_spec && is_tfb_mv(proto)) {
    attr(proto, "mfpc") <- mfpc_x
  } else if (!is.null(mfpc_x) || !is.null(mfpc_y)) {
    # warn only when at least one side actually had a spec to lose. The proto
    # may have demoted further to `tfd_mv` (when component bases differ) -- the
    # extra demote_warning is still useful: the per-component
    # `vec_ptype2.tfb_fpc.tfb_fpc` cast-warning does not mention MFPC.
    warn_mfpc_demotion(
      "Combining MFPC fits with different (or missing) joint specs."
    )
  }
  proto
}

tf_mv_cast <- function(x, to, ...) {
  # casting *onto* a multivariate FPCA basis means jointly re-scoring the new
  # data (per-component casting would give wrong, component-local scores).
  # Reconstruction (`to` a plain tfd_mv) keeps the component-wise path below.
  if (is_tfb_mfpc(to)) {
    return(mfpc_rescore(x, to))
  }
  check_compatible_mv(x, to)
  comps <- map2(tf_components(x), tf_components(to), \(a, b) vec_cast(a, b))
  new_tf_mv(comps)
}

#' @rdname vctrs
#' @name vctrs
#' @export
vec_ptype2.tfd_mv.tfd_mv <- function(x, y, ...) tf_mv_ptype2(x, y)
#' @rdname vctrs
#' @export
vec_ptype2.tfb_mv.tfb_mv <- function(x, y, ...) tf_mv_ptype2(x, y)
# mixing a tfd_mv with a tfb_mv combines component-wise via the univariate
# vec_ptype2(tfd, tfb), which resolves to tfd -- so the common type of a
# tfd_mv and a tfb_mv is a tfd_mv (same demotion as in the univariate case).
#' @rdname vctrs
#' @export
vec_ptype2.tfd_mv.tfb_mv <- function(x, y, ...) tf_mv_ptype2(x, y)
#' @rdname vctrs
#' @export
vec_ptype2.tfb_mv.tfd_mv <- function(x, y, ...) tf_mv_ptype2(x, y)

#' @rdname vctrs
#' @export
vec_cast.tfd_mv.tfd_mv <- function(x, to, ...) tf_mv_cast(x, to)
#' @rdname vctrs
#' @export
vec_cast.tfb_mv.tfb_mv <- function(x, to, ...) tf_mv_cast(x, to)
#' @rdname vctrs
#' @export
vec_cast.tfd_mv.tfb_mv <- function(x, to, ...) tf_mv_cast(x, to)
#' @rdname vctrs
#' @export
vec_cast.tfb_mv.tfd_mv <- function(x, to, ...) tf_mv_cast(x, to)

#-------------------------------------------------------------------------------

#' @export
vec_ptype_abbr.tfd_mv <- function(x, ...) "tfd_mv"

#' @export
vec_ptype_abbr.tfb_mv <- function(x, ...) "tfb_mv"

#' @export
vec_ptype_full.tfd_mv <- function(x, ...) {
  paste0("tfd_mv<d=", tf_ncomp(x), ">")
}

#' @export
vec_ptype_full.tfb_mv <- function(x, ...) {
  paste0("tfb_mv<d=", tf_ncomp(x), ">")
}
