# Re-representation, calculus, smoothing (component-wise) ----------------------

#' @export
tf_rebase.tf_mv <- function(object, basis_from, arg = NULL, ...) {
  cn <- attr(object, "comp_names")
  comps <- tf_components(object)
  # re-express onto a multivariate FPCA basis -> joint re-scoring, not the
  # independent component-wise rebase below.
  if (is_tf_mv(basis_from) && is_tfb_mfpc(basis_from)) {
    return(mfpc_rescore(object, basis_from, arg = arg))
  }
  if (is_tf_mv(basis_from)) {
    check_compatible_mv(object, basis_from)
    bases <- tf_components(basis_from)
    new_comps <- map2(comps, bases, function(o, b) {
      if (is.null(arg)) tf_rebase(o, b, ...) else
        tf_rebase(o, b, arg = arg, ...)
    })
  } else {
    new_comps <- map(comps, function(o) {
      if (is.null(arg)) {
        tf_rebase(o, basis_from, ...)
      } else {
        tf_rebase(o, basis_from, arg = arg, ...)
      }
    })
  }
  names(new_comps) <- cn
  new_tf_mv(new_comps)
}

#' @export
tf_derive.tf_mv <- function(f, arg, order = 1, ...) {
  has_arg <- !missing(arg)
  cn <- attr(f, "comp_names")
  imap_components(
    f,
    function(comp, nm) {
      if (has_arg) {
        tf_derive(
          comp,
          arg = tf_mv_component_arg(arg, nm, cn),
          order = order,
          ...
        )
      } else {
        tf_derive(comp, order = order, ...)
      }
    },
    .op = "tf_derive"
  )
}

#' @export
tf_integrate.tf_mv <- function(f, arg, lower, upper, definite = TRUE, ...) {
  f <- mfpc_demote_for_op(f, "tf_integrate")
  cn <- attr(f, "comp_names") %||% character(0)
  has_arg <- !missing(arg)
  has_lower <- !missing(lower)
  has_upper <- !missing(upper)
  # zero-component object: return a shape-appropriate empty result instead of
  # crashing on results[[1]] below.
  if (!length(cn)) {
    n <- vec_size(f)
    if (definite) {
      return(matrix(
        numeric(0),
        nrow = n,
        ncol = 0,
        dimnames = list(names(f), NULL)
      ))
    }
    return(new_tf_mv(list(), domain = tf_domain(f), class = "tfd_mv"))
  }
  results <- imap(tf_components(f), function(comp, nm) {
    call_args <- list(comp, definite = definite, ...)
    if (has_arg) call_args$arg <- tf_mv_component_arg(arg, nm, cn)
    if (has_lower) call_args$lower <- lower
    if (has_upper) call_args$upper <- upper
    do.call(tf_integrate, call_args)
  })
  if (is.numeric(results[[1]])) {
    mat <- do.call(cbind, results)
    colnames(mat) <- cn
    return(mat)
  }
  names(results) <- cn
  new_tf_mv(results)
}

#' @export
tf_smooth.tf_mv <- function(x, ...) {
  map_components(x, \(comp) tf_smooth(comp, ...), .op = "tf_smooth")
}

#' @export
tf_zoom.tf_mv <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  map_components(
    f,
    \(comp) tf_zoom(comp, begin = begin, end = end, ...),
    .op = "tf_zoom"
  )
}
