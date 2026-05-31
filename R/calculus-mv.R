# Re-representation, calculus, smoothing (component-wise) ----------------------

#' @export
tf_rebase.tf_mv <- function(object, basis_from, arg = NULL, ...) {
  cn <- attr(object, "comp_names")
  comps <- tf_components(object)
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
  map_components(f, function(comp) {
    if (has_arg) {
      tf_derive(comp, arg = arg, order = order, ...)
    } else {
      tf_derive(comp, order = order, ...)
    }
  })
}

#' @export
tf_integrate.tf_mv <- function(f, arg, lower, upper, definite = TRUE, ...) {
  cn <- attr(f, "comp_names")
  has_arg <- !missing(arg)
  has_lower <- !missing(lower)
  has_upper <- !missing(upper)
  results <- map(tf_components(f), function(comp) {
    call_args <- list(comp, definite = definite, ...)
    if (has_arg) call_args$arg <- arg
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
  map_components(x, \(comp) tf_smooth(comp, ...))
}

#' @export
tf_zoom.tf_mv <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  map_components(f, \(comp) tf_zoom(comp, begin = begin, end = end, ...))
}
