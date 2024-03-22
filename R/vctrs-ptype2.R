warn_tfd_cast <- function(x, y, to = class(y)[1]) {
 warning("combining incompatible <", class(x)[1], "> with <", class(y)[1],
         "> by casting to <", to, ">.", call. = FALSE)
}

get_larger_domain <- function(x, y) {
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  dom_x_larger <- domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]
  dom_y_larger <- domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]
  if (!(dom_x_larger | dom_y_larger)) {
    stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                           details = "domains incompatible")
  }
  ifelse(dom_x_larger, "x", "y")
}

#-------------------------------------------------------------------------------
#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2 tfd_reg
#' @export
#' @export vec_ptype2.tfd_reg
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfd_reg <- function(x, y, ...) UseMethod("vec_ptype2.tfd_reg")

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfd_reg
#' @export
vec_ptype2.tfd_reg.tfd_reg <- function(x, y, ...) {
  dom_ret <- get_larger_domain(x, y)
  same_args <- same_args(x, y)
  # same grid --> common way to represent x and y is still a tfd_reg
  if (same_args) {
    # return the one with larger domain
    if (dom_ret == "x") return(x)
    if (dom_ret == "y") return(y)
  } else {
    # different grids--> only tfd_irreg can represent x *and* y
    warn_tfd_cast(x, y, "tfd_irreg")
    if (dom_ret == "x")  return(as.tfd_irreg(x))
    if (dom_ret == "y")  return(as.tfd_irreg(y))
  }
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfd_irreg
#' @export
vec_ptype2.tfd_reg.tfd_irreg <- function(x, y, ...) {
  dom_ret <- get_larger_domain(x, y)
  # different grids --> only tfd_irreg can represent x *and* y
  warn_tfd_cast(x, y, "tfd_irreg")
  if (dom_ret == "x") return(as.tfd_irreg(x))
  if (dom_ret == "y") return(y)
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfb_spline
#' @export
vec_ptype2.tfd_reg.tfb_spline <- function(x, y, ...) {
  warn_tfd_cast(x, y, "tfd_reg")
  vec_ptype2(x, as.tfd(y))
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfb_fpc
#' @export
vec_ptype2.tfd_reg.tfb_fpc <- vec_ptype2.tfd_reg.tfb_spline

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2 tfd_irreg
#' @export
#' @export vec_ptype2.tfd_irreg
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfd_irreg <- function(x, y, ...) UseMethod("vec_ptype2.tfd_irreg")

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_irreg tfd_reg
#' @export
vec_ptype2.tfd_irreg.tfd_reg <- function(x, y, ...) {
  vec_ptype2.tfd_reg.tfd_irreg(x = y, y = x)
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_irreg tfd_irreg
#' @export
vec_ptype2.tfd_irreg.tfd_irreg <- function(x, y, ...) {
  dom_ret <- get_larger_domain(x, y)
  # return the one with larger domain
  if (dom_ret == "x") return(x)
  if (dom_ret == "y") return(y)
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_irreg tfb_spline
#' @export
vec_ptype2.tfd_irreg.tfb_spline <- function(x, y, ...) {
  warn_tfd_cast(x, y, "tfd_irreg")
  vec_ptype2(x, as.tfd_irreg(y))
}
#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_irreg tfb_fpc
#' @export
vec_ptype2.tfd_irreg.tfb_fpc <- vec_ptype2.tfd_irreg.tfb_spline

#----------------- s3 generics for tfb coercion -----------------#

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2 tfb_spline
#' @export
#' @param y Vectors to cast.
#' @export vec_ptype2.tfb_spline
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline <- function(x, y, ...) UseMethod("vec_ptype2.tfb_spline")

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_spline tfb_spline
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline.tfb_spline <- function(x, y, ...) {
  same_basis <- isTRUE(all.equal(tf_basis(y)(tf_arg(x)),
                                 attr(x, "basis_matrix"),
                                 check.attributes = FALSE))
  dom_ret <- get_larger_domain(x, y)
  if (same_basis && dom_ret == "x") return(x)
  if (same_basis && dom_ret == "y") return(y)
  # joint representation for different bases/domains is some tfd
  warn_tfd_cast(x, y, "tfd_reg")
  vec_ptype2(as.tfd(x), as.tfd(y))
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_spline tfb_fpc
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline.tfb_fpc <- function(x, y, ...) {
  # joint representation for different bases/domains is some tfd
  warn_tfd_cast(x, y, "tfd_reg")
  vec_ptype2(as.tfd(x), as.tfd(y))
}


#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_spline tfd_reg
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline.tfd_reg <- function(x, y, ...) {
  vec_ptype2(x = y, y = x)
}
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_spline tfd_irreg
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline.tfd_irreg <- function(x, y, ...) {
  vec_ptype2(x = y, y = x)
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2 tfb_fpc
#' @export
#' @export vec_ptype2.tfb_fpc
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc <- function(x, y, ...) UseMethod("vec_ptype2.tfb_fpc")

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_fpc tfb_spline
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc.tfb_spline <- vec_ptype2.tfb_spline.tfb_fpc


#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_fpc tfb_fpc
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc.tfb_fpc <- vec_ptype2.tfb_spline.tfb_spline

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_fpc tfd_reg
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc.tfd_reg <- function(x, y, ...) {
  vec_ptype2(x = y, y = x)
}
#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_fpc tfd_irreg
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_fpc.tfd_irreg <- function(x, y, ...) {
  vec_ptype2(x = y, y = x)
}
