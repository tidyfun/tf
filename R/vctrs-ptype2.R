warn_tfd_cast <- function(x, y, to = class(y)[1]) {
 warning("combining incompatible <", class(x)[1], "> with <", class(y)[1],
         "> by casting to <", to, ">.", call. = FALSE)
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
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  dom_x_larger <- domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]
  dom_y_larger <- domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]
  if (!(dom_x_larger | dom_y_larger)) {
    stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                           details = "domains incompatible")
  }
  same_args <- same_args(x, y)
  # same grid --> common way to represent x and y is still a tfd_reg
  if (same_args) {
    # return the one with larger domain
    if (dom_x_larger) return(x)
    if (dom_y_larger) return(y)
  } else {
    # different grids--> only tfd_irreg can represent x *and* y
    warn_tfd_cast(x, y, "tfd_irreg")
    if (dom_x_larger) return(as.tfd_irreg(x))
    if (dom_y_larger) return(as.tfd_irreg(y))
  }
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfd_irreg
#' @export
vec_ptype2.tfd_reg.tfd_irreg <- function(x, y, ...) {
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  dom_x_larger <- domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]
  dom_y_larger <- domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]
  if (!(dom_x_larger | dom_y_larger)) {
    stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                           details = "domains incompatible")
  }
  # different grids --> only tfd_irreg can represent x *and* y
  warn_tfd_cast(x, y, "tfd_irreg")
  if (dom_x_larger) return(as.tfd_irreg(x))
  if (dom_y_larger) return(y)
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
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  dom_x_larger <- domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]
  dom_y_larger <- domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]
  if (!(dom_x_larger | dom_y_larger)) {
    stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                         details = "domains incompatible")
  }
  # return the one with larger domain
  if (dom_x_larger) return(x)
  if (dom_y_larger) return(y)
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
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  dom_x_larger <- domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]
  dom_y_larger <- domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]
  if (same_basis && dom_x_larger) return(x)
  if (same_basis && dom_y_larger) return(y)
  if (!(dom_x_larger | dom_y_larger)) {
    stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                           details = "domains incompatible")
  }
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

#----------------- main function for coercion of tfd -----------------#

# c_names <- function(funs) {
#   fnames <- as.list(names(funs) %||% rep("", length(funs)))
#   elnames <- map(funs, \(x) names(x) %||% rep("", length(x)))
#   # always use argnames
#   # argnames replace elementnames if elments have length 1
#   # else paste with "."
#   names <- map2(fnames, elnames, \(x, y) {
#     if (nzchar(x, keepNA = TRUE)) {
#       return(y)
#     }
#     if (all(nzchar(y, keepNA = TRUE)) || length(y) == 1) {
#       return(rep(x, length(y)))
#     }
#     paste(x, y, sep = ".")
#   }) |>
#     unlist()
#   if (all(nzchar(names, keepNA = TRUE))) NULL else names
# }

# vec_ptype2_tfd_tfd <- function(x, y, ...) {
#   funs <- list(x, y)
#   compatible <- do.call(rbind, map(funs, \(x) compare_tf_attribs(funs[[1]], x)))
#
#   stopifnot(all(compatible[, "domain"]))
#   make_irreg <- rep(FALSE, length(funs))
#   irreg <- map_lgl(funs, is_irreg)
#   if (!any(irreg) && !all(compatible[, "arg"])) {
#     warning("concatenating functions on different grids.", call. = FALSE)
#     make_irreg <- rep(TRUE, length(funs))
#   }
#   if (any(irreg) && !all(irreg)) {
#     warning("concatenating functions on different grids.", call. = FALSE)
#     make_irreg[!irreg] <- TRUE
#   }
#   if (any(make_irreg)) {
#     funs <- map_at(funs, which(make_irreg), as.tfd_irreg)
#   }
#   if (!all(compatible[, "evaluator_name"])) {
#     warning(
#       "inputs have different evaluators, result has ",
#       attr(funs[[1]], "evaluator_name"),
#       call. = FALSE
#     )
#   }
#   attr_ret <- attributes(funs[[1]])
#   if (any(irreg | make_irreg)) {
#     attr_ret$arg <- flatten(map(funs, tf_arg))
#   }
#   attr_ret$names <- {
#     tmp <- unlist(flatten(map(
#       funs,
#       function(x) names(x) %||% rep("", length(x))
#     )))
#     if (all(nzchar(tmp, keepNA = TRUE))) NULL else tmp
#   }
#   ret <- flatten(funs)
#   attributes(ret) <- attr_ret
#   setNames(ret, c_names(funs))
# }









#----------------- main function for coercion of tfb -----------------#


# vec_ptype2_tfb_tfb <- function(x, y, ...) {
#   funs <- list(x, y)
#   compatible <- do.call(rbind, map(funs, \(x) compare_tf_attribs(funs[[1]], x)))
#   stopifnot(all(compatible[, "domain"]))
#
#   if (inherits(funs[[1]], "tfb_spline")) {
#     re_evals <- which(
#       !compatible[, "arg"] | !compatible[, "basis_args"]
#     )
#     if (length(re_evals)) {
#       fun_names <- map(as.list(match.call())[-1], \(x) deparse(x)[1])
#       warning(
#         "re-evaluating ", toString(fun_names[re_evals]),
#         " using basis and arg of ", fun_names[1],
#         call. = FALSE
#       )
#
#       funs <- map_at(
#         funs, re_evals,
#         \(x) do.call(
#           tfb,
#           flatten(list(list(x), # converts to tfb then back to tfd
#                        arg = list(tf_arg(funs[[1]])),
#                        attr(funs[[1]], "basis_args")
#           ))
#         )
#       )
#     }
#   } else {
#     re_evals <- which(
#       !compatible[, "arg"] | !compatible[, "basis_matrix"]
#     )
#
#     if (length(re_evals)) {
#       stop(
#         "concatenation not yet implemented for tfb_fpc vectors with different bases",
#         call. = FALSE
#       )
#     }
#   }
#
#   attr_ret <- attributes(funs[[1]])
#   attr_ret$names <- {
#     tmp <- unlist(flatten(map(
#       funs,
#       function(x) names(x) %||% rep("", length(x))
#     )))
#     if (all(nzchar(tmp, keepNA = TRUE))) NULL else tmp
#   }
#   ret <- flatten(funs)
#   attributes(ret) <- attr_ret
#   setNames(ret, c_names(funs))
# }
