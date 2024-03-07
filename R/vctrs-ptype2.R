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
  if (domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]) return(x)
  if (domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]) return(y)
  stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                         details = "domains incompatible")
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_reg tfd_irreg
#' @export
vec_ptype2.tfd_reg.tfd_irreg <- function(x, y, ...) {
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  if (domains[1,1] >= domains[1,2] && domains[2,1] <= domains[2,2]) return(y)
  stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                         details = "domains incompatible")
}

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
  domains <- cbind(x = tf_domain(x), y = tf_domain(y))
  if (domains[1,1] <= domains[1,2] && domains[2,1] >= domains[2,2]) return(x)
  stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                         details = "domains incompatible")
}

#' @name vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfd_irreg tfd_irreg
#' @export
vec_ptype2.tfd_irreg.tfd_irreg <- vec_ptype2.tfd_reg.tfd_reg


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
                                 attr(x, "basis_matrix")))
  same_domain <- all(tf_domain(x) == tf_domain(y))
  if (same_basis && same_domain) return(x)
  details <- paste0("different ", ifelse(!same_basis, "bases", ""),
                    ifelse(!same_domain & !same_basis, " and ", ""),
                    ifelse(!same_domain, "domains", ""))
  stop_incompatible_type(x, y, x_arg = "", y_arg = "",
                         details = details)
}

#' @rdname vctrs
#' @family tidyfun vctrs
#' @method vec_ptype2.tfb_spline tfb_fpc
#' @export
#' @inheritParams vctrs::vec_ptype2
vec_ptype2.tfb_spline.tfb_fpc <- function(x, y, ...) {
  stop_incompatible_type(x, y, x_arg = "", y_arg = "")
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

#----------------- main function for coercion of tfd -----------------#



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
