# used for Summary group generics and stats-methods...
# op has to be a string!
summarize_tf <- function(..., op = NULL, eval = FALSE) {
  dots <- list(...)
  funs <- map_lgl(dots, is_tf)
  op_args <- dots[!funs]
  funs <- dots[funs]
  op_call <- function(x) do.call(op, c(list(x), op_args))
  funs <- do.call(c, funs)
  # setting interpolate = TRUE would return more useful results for tfd_irreg
  # - not done here for transparency reasons.
  m <- suppressWarnings(as.matrix(funs))
  value <- apply(m, 2, op_call) |> unname() |> list()
  args <- c(value,
            arg = list(attr(m, "arg")),
            domain = list(tf_domain(funs))
  )
  if (eval) {
    ret <- do.call(tfd, c(args, evaluator = attr(funs, "evaluator_name")))
    if (is_irreg(funs) && !is_irreg(ret)) ret <- as.tfd_irreg(ret)
    if (!is_irreg(funs) && is_irreg(ret)) ret <- as.tfd(ret)
    return(ret)
  } else {
    return(do.call(tfb, c(args,
                          penalized = FALSE, verbose = FALSE,
                          attr(funs, "basis_args")
    )))
  }
}
#-------------------------------------------------------------------------------

#' Functions that summarize `tf` objects across argument values
#'
#' These will return a `tf` object containing the respective *functional*
#' statistic. See [tf_fwise()] for scalar summaries
#' (e.g. `tf_fmean` for means, `tf_fmax` for max. values) of each entry
#' in a `tf`-vector.
#'
#' @param x a `tf` object
#' @param ... optional additional arguments.
#' @returns a `tf` object with the computed result.\cr
#' **`summary.tf`** returns a `tf`-vector with the mean function, the
#' variance function, the functional median, and the functional range
#' (i.e., *pointwise* min/max) of the central half of the functions,
#' as defined by [tf_depth()].
#' @name tfsummaries
#' @family tidyfun summary functions
#' @seealso [tf_fwise()]
NULL

#' @export
#' @rdname tfsummaries
mean.tf <- function(x, ...) {
  summarize_tf(x, op = "mean", eval = is_tfd(x), ...)
}

#' @param depth method used to determine the most central element in `x`, i.e.,
#'   the median. One of the functional data depths available via [tf_depth()] or
#'   `"pointwise"` for a pointwise median function.
#' @importFrom stats median
#' @export
#' @rdname tfsummaries
median.tf <- function(x, na.rm = FALSE, depth = c("MBD", "pointwise"), ...) {
  if (!na.rm) {
    if (anyNA(x)) return(1 * NA * x[1])
  } else {
    x <- x[!is.na(x)]
  }
  depth <- match.arg(depth)
  if (depth == "pointwise") {
    summarize_tf(x, na.rm = na.rm, op = "median", eval = is_tfd(x), ...)
  } else {
    tf_depths <- tf_depth(x, depth = depth)
    med <- x[tf_depths == max(tf_depths)]
    if (length(med) > 1) {
      warning(
        length(med), " observations with maximal depth, returning their mean.",
        call. = FALSE
      )
      mean(med)
    } else {
      med
    }
  }
}

#' @inheritParams stats::sd
#' @export
#' @rdname tfsummaries
sd <- function(x, na.rm = FALSE) UseMethod("sd")

#' @importFrom stats sd
#' @export
#' @rdname tfsummaries
sd.default <- stats::sd

#' @export
#' @rdname tfsummaries
sd.tf <- function(x, na.rm = FALSE) {
  summarize_tf(x, na.rm = na.rm, op = "sd", eval = is_tfd(x))
}

#' @inheritParams stats::var
#' @export
#' @rdname tfsummaries
var <- function(x, y = NULL, na.rm = FALSE, use) UseMethod("var")

#' @export
#' @importFrom stats sd
#' @rdname tfsummaries
var.default <- stats::var

#' @export
#' @rdname tfsummaries
var.tf <- function(x, y = NULL, na.rm = FALSE, use) {
  summarize_tf(x, na.rm = na.rm, op = "var", eval = is_tfd(x))
}

# cov / cor # needs image class/fpca methods
#' @param object a `tfd` object
#' @export
#' @rdname tfsummaries
summary.tf <- function(object, ...) {
  tf_depths <- tf_depth(object, ...)
  central <- which(tf_depths <= median(tf_depths))
  central_half <- range(object[central])
  c(
    mean = mean(object), var = var(object),
    median = object[which.max(tf_depths)] |> unname(),
    upper_mid =  central_half[1], lower_mid =  central_half[2]
  )
}

#-------------------------------------------------------------------------------
# nocov start
#' @rdname tfgroupgenerics
#' @export
Summary.tf <- function(...) {
  not_defined <- switch(.Generic,
                        `all` = , `any` = TRUE, FALSE
  )
  if (not_defined) {
    stop(sprintf("%s not defined for \"tf\" objects", .Generic))
  }
  summarize_tf(..., op = .Generic, eval = is_tfd(list(...)[[1]]))
}

#' @rdname tfgroupgenerics
#' @export
cummax.tfd <- function(...) {
  summarize_tf(..., op = "cummax", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cummin.tfd <- function(...) {
  summarize_tf(..., op = "cummin", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cumsum.tfd <- function(...) {
  summarize_tf(..., op = "cumsum", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
#' @family tidyfun compute
cumprod.tfd <- function(...) {
  summarize_tf(..., op = "cumprod", eval = TRUE)
}
#' @rdname tfgroupgenerics
#' @export
cummax.tfb <- function(...) {
  summarize_tf(..., op = "cummax", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cummin.tfb <- function(...) {
  summarize_tf(..., op = "cummin", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cumsum.tfb <- function(...) {
  summarize_tf(..., op = "cumsum", eval = FALSE)
}
#' @rdname tfgroupgenerics
#' @export
cumprod.tfb <- function(...) {
  summarize_tf(..., op = "cumprod", eval = FALSE)
}
# nocov end
