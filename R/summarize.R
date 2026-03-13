# used for Summary group generics and stats-methods...
# op has to be a string!
summarize_tf <- function(..., op = NULL, eval = FALSE, verbose = TRUE) {
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

  args <- c(value, arg = list(attr(m, "arg")), domain = list(tf_domain(funs)))
  if (eval) {
    ret <- do.call(tfd, c(args, evaluator = attr(funs, "evaluator_name")))
    empty <- length(ret) == 0
    if (empty) ret <- c(ret, NA)

    if (is_irreg(funs) && !is_irreg(ret)) ret <- as.tfd_irreg(ret)
    if (!is_irreg(funs) && is_irreg(ret)) ret <- as.tfd(ret)

    na_rm <- dots$na.rm %||% FALSE
    if ((empty || is_irreg(ret) && !na_rm) && verbose) {
      if (empty) {
        warn_text <- "NA created."
      } else {
        warn_text <- "Returning irregular {.cls tfd}."
      }
      cli::cli_warn(
        message = c(
          x = warn_text,
          i = "{round(mean(is.na(unlist(value))),2)*100}% of results of argument-wise {.code {op}} were {.code NA}",
          `>` = "Set {.code na.rm = TRUE} and/or interpolate irregular inputs to a regular grid first."
        )
      )
    }
    return(unname(ret))
  }
  ret <- do.call(
    tfb,
    c(args, penalized = FALSE, verbose = FALSE, attr(funs, "basis_args"))
  ) |>
    unname()
}
#-------------------------------------------------------------------------------

#' Functions that summarize `tf` objects across argument values
#'
#' These will return a `tf` object containing the respective *functional*
#' statistic. See [tf_fwise()] for scalar summaries
#' (e.g. `tf_fmean` for means, `tf_fmax` for max. values) of each entry
#' in a `tf`-vector.
#'
#' @param x a `tf` object.
#' @param ... optional additional arguments.
#' @returns a `tf` object with the computed result.\cr
#'   **`summary.tf`** returns a `tf`-vector with the mean function, the functional
#'   median, the *pointwise* min and max of `x`, and the *pointwise* min and max
#'   of the central half of the functions in `x`, as defined by the chosen
#'   `depth` (default `"MBD"`, see [tf_depth()]).
#' @examples
#' set.seed(123)
#' x <- tf_rgp(1) * 1:5
#' mean(x)
#' median(x, depth = "pointwise")
#' sd(x)
#' var(x)
#' summary(x)
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
#'   the median. One of the functional data depths available via [tf_depth()],
#'   `"pointwise"` for a pointwise median function, or a custom depth function
#'   that takes a `tf` vector and returns a numeric vector of depth values.
#' @importFrom stats median
#' @export
#' @rdname tfsummaries
median.tf <- function(x, na.rm = FALSE, depth = "MBD", ...) {
  if (!na.rm && anyNA(x)) {
    return(1 * NA * x[1])
  }
  x <- x[!is.na(x)]
  if (is.character(depth) && length(depth) == 1 && depth == "pointwise") {
    return(summarize_tf(x, na.rm = na.rm, op = "median", eval = is_tfd(x), ...))
  }
  validate_depth(depth)
  tf_depths <- compute_depth(x, depth, na.rm = TRUE, ...)
  med <- x[tf_depths == max(tf_depths)]
  if (length(med) > 1) {
    cli::cli_inform(c(
      x = "{length(med)} observations with maximal depth, returning their mean."
    ))
    ret <- mean(med)
  } else {
    ret <- med
  }
  unname(ret)
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

#' @importFrom stats var
#' @export
#' @rdname tfsummaries
var.default <- stats::var

#' @export
#' @rdname tfsummaries
var.tf <- function(x, y = NULL, na.rm = FALSE, use) {
  summarize_tf(x, na.rm = na.rm, op = "var", eval = is_tfd(x))
}

#' @param object a `tfd` object
#' @param depth depth method used for computing the median, central region, and
#'   (if not `NULL`) min/max. See [tf_depth()] for available methods, or pass a
#'   custom depth function. Defaults to `"MBD"`.
#' @export
#' @rdname tfsummaries
summary.tf <- function(object, ..., depth = "MBD") {
  if (length(object) == 0) {
    ret <- c(object, rep(NA, 6))
    names(ret) <- c("min", "lower_mid", "median", "mean", "upper_mid", "max")
    return(ret)
  }
  validate_depth(depth)
  tf_depths <- compute_depth(object[!is.na(object)], depth, na.rm = TRUE, ...)
  central <- which(tf_depths >= stats::median(tf_depths))
  object_nona <- object[!is.na(object)]

  c(
    min = min(object, na.rm = TRUE),
    lower_mid = min(object_nona[central], na.rm = TRUE),
    median = median(object, na.rm = TRUE, depth = depth, ...),
    mean = mean(object, na.rm = TRUE),
    upper_mid = max(object_nona[central], na.rm = TRUE),
    max = max(object, na.rm = TRUE)
  )
}

#' Tukey's Five Number Summary for `tf` vectors
#'
#' Computes a depth-based five number summary for functional data: the
#' observations with minimum, lower-hinge, median, upper-hinge, and maximum
#' depth values.
#'
#' @param x a `tf` vector (or numeric for the default method).
#' @param na.rm logical; if `TRUE`, `NA` observations are removed first.
#' @param depth depth method for ordering. See [tf_depth()]. Defaults to
#'   `"MHI"` for an up-down ordering.
#' @param ... passed to [tf_depth()].
#' @returns **`fivenum.tf`**: a named `tf` vector of length 5.\cr
#'   **`fivenum.default`**: see [stats::fivenum()].
#' @export
#' @family tidyfun summary functions
#' @name fivenum
fivenum <- function(x, na.rm = FALSE, ...) UseMethod("fivenum")

#' @importFrom stats fivenum
#' @export
#' @rdname fivenum
fivenum.default <- stats::fivenum

#' @export
#' @rdname fivenum
fivenum.tf <- function(x, na.rm = FALSE, depth = "MHI", ...) {
  if (!na.rm && anyNA(x)) {
    return(1 * NA * x[1])
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    ret <- c(x, rep(NA, 5))
    names(ret) <- c("min", "lower_hinge", "median", "upper_hinge", "max")
    return(ret)
  }
  validate_depth(depth)
  d <- compute_depth(x, depth, na.rm = TRUE, ...)
  o <- order(d)
  # pick indices corresponding to fivenum positions
  n <- length(x)
  idx <- c(o[1], o[floor((n + 1) / 4)], o[floor((n + 1) / 2)],
           o[floor(3 * (n + 1) / 4)], o[n])
  ret <- x[idx]
  names(ret) <- c("min", "lower_hinge", "median", "upper_hinge", "max")
  ret
}

#-------------------------------------------------------------------------------
#' @rdname tfgroupgenerics
#' @export
Summary.tf <- function(...) {
  not_defined <- switch(.Generic, all = , any = TRUE, FALSE)
  if (not_defined) {
    cli::cli_abort("{.Generic} not defined for {.cls tf} objects.")
  }
  # min, max, range have dedicated methods that accept a depth argument
  if (.Generic %in% c("min", "max", "range")) {
    return(do.call(.Generic, list(...)))
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
