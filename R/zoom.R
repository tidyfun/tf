#' Functions to zoom in/out on functions
#'
#' These are used to redefine or restrict the `domain` of `tf` objects.
#'
#' @param f a `tf`-object
#' @param begin numeric vector of length 1 or `length(f)`.
#'  Defaults to the lower limit of the domain of `f`.
#' @param end numeric vector of length 1 or `length(f)`.
#'  Defaults to the upper limit of the domain of `f`.
#' @param ... not used
#' @returns an object like `f` on a new domain (potentially).
#' Note that regular functional data and functions in basis representation will
#'   be turned into irregular `tfd`-objects iff `begin` or `end` are not scalar.
#' @family tidyfun utility functions
#' @export
#' @examples
#' x <- tf_rgp(10)
#' plot(x)
#' tf_zoom(x, 0.5, 0.9)
#' tf_zoom(x, 0.5, 0.9) |> lines(col = "red")
#' tf_zoom(x, seq(0, 0.5, length.out = 10), seq(0.5, 1, length.out = 10)) |>
#'   lines(col = "blue", lty = 3)
tf_zoom <- function(f, begin, end, ...) {
  UseMethod("tf_zoom")
}

prep_tf_zoom_args <- function(f, begin, end) {
  assert_numeric(begin, any.missing = FALSE, min.len = 1, max.len = length(f))
  assert_numeric(end, any.missing = FALSE, min.len = 1, max.len = length(f))
  regular <- TRUE
  # uses unique to homogenize and check regularity in one go
  if (n_distinct(begin) == 1) {
    begin <- rep(begin[1], length(f))
  } else {
    regular <- FALSE
  }
  if (n_distinct(end) == 1) {
    end <- rep(end[1], length(f))
  } else {
    regular <- FALSE
  }
  if (length(begin) != length(end) || !all(begin < end)) {
    cli::cli_abort(
      "{.arg begin} and {.arg end} must be of same length and ordered."
    )
  }
  new_domain <- c(min(begin), max(end))
  list(begin = begin, end = end, dom = new_domain, regular = regular)
}

#' @rdname tf_zoom
#' @export
tf_zoom.tfd <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  args <- prep_tf_zoom_args(f, begin, end)
  arg <- NULL
  ret <- pmap(
    list(f[, tf_arg(f), matrix = FALSE], args$begin, args$end),
    \(x, y, z) subset(x, arg >= y & arg <= z)
  )
  if (is_irreg(f) || !args$regular) {
    nas <- map_int(ret, nrow) == 0
    if (all(nas)) cli::cli_abort("No data in zoom region.")
    if (any(nas))
      cli::cli_warn(
        "{.code NA}s created by re-evaluating on new grid in {.fn tf_zoom}."
      )
    for (n in which(nas)) {
      ret[[n]] <- data_frame0(arg = unname(args$dom[1]), value = NA_real_)
    }
  } else if (any(map_int(ret, nrow) == 0)) {
    cli::cli_abort("No data in zoom region.")
  }
  ret <- tfd(ret, domain = args$dom)
  tf_evaluator(ret) <- attr(f, "evaluator_name")
  ret
}

#' @rdname tf_zoom
#' @export
tf_zoom.tfb <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  args <- prep_tf_zoom_args(f, begin, end)
  if (!args$regular) {
    cli::cli_inform(c(
      x = "{.fn tf_zoom} was called with varying start or end points - converting to {.code tfd}."
    ))
    return(tf_zoom(tfd(f), begin, end))
  }
  use <- tf_arg(f) >= args$dom[1] & tf_arg(f) <= args$dom[2]
  if (!any(use)) cli::cli_abort("No data in zoom region.")
  ret <- f
  attr(ret, "basis_matrix") <- attr(f, "basis_matrix")[use, ]
  attr(ret, "arg") <- tf_arg(f)[use]
  attr(ret, "domain") <- args$dom
  ret
}

#' @rdname tf_zoom
#' @export
tf_zoom.tfb_fpc <- function(
  f,
  begin = tf_domain(f)[1],
  end = tf_domain(f)[2],
  ...
) {
  cli::cli_warn("FPC basis no longer orthonormal on sub-domain.")
  NextMethod()
}
