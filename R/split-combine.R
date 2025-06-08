#' Split / Combine functional fragments
#'
#' `tf_split` separates each function into a vector of functions defined on a sub-interval of
#' its domain, either with overlap at the cut points or without.
#'
#' @param x a `tf` object.
#' @param splits numeric vector containing `arg`-values at which to split.
#' @param include which of the end points defined by `splits` to include in each
#'  of the resulting split functions. Defaults to `"both"`, other options are "`left`" or
#'   "`right`". See examples.
#'
#' @return for `tf_split`: a list of `tf` objects.
#' @export
#' @rdname tf_splitcombine
#'
#' @examples
#' x <- tfd(1:100, arg = 1:100)
#' tf_split(x, splits = c(20, 80))
#' tf_split(x, splits = c(20, 80), include = "left")
#' tf_split(x, splits = c(20, 80), include = "right")
tf_split <- function(x, splits, include = c("both", "left", "right")) {
  assert_tf(x)
  assert_numeric(
    splits,
    lower = tf_domain(x)[1],
    upper = tf_domain(x)[2],
    any.missing = FALSE,
    sorted = TRUE,
    unique = TRUE
  )
  include <- match.arg(include)
  resolution_x <- get_resolution(tf_arg(x))
  # if user supplied domain limit(s), remove
  if (splits[1] == tf_domain(x)[1]) {
    splits <- splits[-1]
  }
  if (splits[length(splits)] == tf_domain(x)[2]) {
    splits <- splits[-length(splits)]
  }

  start <- c(tf_domain(x)[1], splits)
  end <- c(splits, tf_domain(x)[2])
  if (include == "left") {
    end[1:(length(end) - 1)] <- head(end, -1) - resolution_x
  }
  if (include == "right") {
    start[-1] <- start[-1] + resolution_x
  }

  map2(start, end, \(a, b) tf_zoom(x, begin = a, end = b))
}


#' @description
#' `tf_combine` joins functional fragments together to create longer (or more densely evaluated) functions.
#' @param ... `tf`-objects of identical lengths to combine
#' @param strict only combine functions whose argument ranges do not overlap,
#'   are given in the correct order & contain no duplicate values at identical arguments?
#'   defaults to `FALSE`. If `strict == FALSE`, only the first function values at duplicate locations
#'   are used, the rest are discarded (with a warning).
#'
#' @return for `tf_combine`: a `tfd` with the combined subfunctions on the union of the input `tf_arg`-values
#' @export
#' @rdname tf_splitcombine
#' @aliases tf_splitcombine
#'
#' @examples
#'   x <- tf_rgp(5)
#'   tfs <- tf_split(x, splits = c(.2, .6))
#'   x2 <- tf_combine(tfs[[1]], tfs[[2]], tfs[[3]])
#'   # tf_combine(tfs[[1]], tfs[[2]], tfs[[3]], strict = TRUE) # errors out - duplicate values!
#'   all.equal(x, x2)
#'   # combine works for different input types:
#'   tfs2_sparse <- tf_sparsify(tfs[[2]])
#'   tfs3_spline <- tfb(tfs[[3]])
#'   tf_combine(tfs[[1]], tfs2_sparse, tfs3_spline)
#'   # combine(.., strict = F) can be used to coalesce different measurements
#'   # of the same process over different grids:
#'   x1 <- tfd(x, arg = tf_arg(x)[seq(1, 51, by = 2)])
#'   x2 <- tfd(x, arg = tf_arg(x)[seq(2, 50, by = 2)])
#'   tf_combine(x2, x1, strict = FALSE) == x
#'
#'   plot(tf_combine(x2, x1, strict = FALSE))
#'   points(x1, col = "blue", pch = "x")
#'   points(x2, col = "red", pch = "o")
tf_combine <- function(..., strict = FALSE) {
  tfs <- list(...)
  map(tfs, assert_tf)
  sizes <- map_int(tfs, vec_size)
  if (!all(duplicated(sizes)[-1])) {
    cli::cli_abort("can't {.fun tf_combine} objects of different sizes")
  }
  size <- sizes[1]

  if (strict) {
    # assert arg ranges in tfs at each vector position are strictly ordered
    args <- map(tfs, \(x) tf_arg(x) |> ensure_list())
    irreg <- lengths(args) != 1
    if (any(irreg)) {
      args <- map_if(args, !irreg, \(x) replicate(size, x))
    }

    arg_mins <- do.call(cbind, map_depth(args, pluck_depth(args) - 1, min))
    arg_maxs <- do.call(cbind, map_depth(args, pluck_depth(args) - 1, max))

    min_overlap <- apply(arg_mins, 1, \(x) is.unsorted(as.numeric(x)))
    max_overlap <- apply(arg_maxs, 1, \(x) is.unsorted(as.numeric(x)))
    if (any(min_overlap) || any(max_overlap)) {
      cli::cli_abort(
        "{.fun tf_arg}-ranges of input data are not strictly ordered."
      )
    }
  }

  domains <- do.call(rbind, map(tfs, tf_domain))
  new_domain <- c(min(domains[, 1]), max(domains[, 2]))
  tfs <- map(tfs, function(x) {
    suppressWarnings(tf_domain(x) <- new_domain)
    x
  })
  # TODO: add check for names and reuse for output if identical?  map(tfs, names) |> reduce(identical)
  tfs_data <- do.call(
    rbind,
    map(tfs, \(x) {
      names(x) <- seq_along(x)
      tf_2_df(x)
    })
  )

  duplicates <- which(duplicated(tfs_data[, -3])) #check for multiple values at some id&arg
  if (length(duplicates)) {
    if (strict) {
      cli::cli_abort(
        "can't combine functions with multiple values at same argument."
      )
    }
    cli::cli_alert_warning(
      "removing {length(duplicates)} duplicated points from input data."
    )
    tfs_data <- tfs_data[-duplicates, ]
  }

  tfd(tfs_data, domain = new_domain)
}
