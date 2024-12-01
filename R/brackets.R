#' Accessing, evaluating, subsetting and subassigning `tf` vectors
#'
#' These functions access, subset, replace and evaluate `tf` objects.
#' For more information on creating `tf` objects and converting them to/from
#' `list`, `data.frame` or `matrix`, see [tfd()] and [tfb()]. See Details.\cr
#'
#' Note that these break certain (terrible) R conventions for vector-like objects:\cr
#'
#' - no argument recycling,
#' - no indexing with `NA`,
#' - no indexing with names not present in `x`,
#' - no indexing with integers `> length(x)`
#'
#' All of the above will trigger errors.
#'
#' @param x an `tf`
#' @param i index of the observations (`integer`ish, `character` or `logical`,
#'   usual R rules apply)
#' @param j The `arg` used to evaluate the functions. A (list of) `numeric`
#'   vectors. *NOT* interpreted as a column number but as the argument value of
#'   the respective functional datum.
#' @param interpolate should functions be evaluated (i.e., inter-/extrapolated)
#'   for values in `arg` for which no original data is available? Only relevant for
#'   the raw data class `tfd`, for which it defaults to `TRUE`. Basis-represented
#'   `tfb` are always "interpolated".
#' @param matrix should the result be returned as a `matrix` or as a list of
#'   `data.frame`s? If `TRUE`, `j` has to be a (list of a) single vector of
#'   `arg`. See return value.
#' @returns If `j` is missing, a subset of the functions in `x` as given by
#'   `i`.\cr If `j` is given and `matrix == TRUE`, a numeric matrix of function
#'   evaluations in which each row represents one function and each column
#'   represents one `argval` as given in argument `j`, with an attribute
#'   `arg`=`j` and row- and column-names derived from `x[i]` and `j`.\cr If
#'   `j` is given and `matrix == FALSE`, a list of `tbl_df`s with columns
#'   `arg` = `j` and `value` = evaluations at `j` for each observation in
#'   `i`.
#'
#' @rdname tfbrackets
#' @name tfbrackets
#' @export
#' @aliases tfbrackets
#' @family tidyfun bracket-operator
#' @examples
#' x <- 1:3 * tfd(data = 0:10, arg = 0:10)
#' plot(x)
#' # this operator's 2nd argument is quite overloaded -- you can:
#' # 1. simply extract elements from the vector if no second arg is given:
#' x[1]
#' x[c(TRUE, FALSE, FALSE)]
#' x[-(2:3)]
#' # 2. use the second argument and optional additional arguments to
#' #    extract specific function evaluations in a number of formats:
#' x[1:2, c(4.5, 9)] # returns a matrix of function evaluations
#' x[1:2, c(4.5, 9), interpolate = FALSE] # NA for arg-values not in the original data
#' x[-3, seq(1, 9, by = 2), matrix = FALSE] # list of data.frames for each function
#' # in order to evaluate a set of observed functions on a new grid and
#' # save them as a functional data vector again, use `tfd` or `tfb` instead:
#' tfd(x, arg = seq(0, 10, by = 0.01))
`[.tf` <- function(x, i, j, interpolate = TRUE, matrix = TRUE) {
  if (!interpolate && is_tfb(x)) {
    interpolate <- TRUE
    cli::cli_inform("{.arg interpolate} ignored for data in basis representation.")
  }
  # handle i
  if (missing(i)) {
    i <- seq_along(x)
  } else {
    i <- vec_as_location(i, n = vec_size(x), names = names(x), missing = "error")
  }
  x <- vec_slice(x, i)
  if (missing(j)) {
    return(x)
  }

  # handle j
  if (matrix && is.list(j)) {
    cli::cli_abort("Need a single vector-valued {.arg j} if {.code matrix = TRUE}.")
  }
  j <- ensure_list(j)
  if (!(length(j) %in% c(1, length(i)))) {
    cli::cli_abort("Wrong length for {.arg j}.")
  }
  evals <- tf_evaluate(x, arg = j)
  if (!interpolate) {
    new_j <- map2(j, ensure_list(tf_arg(x)), \(x, y) !(x %in% y))
    if (any(unlist(new_j, use.names = FALSE))) {
      cli::cli_warn(
        "{.code interpolate = FALSE} & no evaluations for some {.arg j}: {.code NA}s created."
      )
    }
    evals <- map2(evals, new_j, \(x, y) ifelse(y, NA, x))
  }


  if (matrix) {
    ret <- do.call(rbind, evals)
    j <- unlist(j, use.names = FALSE)
    colnames(ret) <- j
    rownames(ret) <- names(x)
    return(structure(ret, arg = j))
  }

  map2(
    j, evals, \(x, y) data_frame(arg = x, value = y, .name_repair = "minimal")
  ) |> setNames(names(x))
}

#' @param value `tf` object for subassignment. This is typed more strictly
#' than concatenation:  subassignment only happens if the common type of
#' `value` and `x` is the same as the type of `x`,
#' so subassignment never changes the type of `x` but may do a
#' potentially lossy cast of `value` to the type of `x` (with a warning).
#'@rdname tfbrackets
#' @family tidyfun bracket-operator
#' @export
`[<-.tf` <- function(x, i, value) {
  if (missing(i)) {
    i <- seq_along(x)
  }
  cast_to <- vec_ptype2(value, x) |> suppressWarnings()
  # never change type of x in subassignment
  if (!identical(vec_ptype(x), cast_to)) {
    stop_incompatible_type(x = x, y = value, x_arg = "", y_arg = "")
  }
  needs_cast <- !identical(vec_ptype(value), cast_to, ignore.environment = FALSE)
  if (needs_cast) {
    value <- vec_cast(value, vec_ptype2(value, x)) |> allow_lossy_cast()
  }
  vec_slice(x, i) <- value
  x
}
