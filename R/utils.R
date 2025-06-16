#-------------------------------------------------------------------------------
# arg-related

find_arg <- function(data, arg) {
  if (is.null(arg)) {
    names <- dimnames(data)[[2]]
    arg <- suppressWarnings(as.numeric(names))
    if (is.null(arg) || anyNA(arg)) {
      # extract number-strings
      # will interpret separating-dashes as minus-signs, so functions may run
      # backwards.
      # regex adt'd from https://www.regular-expressions.info/floatingpoint.html
      arg_matches <- regexpr(
        pattern = "[-+]?(0|(0\\.[0-9]+)|([1-9][0-9]*\\.?[0-9]*))([eE][-+]?[0-9]+)?$", # nolint
        names
      )
      arg <- regmatches(names, arg_matches)
      arg <- suppressWarnings(as.numeric(arg))
      if (n_distinct(arg) != ncol(data)) arg <- NULL
    }
    if (is.null(arg) || anyNA(arg)) {
      cli::cli_inform(
        "Column names not suitable as {.arg arg} values. Using {.code 1:{length(ncol(data))}} instead."
      )
      arg <- numeric(0)
    }
  }
  if (length(arg) == 0) arg <- seq_len(ncol(data))
  assert_numeric(arg, any.missing = FALSE)
  if (length(arg) != ncol(data)) {
    cli::cli_abort("{.arg arg} must have same length as {.arg data}.")
  }
  list(arg)
}

domains_overlap <- function(x, y) {
  dom_x <- tf_domain(x)
  dom_y <- tf_domain(y)
  (dom_x[1] %inr% dom_y || dom_x[2] %inr% dom_y) ||
    (dom_y[1] %inr% dom_x || dom_y[2] %inr% dom_x)
}

# default resolution is ~ smallest observed interval/10
# rounded down to the nearest decimal
get_resolution <- function(arg) {
  .min_diff <- function(x) {
    suppressWarnings(ifelse(length(x) - 1, min(abs(diff(x))), NA))
  }
  min_diff <- map_dbl(
    ensure_list(arg),
    possibly(.f = .min_diff, otherwise = NA)
  ) |>
    min(na.rm = TRUE) |>
    suppressWarnings()
  if (min_diff < .Machine$double.eps * 10) {
    cli::cli_abort("(Almost) non-unique {.arg arg} values detected.")
  }
  10^(floor(log10(min_diff)) - 1)
}

# "quantize" the values in arg to the given resolution
# always rounding up/down for positive/negative `updown`
round_resolution <- function(arg, resolution, updown = 0) {
  if (updown == 0) {
    round(arg / resolution) * resolution
  } else if (updown < 0) {
    floor(arg / resolution) * resolution
  } else {
    ceiling(arg / resolution) * resolution
  }
}

# grids are "equidistant" if their distances deviate less
# than tol * resolution (by default: by less than 10%)
is_equidist <- function(f, tol = 1) {
  if (is_irreg(f)) {
    return(FALSE)
  }
  arg <- tf_arg(f)
  f_resolution <- get_resolution(arg) * tol
  equidist <- map_lgl(
    ensure_list(arg),
    function(x) {
      deviate <- diff(x) |> diff() |> abs() |> max()
      deviate < f_resolution
    }
  )
  all(equidist)
}

# get intersection of arg vectors
common_args <- function(e1, e2) {
  arg1 <- tf_arg(e1) |> ensure_list()
  arg2 <- tf_arg(e2) |> ensure_list()
  map2(arg1, arg2, \(x, y) intersect(x, y) |> sort())
}

# get union of arg vectors
all_args <- function(e1, e2) {
  arg1 <- tf_arg(e1) |> ensure_list()
  arg2 <- tf_arg(e2) |> ensure_list()
  map2(arg1, arg2, \(x, y) union(x, y) |> sort())
}


#-------------------------------------------------------------------------------
#  compatibility

compare_tf_attribs <- function(
  e1,
  e2,
  ignore = c("names", "id"),
  check_attrib = TRUE
) {
  # TODO: better way to check evaluator/basis functions?
  a1 <- attributes(e1)
  a2 <- attributes(e2)
  attribs <- union(names(a1), names(a2))
  if (length(ignore)) attribs <- attribs[!(attribs %in% ignore)]
  .compare <- function(a, b) {
    if (is.null(a) != is.null(b)) {
      return(FALSE)
    }
    suppressWarnings(
      if (is.function(a)) {
        # FIXME: this is not reliable/useful but prob. impossible to solve
        # generally: would need to know which (functional) objects in the
        # enclosure of these functions are relevant for comparison -- comparing
        # all is too strict but comparing none is rather dangerous. Right now
        # the function bodies all look the same since they share a common
        # wrapper.... Fingers crossed relevant differences get picked up by
        # differences in the label or basis attributes...
        identical(a, b, ignore.environment = TRUE)
      } else if (is.list(a)) {
        all(map2_lgl(a, ensure_list(b), .compare))
      } else {
        isTRUE(all.equal(a, b, check.attributes = check_attrib))
      }
    )
  }
  ret <- map_lgl(attribs, \(x) .compare(a1[[x]], a2[[x]])) |> setNames(attribs)
  ret
}

same_basis <- function(x, y) {
  if (!(is_tfb(x) && is_tfb(y))) {
    return(FALSE)
  }
  all.equal(
    attr(x, "basis_matrix"),
    attr(y, "basis_matrix"),
    check.attributes = FALSE
  ) |>
    isTRUE()
}

#-------------------------------------------------------------------------------
# misc

#' Find out if values are inside given bounds
#'
#' `in_range` and its infix-equivalent `%inr%` return `TRUE` for all values in
#'  the numeric vector `f` that are within the range of values in `r`.
#'
#' @param f a numeric vector.
#' @param r numeric vector used to specify a range, only the minimum and maximum
#'   of `r` are used.
#' @returns a `logical` vector of the same length as `f`.
#' @family tidyfun utility functions
#' @export
in_range <- function(f, r) {
  assert_numeric(f)
  assert_numeric(r)
  r <- range(r, na.rm = TRUE)
  f >= r[1] & f <= r[2]
}

#' @rdname in_range
#' @export
`%inr%` <- function(f, r) in_range(f, r)

get_args <- function(args, f) {
  args[names(args) %in% formalArgs(f)]
}

#' Turns any object into a list
#'
#' See above.
#' @param x any input.
#' @returns `x` turned into a list.
#' @export
#' @family tidyfun developer tools
ensure_list <- function(x) {
  if (is.list(x)) x else list(x)
}

#' Make syntactically valid unique names
#'
#' See above.
#' @param x any input.
#' @returns `x` turned into a list.
#' @export
#' @family tidyfun developer tools
# export for tidyfun...
unique_id <- function(x) {
  if (anyDuplicated(x) == 0) {
    return(x)
  }
  if (is.character(x)) x <- sub("$^", "NA", x)
  x <- make.names(as.character(x), unique = TRUE)
  x
}

na_to_0 <- function(x) replace(x, is.na(x), 0)

n_distinct <- function(x) length(unique(x))

sort_unique <- function(x, simplify = FALSE) {
  if (simplify) {
    x <- unlist(x, use.names = FALSE)
  }
  sort(unique(x))
}

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

# FS: better/more general to supply "arg" vector instead (and see below for default)?
fd_to_matrix <- function(fd, n_points = 100) {
  assert_class(fd, "fd")
  assert_integer(n_points, lower = 1)
  rng <- fd$basis$rangeval
  arg <- seq(rng[1], rng[2], length = n_points) #FS: shouldn't this use fd$fdnames$time?
  t(fda::eval.fd(arg, fd))
}

# Source: <https://github.com/mlr-org/mlr3misc/blob/main/R/format_bib.R>
# by Michel Lang (copied here Feb 2024)
format_bib <- function(..., bibentries = NULL, envir = parent.frame()) {
  if (is.null(bibentries)) {
    bibentries <- get("bibentries", envir = envir)
  }
  assert_list(bibentries, "bibentry", names = "unique")
  str <- map_chr(list(...), \(entry) tools::toRd(bibentries[[entry]]))
  paste0(str, collapse = "\n\n")
}
