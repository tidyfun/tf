#-------------------------------------------------------------------------------
# arg-related

find_arg <- function(data, arg) {
  if (is.null(arg)) {
    names <- dimnames(data)[[2]]
    suppressWarnings(arg <- as.numeric(names))
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
      suppressWarnings(arg <- as.numeric(arg))
      if (length(unique(arg)) != dim(data)[2]) arg <- NULL
    }
    if (is.null(arg) || anyNA(arg)) {
      message(
        "Column names not suitable as 'arg'-values. Using 1:ncol(data).",
        call. = FALSE
      )
      arg <- numeric(0)
    }
  }
  if (!length(arg)) arg <- seq_len(dim(data)[2])
  stopifnot(length(arg) == dim(data)[2], is.numeric(arg), all(!is.na(arg)))
  list(arg)
}

#' @import checkmate
assert_arg <- function(arg, x, check_unique = TRUE) {
  if (is.list(arg)) {
    assert_true(length(arg) %in% c(1, length(x)))
    walk(arg, \(arg) assert_arg_vector(arg, x = x, check_unique = check_unique))
  } else {
    assert_arg_vector(arg, x, check_unique = check_unique)
  }
}

.assert_arg_vector <- function(arg, domain_x, check_unique) {
  if (check_unique && (anyDuplicated(arg) > 0) ) {
      stop("Non-unique arg-values.", call. = FALSE)
  }
  assert_numeric(arg,
    any.missing = FALSE, unique = FALSE, sorted = TRUE,
    lower = domain_x[1], upper = domain_x[2]
  )
}

assert_arg_vector <- function(arg, x, check_unique = TRUE) {
  domain_x <- tf_domain(x)
  .assert_arg_vector(arg, domain_x, check_unique)
}

# default resolution is ~ smallest observed interval/10
# rounded down to the nearest decimal
get_resolution <- function(arg) {
  .min_diff <- function(x) {
    suppressWarnings(ifelse(length(x) - 1, min(diff(x)), x[1]))
  }
  min_diff <- map_dbl(ensure_list(arg),
                      possibly(.f = .min_diff, otherwise = NA)) |>
    min(na.rm = TRUE)
  if (min_diff < .Machine$double.eps * 10) {
    stop("(Almost) non-unique arg values detected.", call. = FALSE)
  }
  10^(floor(log10(min_diff)) - 1)
}

adjust_resolution <- function(arg, f, unique = TRUE) {
  resolution <- resolution(f)
  .adjust_resolution(arg, resolution, unique = unique)
}

.adjust_resolution <- function(arg, resolution, unique = TRUE) {
  u <- if (unique) base::unique else function(x) x
  if (is.list(arg)) {
    map(arg, \(x) u(round_resolution(x, resolution)))
  } else {
    u(round_resolution(arg, resolution))
  }
}

# "quantize" the values in arg to the given resolution
round_resolution <- function(arg, resolution, updown = 0) {
  if (updown == 0) {
    round(arg / resolution) * resolution
  } else if (updown < 0) {
    floor(arg / resolution) * resolution
  } else {
    ceiling(arg / resolution) * resolution
  }
}

is_equidist <- function(f) {
  if (is_irreg(f)) {
    return(FALSE)
  }
  unique_diffs <- map_lgl(
    ensure_list(tf_arg(f)),
    \(x) {
      round_resolution(x, attr(f, "resolution")) |>
        diff() |>
        duplicated() |>
        tail(-1) |>
        all()
    }
  )
  all(unique_diffs)
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

compare_tf_attribs <- function(e1, e2,
                               ignore = c("names", "id"), check_attrib = TRUE) {
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
      } else {
        if (is.list(a)) {
          all(map2_lgl(a, ensure_list(b), .compare))
        } else {
          isTRUE(all.equal(a, b, check.attributes = check_attrib))
        }
      }
    )
  }
  ret <- map_lgl(attribs, \(x) .compare(a1[[x]], a2[[x]])) |> setNames(attribs)
  ret
}

#-------------------------------------------------------------------------------
# misc

#' Find out if values are inside given bounds
#'
#' `in_range` and its infix-equivalent `%inr%` return `TRUE` for all values in
#'  the numeric vector `f` that are within the range of values in `r`.
#'
#' @param f a numeric vector
#' @param r numeric vector used to specify a range, only the minimum and maximum
#'   of `r` are used.
#' @returns a `logical` vector of the same length as `f`
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
  args[names(args) %in% names(formals(f))]
}

#' Turns any object into a list
#'
#' See above.
#' @param x any input
#' @returns `x` turned into a list.
#' @export
#' @family tidyfun developer tools
ensure_list <- function(x) {
  if (!is.list(x)) list(x) else x
}

#' Make syntactically valid unique names
#'
#' See above.
#' @param x any input
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

na_to_0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

n_distinct <- function(x) length(unique(x))

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
