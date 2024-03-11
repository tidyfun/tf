#' @import purrr
new_tfd <- function(arg = NULL, datalist = NULL, regular = TRUE,
                    domain = NULL, evaluator) {
  # FIXME: names weirdness- tfd  objects will ALWAYS be named if they were
  # created from an (intermediate) data.frame, but may be unnamed for different
  # provenance....
  if (vctrs::vec_size(datalist) == 0 || all(is.na(unlist(datalist)))) {
    arg <- arg %||% list(numeric())
    domain <- domain %||% numeric(2)
    subclass <- ifelse(regular, "tfd_reg", "tfd_irreg")
    datalist <- list()
    # message("empty or missing input `data`; returning prototype of length 0")
    ret <- vctrs::new_vctr(
      datalist,
      arg = arg,
      domain = domain,
      evaluator = get(evaluator, mode = "function", envir = parent.frame()),
      evaluator_name = evaluator,
      class = c(subclass, "tfd", "tf")
    )
    return(ret)
  }

  assert_string(evaluator)
  evaluator_f <- get(evaluator, mode = "function", envir = parent.frame())
  assert_function(evaluator_f)
  assert_set_equal(
    names(formals(evaluator_f)),
    c("x", "arg", "evaluations")
  )

  # sort args and values by arg:
  arg_o <- map(arg, order)
  arg <- map2(arg, arg_o, \(x, y) x[y])
  datalist <- map2(datalist, arg_o, \(x, y) unname(x[y]))

  domain <- domain %||% range(arg, na.rm = TRUE)
  assert_numeric(domain,
    finite = TRUE, any.missing = FALSE,
    sorted = TRUE, len = 2, unique = TRUE
  )
  stopifnot(
    domain[1] <= min(unlist(arg)),
    domain[2] >= max(unlist(arg))
  )

  if (!regular) {
    datalist <- map2(
      datalist, arg,
      \(x, y) {
        this_arg <- unname(y[!is.na(x)])
        list(arg = this_arg, value = unname(x[!is.na(x)]))
      }
    )
    n_evals <- map(datalist, \(x) length(x$value))
    if (any(n_evals == 0)) warning("NA entries created.", call. = FALSE)
    datalist <- map_if(
      datalist, n_evals == 0, \(x) list(arg = domain[1], value = NA)
    )
    arg <- numeric(0)
    class <- "tfd_irreg"
  } else {
    arg <- list(arg[[1]])
    class <- "tfd_reg"
  }

  if (!is.null(names(datalist))) {
    # ensure "unique" names
    names(datalist) <- vctrs::vec_as_names(names(datalist), repair = "unique")
  }

  ret <- vctrs::new_vctr(
    datalist,
    arg = arg,
    domain = domain,
    evaluator = evaluator_f,
    evaluator_name = evaluator,
    class = c(class, "tfd", "tf")
  )
  assert_arg(tf_arg(ret), ret)
  ret
}

#------------------------------------------------------------------------------

#' Constructors for vectors of "raw" functional data
#'
#' Various constructor methods for `tfd`-objects.
#'
#' **`evaluator`**: must be the (quoted or bare) name of a
#' function with signature `function(x, arg, evaluations)` that returns
#' the functions' (approximated/interpolated) values at locations `x` based on
#' the function `evaluations` available at locations `arg`.\cr
#' Available `evaluator`-functions:
#' - `tf_approx_linear` for linear interpolation without extrapolation (i.e.,
#' [zoo::na.approx()] with `na.rm = FALSE`)  -- this is the default,
#' - `tf_approx_spline` for cubic spline interpolation, (i.e., [zoo::na.spline()]
#' with `na.rm = FALSE`),
#' - `tf_approx_fill_extend` for linear interpolation and constant extrapolation
#' (i.e., [zoo::na.fill()] with `fill = "extend"`)
#' - `tf_approx_locf` for "last observation carried forward"  (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE` and
#' - `tf_approx_nocb` for "next observation carried backward" (i.e.,
#' [zoo::na.locf()] with `na.rm = FALSE, fromLast = TRUE`).
#' See `tf:::zoo_wrapper` and `tf:::tf_approx_linear`, which is simply
#' `zoo_wrapper(zoo::na.tf_approx, na.rm = FALSE)`, for examples of
#' implementations of this.
#'
#' @param data a `matrix`, `data.frame` or `list` of suitable shape, or another
#'   `tf`-object. when this argument is `NULL` (i.e. when calling `tfd()`) this
#'   returns a prototype of class `tfd`
#' @param ... not used in `tfd`, except for `tfd.tf` -- specify `arg` and
#'   `ìnterpolate = TRUE` to turn an irregular `tfd` into a regular one, see
#'   examples.
#' @returns an `tfd`-object (or a `data.frame`/`matrix` for the conversion
#'   functions, obviously.)
#' @family tfd-class
#' @export
tfd <- function(data, ...) UseMethod("tfd")

#' @export
#' @rdname tfd
#' @description `tfd.matrix` accepts a numeric matrix with one function per
#'   *row* (!). If `arg` is not provided, it tries to guess `arg` from the
#'   column names and falls back on `1:ncol(data)` if that fails.
#' @param arg `numeric`, or list of `numeric`s. The evaluation grid.
#'   For the `data.frame`-method: the
#'   name/number of the column defining the evaluation grid. The `matrix` method
#'   will try to guess suitable `arg`-values from the column names of `data` if
#'   `arg` is not supplied. Other methods fall back on integer sequences
#'   (`1:<length of data>`) as the default if not provided.
#' @param domain range of the `arg`.
#' @param evaluator a function accepting arguments `x, arg, evaluations`. See
#'   details for [tfd()].
#' @importFrom rlang quo_name enexpr
tfd.matrix <- function(data, arg = NULL, domain = NULL,
                       evaluator = tf_approx_linear, ...) {
  assert_numeric(data)
  evaluator <- quo_name(enexpr(evaluator))
  arg <- find_arg(data, arg) # either arg or numeric colnames or 1:ncol
  id <- unique_id(rownames(data) %||% seq_len(dim(data)[1]))
  # make factor conversion explicit to avoid reordering
  datalist <- split(data, factor(id, unique(as.character(id))))
  names(datalist) <- rownames(data)
  regular <- !anyNA(data)
  new_tfd(arg, datalist, regular, domain, evaluator)
}

#' @rdname tfd
#' @export
tfd.numeric <- function(data, arg = NULL,
                        domain = NULL, evaluator = tf_approx_linear, ...) {
  evaluator <- quo_name(enexpr(evaluator))
  data <- t(as.matrix(data))
  # dispatch to matrix method
  args <- list(data,
    arg = arg, domain = domain,
    evaluator = evaluator
  )
  return(do.call(tfd, args))
}

#' @description `tfd.data.frame` uses the first 3 columns of \code{data} for
#'   function information by default: (`id`, `arg`, `value`)
#' @export
#' @rdname tfd
#' @param id The name or number of the column defining which data belong to
#'   which function.
#' @param value The name or number of the column containing the function
#'   evaluations.
tfd.data.frame <- function(data, id = 1, arg = 2, value = 3, domain = NULL,
                           evaluator = tf_approx_linear, ...) {
  stopifnot(
    is.numeric(data[[arg]]),
    is.numeric(data[[value]])
  )
  evaluator <- quo_name(enexpr(evaluator))
  data <- na.omit(data[, c(id, arg, value)])

  # make factor conversion explicit to avoid reordering
  id <- factor(data[[1]], levels = as.factor(unique(data[[1]])))
  datalist <- split(data[[3]], id)
  arg <- split(data[[2]], id)
  regular <- length(arg) == 1 | all(duplicated(arg)[-1])
  new_tfd(arg, datalist, regular, domain, evaluator)
}

# TODO this will break for multivariate data!
#' @description `tfd.list` accepts a list of vectors of identical lengths
#' containing evaluations or a list of 2-column matrices/data.frames with
#' `arg` in the first and evaluations in the second column
#' @export
#' @rdname tfd
tfd.list <- function(data, arg = NULL, domain = NULL,
                     evaluator = tf_approx_linear, ...) {
  evaluator <- quo_name(enexpr(evaluator))
  vectors <- map_lgl(data, \(x) is.numeric(x) & !is.array(x))
  if (all(vectors)) {
    where_na <- map(data, is.na)
    data <- map2(data, where_na, \(x, y) x[!y])
    lens <- lengths(data)
    regular <- all(lens == lens[1]) &
      (is.numeric(arg) || all(duplicated(arg)[-1])) # duplicated(NULL) == TRUE!
    if (!regular) {
      stopifnot(
        !is.null(arg), length(arg) == length(data),
        all(lengths(arg) == lengths(where_na))
      )
      arg <- map2(arg, where_na, \(x, y) x[!y])
    } else {
      if (is.null(arg)) {
        warning("No argument values supplied, using index positions.", call. = FALSE)
        arg <- map(data, seq_along)
      }
      arg <- ensure_list(arg)
      assert_numeric(arg[[1]],
        finite = TRUE, any.missing = FALSE,
        sorted = TRUE
      )
    }
  }
  if (!any(vectors)) {
    dims <- map(data, dim)
    stopifnot(
      all(lengths(dims) == 2), all(map_int(dims, 2) == 2),
      all(rapply(data, is.numeric))
    )
    arg <- map(data, \(x) unlist(x[, 1]))
    data <- map(data, \(x) unlist(x[, 2]))
    regular <- (length(data) == 1 | all(duplicated(arg)[-1]))
  }
  new_tfd(arg, data,
    regular = regular, domain = domain,
    evaluator = evaluator
  )
}

#' @export
#' @examples
#' # turn irregular to regular tfd by evaluating on a common grid:
#'
#' f <- c(
#'   tf_rgp(1, arg = seq(0, 1, length.out = 11)),
#'   tf_rgp(1, arg = seq(0, 1, length.out = 21))
#' )
#' tfd(f, arg = seq(0, 1, length.out = 21))
#'
#' set.seed(1213)
#' f <- tf_rgp(3, arg = seq(0, 1, length.out = 51)) |> tf_sparsify(0.9)
#' # does not yield regular data because linear extrapolation yields NAs
#' #   outside observed range:
#' tfd(f, arg = seq(0, 1, length.out = 101))
#' # this "works" (but may not yield sensible values..!!) for
#' #   e.g. constant extrapolation:
#' tfd(f, evaluator = tf_approx_fill_extend, arg = seq(0, 1, length.out = 101))
#' plot(f, col = 2)
#' tfd(f,
#'   arg = seq(0, 1, length.out = 151), evaluator = tf_approx_fill_extend
#' ) |> lines()
#' @rdname tfd
tfd.tf <- function(data, arg = NULL, domain = NULL,
                   evaluator = NULL, ...) {
  evaluator_name <- enexpr(evaluator)
  evaluator <- if (is_tfd(data) && is.null(evaluator)) {
    attr(data, "evaluator_name")
  } else {
    if (is.null(evaluator)) "tf_approx_linear" else quo_name(evaluator_name)
  }
  domain <- (domain %||% unlist(arg) %||% tf_domain(data)) |> range()
  re_eval <- !is.null(arg)
  arg <- ensure_list(arg %||% tf_arg(data))
  evaluations <- if (re_eval) {
    evaluator_f <- get(evaluator, mode = "function", envir = parent.frame())
    tf_evaluate(data, arg = arg, evaluator = evaluator_f)
  } else {
    tf_evaluations(data)
  }
  nas <- map(evaluations, \(x) which(is.na(x)))
  if (re_eval & any(lengths(nas))) {
    evaluations <- map2(evaluations, nas, \(x, y) if (length(y)) x[-y] else x)
    # check if all NAs occur at the same args and try to make a regular tfd if so
    na_args <- map2(arg, nas, ~.x[.y])
    if (!all(duplicated(na_args)[-1])) {
      warning(
        length(unlist(nas)), " evaluations were NA, returning irregular tfd.",
        call. = FALSE
      )
    } else {
      na_arg_string <- prettyNum(na_args[[1]]) |> paste(collapse = ", ")
      if (nchar(na_arg_string) > options()$width) {
        na_arg_string <- substr(na_arg_string, 1, options()$width - 15) |>
          paste0("[... truncated]")
      }
      warning(
        length(unlist(nas)), " evaluations on arg = (", na_arg_string,
        ") were NA, returning regular data on reduced grid.",
        call. = FALSE
      )
      nas <- nas[1]
    }
    arg <- map2(arg, nas, \(x, y) if (length(y)) x[-y] else x)
  }
  names(evaluations) <- names(data)
  new_tfd(arg, evaluations,
    regular = (length(arg) == 1),
    domain = domain, evaluator = evaluator
  )
}

#' @rdname tfd
#' @description `tfd.default` returns class prototype when argument to tfd() is
#'   NULL or not a recognised class
#' @export
tfd.default <- function(data, arg = NULL, domain = NULL,
                        evaluator = tf_approx_linear, ...) {
  message("input `data` not recognized class; returning prototype of length 0")
  datalist <- list()
  evaluator <- quo_name(enexpr(evaluator))
  new_tfd(arg = arg, datalist = datalist, domain = domain, regular = TRUE,
          evaluator = evaluator)
}

#-------------------------------------------------------------------------------

#' @rdname tfd
#' @export
as.tfd <- function(data, ...) UseMethod("as.tfd")
#' @export
as.tfd.default <- function(data, ...) {
  tfd(data, ...)
}


#' @rdname tfd
#' @export
as.tfd_irreg <- function(data, ...) UseMethod("as.tfd_irreg")
#' @import purrr
#' @export
as.tfd_irreg.tfd_reg <- function(data, ...) {
  arg <- ensure_list(tf_arg(data))
  ret <- map2(tf_evaluations(data), arg, \(x, y) list(arg = y, value = x))
  attributes(ret) <- attributes(data)
  attr(ret, "arg") <- numeric(0)
  class(ret)[1] <- "tfd_irreg"
  ret
}
#' @export
as.tfd_irreg.tfd_irreg <- function(data, ...) {
  data
}
#' @export
as.tfd_irreg.tfb <- function(data, ...) {
  tfd(data) |> as.tfd_irreg()
}
