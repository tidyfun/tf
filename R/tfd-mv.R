#' @include tfd-class.R tfb-class.R
NULL

# Low-level constructor shared by tfd_mv and tfb_mv ----------------------------

# `components`: a (named) list of `d` *univariate* tf vectors, all of the same
# length `n`, the same kind (all tfd or all tfb) and the same domain. The
# multivariate object is a vctrs vector of length `n` (the number of curves)
# that simply bundles these `d` component-functions; (almost) all methods
# delegate to the univariate machinery by mapping over the components. See
# `design/multivariate.md` for the rationale.
new_tf_mv <- function(components = list(), domain = NULL, class = NULL) {
  assert_list(components)
  if (length(components)) {
    if (!all(map_lgl(components, is_tf))) {
      cli::cli_abort("All components must be {.cls tf} objects.")
    }
    all_tfd <- all(map_lgl(components, is_tfd))
    all_tfb <- all(map_lgl(components, is_tfb))
    if (!(all_tfd || all_tfb)) {
      cli::cli_abort(
        "All components must be the same kind: either all {.cls tfd} or all {.cls tfb}."
      )
    }
    lens <- map_int(components, vec_size)
    if (length(unique(lens)) > 1L) {
      cli::cli_abort(
        "All components must have the same length, but lengths are {.val {lens}}."
      )
    }
    domains <- map(components, tf_domain)
    same_domain <- all(map_lgl(
      domains[-1],
      \(d) isTRUE(all.equal(d, domains[[1]]))
    ))
    if (!same_domain) {
      cli::cli_abort("All components must share the same {.arg domain}.")
    }
    domain <- domain %||% domains[[1]]
    subclass <- if (all_tfb) "tfb_mv" else "tfd_mv"
    if (!is.null(class) && !identical(class, subclass)) {
      cli::cli_abort(
        "Components are {.cls {subclass}} but {.arg class} is {.val {class}}."
      )
    }
    n <- lens[1]
    if (is.null(names(components))) {
      names(components) <- paste0("v", seq_along(components))
    }
    names(components) <- vec_as_names(names(components), repair = "unique")
  } else {
    domain <- domain %||% numeric(2)
    subclass <- class %||% "tfd_mv"
    n <- 0L
  }
  new_vctr(
    seq_len(n),
    components = components,
    comp_names = names(components),
    domain = domain,
    class = c(subclass, "tf_mv", "tf")
  )
}

# normalize the `evaluator`/`basis` argument forwarding via rlang injection so
# the univariate constructors' NSE-capture (`as_name(enexpr(evaluator))`) sees
# the original expression rather than the local variable.
build_components <- function(data, constructor, arg, domain, dots, extra) {
  # `data` is a list whose elements are matrices/data.frames/numerics
  nms <- names(data) %||% paste0("v", seq_along(data))
  components <- map(data, function(d) {
    rlang::inject(
      constructor(d, arg = arg, domain = domain, !!!extra, !!!dots)
    )
  })
  setNames(components, nms)
}

#------------------------------------------------------------------------------

#' Constructors for vector-valued functional data (`f: R -> R^d`)
#'
#' `tfd_mv` represents *vector-valued* functional data -- vectors of functions
#' \eqn{f: \mathcal{T} \subset \mathbb{R} \to \mathbb{R}^d}, such as movement
#' trajectories \eqn{(x(t), y(t))} or other multivariate-output curves (see
#' GitHub issues #18 and #27).
#'
#' A `tfd_mv` object of length `n` bundles `d` *univariate* [tfd()] vectors
#' (one per output dimension / component), each of length `n`. All numeric work
#' (evaluation, arithmetic, smoothing, ...) is delegated to these components, so
#' regular and irregular sampling, the choice of `evaluator`, etc. all behave
#' exactly as in the univariate case -- and components may even live on
#' different argument grids. Use [tfb_mv()] for a basis representation.
#'
#' @param data one of: a (named) `list` of univariate `tf` vectors (used
#'   directly, one per component); a (named) `list` of numeric matrices /
#'   data.frames (one *per component*, each turned into a [tfd()]); a 3-d
#'   numeric `array` with dimensions `[curve, arg, component]`; or a long
#'   `data.frame` with an `id` column, an `arg` column and one or more `value`
#'   columns (one component per `value` column).
#' @param arg evaluation grid, see [tfd()].
#' @param domain range of `arg`, see [tfd()].
#' @param evaluator inter-/extrapolation function, see [tfd()].
#' @param ... forwarded to the univariate [tfd()] constructor.
#' @returns a `tfd_mv` object (a vctrs vector of length `n`).
#' @seealso [tfb_mv()] for basis representation; [tf_components()],
#'   [tf_ncomp()] and the `$` operator to access components.
#' @family tf_mv-class
#' @examples
#' # a 2-d trajectory built from two univariate tfd vectors:
#' traj <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
#' traj
#' tf_ncomp(traj)
#' traj$x
#' @rdname tfd_mv
#' @export
tfd_mv <- function(data, ...) UseMethod("tfd_mv")

#' @rdname tfd_mv
#' @export
tfd_mv.list <- function(
  data,
  arg = NULL,
  domain = NULL,
  evaluator = tf_approx_linear,
  ...
) {
  if (!length(data)) {
    return(new_tf_mv(list(), domain = domain, class = "tfd_mv"))
  }
  if (all(map_lgl(data, is_tf))) {
    return(new_tf_mv(data, domain = domain))
  }
  evaluator <- enexpr(evaluator)
  components <- build_components(
    data,
    constructor = tfd,
    arg = arg,
    domain = domain,
    dots = list(...),
    extra = list(evaluator = evaluator)
  )
  new_tf_mv(components, domain = domain)
}

#' @rdname tfd_mv
#' @export
tfd_mv.array <- function(
  data,
  arg = NULL,
  domain = NULL,
  evaluator = tf_approx_linear,
  ...
) {
  d <- dim(data)
  if (length(d) != 3) {
    cli::cli_abort(
      "{.arg data} array must be 3-dimensional ([curve, arg, component])."
    )
  }
  comp_names <- dimnames(data)[[3]] %||% paste0("v", seq_len(d[3]))
  slices <- map(seq_len(d[3]), \(k) data[, , k, drop = TRUE]) |>
    setNames(comp_names)
  evaluator <- enexpr(evaluator)
  components <- build_components(
    slices,
    constructor = tfd,
    arg = arg,
    domain = domain,
    dots = list(...),
    extra = list(evaluator = evaluator)
  )
  new_tf_mv(components, domain = domain)
}

#' @rdname tfd_mv
#' @param id,value for the `data.frame` method: the column defining function
#'   `id`, the column defining the `arg` grid, and the (possibly several)
#'   columns containing component evaluations (one component per `value`
#'   column).
#' @export
tfd_mv.data.frame <- function(
  data,
  id = 1,
  arg = 2,
  value = 3,
  domain = NULL,
  evaluator = tf_approx_linear,
  ...
) {
  evaluator <- enexpr(evaluator)
  value_names <- if (is.character(value)) value else names(data)[value]
  components <- map(value, function(v) {
    rlang::inject(
      tfd(
        data[, c(if (is.character(id)) id else names(data)[id],
                 if (is.character(arg)) arg else names(data)[arg],
                 if (is.character(v)) v else names(data)[v])],
        domain = domain,
        evaluator = !!evaluator,
        ...
      )
    )
  }) |>
    setNames(value_names)
  new_tf_mv(components, domain = domain)
}

#' @rdname tfd_mv
#' @export
tfd_mv.tf_mv <- function(
  data,
  arg = NULL,
  domain = NULL,
  evaluator = NULL,
  ...
) {
  evaluator <- enexpr(evaluator)
  components <- map(tf_components(data), function(comp) {
    rlang::inject(tfd(comp, arg = arg, domain = domain, evaluator = !!evaluator, ...))
  })
  new_tf_mv(components, domain = domain)
}

#' @rdname tfd_mv
#' @export
tfd_mv.default <- function(data, arg = NULL, domain = NULL, ...) {
  if (!missing(data)) {
    cli::cli_warn(
      "Input {.arg data} not a recognized class; returning prototype of length 0."
    )
  }
  new_tf_mv(list(), domain = domain, class = "tfd_mv")
}

#------------------------------------------------------------------------------

#' @rdname tfd_mv
#' @export
as.tfd_mv <- function(data, ...) UseMethod("as.tfd_mv")

#' @rdname tfd_mv
#' @export
as.tfd_mv.default <- function(data, ...) tfd_mv(data, ...)

#' @rdname tfd_mv
#' @export
as.tfd_mv.tf_mv <- function(data, ...) {
  components <- map(tf_components(data), \(comp) as.tfd(comp, ...))
  new_tf_mv(components, domain = tf_domain(data))
}
