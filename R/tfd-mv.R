#' @include tfd-class.R tfb-class.R
NULL

# Low-level constructor shared by tfd_mv and tfb_mv ----------------------------

# `components`: a (named) list of `d` *univariate* tf vectors, all of the same
# length `n`, the same kind (all tfd or all tfb) and the same domain. The
# multivariate object is a vctrs vector of length `n` (the number of curves)
# that simply bundles these `d` component-functions; (almost) all methods
# delegate to the univariate machinery by mapping over the components. See
# `design/multivariate.md` for the rationale.
new_tf_mv <- function(
  components = list(),
  domain = NULL,
  class = NULL,
  check_curve_names = TRUE,
  mfpc = NULL
) {
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
    if (is.null(domain)) {
      # union of component domains: a tf_mv lives on a single time axis, but
      # individual components may have been observed only on a subset of it.
      lows <- map_dbl(domains, 1)
      highs <- map_dbl(domains, 2)
      domain <- c(min(lows), max(highs))
    } else {
      assert_numeric(domain, len = 2, finite = TRUE, sorted = TRUE)
      # supplied domain must contain every component's domain
      for (cd in domains) {
        if (cd[1] < domain[1] || cd[2] > domain[2]) {
          cli::cli_abort(
            "Component domain {.val {cd}} not contained in supplied {.arg domain} {.val {domain}}."
          )
        }
      }
    }
    # widen each component's domain to the shared mv domain so all components
    # agree on the time axis. (`tf_domain<-` warns about changing the domain;
    # in this context the widening is intended, so we silence it.)
    components <- map(components, function(comp) {
      if (!isTRUE(all.equal(tf_domain(comp), domain))) {
        suppressWarnings(tf_domain(comp) <- domain)
      }
      comp
    })
    curve_names <- map(components, names)
    has_curve_names <- map_lgl(curve_names, Negate(is.null))
    if (any(has_curve_names) && !all(has_curve_names)) {
      cli::cli_abort(
        "All components must either be unnamed or have identical curve names."
      )
    }
    if (all(has_curve_names)) {
      first_names <- curve_names[[1]]
      if (!all(map_lgl(curve_names[-1], identical, y = first_names))) {
        if (check_curve_names) {
          cli::cli_abort("All components must have identical curve names.")
        }
        curve_names <- NULL
      } else {
        curve_names <- first_names
      }
    } else {
      curve_names <- NULL
    }
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
    curve_names <- NULL
  }
  data <- seq_len(n)
  names(data) <- curve_names
  new_vctr(
    data,
    components = components,
    comp_names = names(components),
    domain = domain,
    mfpc = mfpc,
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
#' trajectories \eqn{(x(t), y(t))} or other multivariate-output curves.
#'
#' A `tfd_mv` object of length `n` bundles `d` *univariate* [tfd()] vectors
#' (one per output dimension / component), each of length `n`. All numeric work
#' (evaluation, arithmetic, smoothing, ...) is delegated to these components, so
#' regular and irregular sampling, the choice of `evaluator`, etc. all behave
#' exactly as in the univariate case -- and components may even live on
#' different argument grids. Use [tfb_mv()] for a basis representation.
#'
#' @section Inheritance contract:
#' `tf_mv` classes inherit from `"tf"`, so any S3 generic registered on `"tf"`
#' without an explicit `.tf_mv` method is dispatched to the univariate
#' implementation -- the right thing component-wise for almost every verb in
#' the package (the `Math` / `Ops` / `Summary` group generics, `[`, `format`,
#' `print`, `plot`, `lines`, `tf_evaluate`, `tf_evaluations`, `tf_arg`,
#' `tf_domain`, `as.matrix`, `as.data.frame`, ... all have explicit `.tf_mv`
#' methods). When you need to *distinguish* univariate-only from any-`tf`
#' inside a helper, use [is_tf_1d()]: it returns `TRUE` for `tfd` / `tfb` and
#' `FALSE` for `tfd_mv` / `tfb_mv`.
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
#' # (a) from a (named) list of univariate tfd vectors -- one per component:
#' traj <- tfd_mv(list(x = tf_rgp(3), y = tf_rgp(3)))
#' traj
#' tf_ncomp(traj)
#' traj$x
#'
#' # (b) from a list of matrices (one [curve, arg] matrix per component):
#' t <- seq(0, 1, length.out = 50)
#' mx <- matrix(sin(2 * pi * outer(1:3, t)), nrow = 3)
#' my <- matrix(cos(2 * pi * outer(1:3, t)), nrow = 3)
#' tfd_mv(list(x = mx, y = my), arg = t)
#'
#' # (c) from a 3-d array with dimensions [curve, arg, component]:
#' arr <- array(c(mx, my), dim = c(3, 50, 2),
#'              dimnames = list(NULL, NULL, c("x", "y")))
#' tfd_mv(arr, arg = t)
#'
#' # (d) from a long data.frame (id, arg, one value column per component):
#' df <- data.frame(
#'   id = rep(1:3, each = 50),
#'   arg = rep(t, times = 3),
#'   x = as.vector(t(mx)),
#'   y = as.vector(t(my))
#' )
#' tfd_mv(df, id = "id", arg = "arg", value = c("x", "y"))
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
  slices <- map(seq_len(d[3]), \(k) data[,, k, drop = TRUE]) |>
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
        data[, c(
          if (is.character(id)) id else names(data)[id],
          if (is.character(arg)) arg else names(data)[arg],
          if (is.character(v)) v else names(data)[v]
        )],
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
    rlang::inject(tfd(
      comp,
      arg = arg,
      domain = domain,
      evaluator = !!evaluator,
      ...
    ))
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
