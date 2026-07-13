#' @include tfd-mv.R
NULL

#' Vector-valued functional data in basis representation (`f: R -> R^d`)
#'
#' `tfb_mv` is the basis-representation analogue of [tfd_mv()]: it bundles `d`
#' univariate [tfb()] vectors (one per output dimension / component) into a
#' single vctrs vector of vector-valued functions \eqn{f: \mathbb{R} \to
#' \mathbb{R}^d}. Each component is fitted independently with the usual
#' univariate [tfb()] machinery (spline or FPC basis), so all of its arguments
#' (`k`, `bs`, `penalized`, `basis`, ...) apply per component.
#'
#' @param data a [tfd_mv()] / `tfb_mv` object, a (named) `list` of univariate
#'   `tf` vectors, or anything [tfd_mv()] accepts (it is converted to `tfd_mv`
#'   first and then each component is expanded into a basis).
#' @param basis spline (default) or fpc basis, see [tfb()].
#' @param ... forwarded to the univariate [tfb()] constructor.
#' @returns a `tfb_mv` object.
#' @family tf_mv-class
#' @examples
#' traj <- tfd_mv(list(x = tf_rgp(5), y = tf_rgp(5)))
#' tb <- tfb_mv(traj, k = 7, verbose = FALSE)
#' tb
#' tf_ncomp(tb)
#' @rdname tfb_mv
#' @export
tfb_mv <- function(data, ...) UseMethod("tfb_mv")

#' @rdname tfb_mv
#' @details
#' By default a single `...` is shared across all components (every component
#' gets the same `k`, `bs`, `sp`, etc.). To pass *different* basis arguments
#' to different components, give the argument as a list named by component
#' names -- e.g. `tfb_mv(f, k = list(x = 5, y = 12))` fits component `x` with
#' `k = 5` and component `y` with `k = 12`. Any list-valued `...` whose names
#' do **not** match the component names is treated as a shared argument value.
#' (Already-`tfb` components passed via `tfb_mv.list()` are kept as-is, which
#' is the most permissive way to mix entirely different basis kinds across
#' components.)
#' @export
tfb_mv.tf_mv <- function(data, basis = c("spline", "fpc"), ...) {
  requested_basis <- !missing(basis)
  basis <- match.arg(basis)
  dots <- list(...)
  if (!tf_ncomp(data)) {
    return(new_tf_mv(list(), domain = tf_domain(data), class = "tfb_mv"))
  }
  # shortcut only when no re-fit was requested: an explicit `basis` (or any
  # basis arguments) must trigger a re-fit, not silently return the old basis
  if (is_tfb_mv(data) && !length(dots) && !requested_basis) {
    return(data)
  }
  comp_names <- attr(data, "comp_names")
  components <- map2(tf_components(data), comp_names, function(comp, nm) {
    per_comp_dots <- distribute_dots(dots, nm, comp_names)
    if (is_tfb(comp)) {
      same_kind <- (basis == "spline" && inherits(comp, "tfb_spline")) ||
        (basis == "fpc" && inherits(comp, "tfb_fpc"))
      # the univariate tfb re-fitters assume same-kind attributes; a change
      # of basis kind must go through the raw evaluations
      if (!same_kind) comp <- tfd(comp)
    }
    do.call(tfb, c(list(comp), list(basis = basis), per_comp_dots))
  })
  new_tf_mv(components, domain = tf_domain(data))
}

# Distribute a `...` collection across one component `nm`: a `...` argument that
# is a list named by *all* component names is treated as per-component (return
# its `nm` entry); any other argument is shared (returned as-is). Used by both
# tfb_mv() and tfb_mfpc() to allow e.g. `k = list(x = 5, y = 12)`.
distribute_dots <- function(dots, nm, comp_names) {
  map(dots, function(arg) {
    if (
      is.list(arg) &&
        !is.null(names(arg)) &&
        length(arg) == length(comp_names) &&
        !anyDuplicated(names(arg)) &&
        setequal(names(arg), comp_names)
    ) {
      arg[[nm]]
    } else {
      arg
    }
  })
}

#' @rdname tfb_mv
#' @export
tfb_mv.list <- function(
  data,
  basis = c("spline", "fpc"),
  arg = NULL,
  domain = NULL,
  ...
) {
  basis <- match.arg(basis)
  if (!length(data)) {
    return(new_tf_mv(list(), class = "tfb_mv"))
  }
  if (all(map_lgl(data, is_tf))) {
    mv <- new_tf_mv(data, domain = domain)
    if (all(map_lgl(data, is_tfb)) && !length(list(...))) {
      return(mv)
    }
    return(tfb_mv(mv, basis = basis, ...))
  }
  # partition the dots: constructor arguments (arg, domain) go to tfd_mv()
  # only, everything else are basis arguments for the tfb re-fit only --
  # forwarding all of `...` to both leaks basis arguments into the tfd
  # constructor and re-grids the basis fit on `arg`
  tfb_mv(tfd_mv(data, arg = arg, domain = domain), basis = basis, ...)
}

#' @rdname tfb_mv
#' @export
tfb_mv.default <- function(
  data,
  basis = c("spline", "fpc"),
  arg = NULL,
  domain = NULL,
  ...
) {
  if (missing(data) || vec_size(data) == 0) {
    return(new_tf_mv(list(), class = "tfb_mv"))
  }
  tfb_mv(
    tfd_mv(data, arg = arg, domain = domain),
    basis = match.arg(basis),
    ...
  )
}

#------------------------------------------------------------------------------

#' @rdname tfb_mv
#' @export
as.tfb_mv <- function(data, ...) UseMethod("as.tfb_mv")

#' @rdname tfb_mv
#' @export
as.tfb_mv.default <- function(data, ...) tfb_mv(data, ...)
