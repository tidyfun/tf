# Registration: one shared time-warp per curve, applied to all components ------

# univariate signal used to estimate the (joint) warp for a multivariate curve
mv_registration_signal <- function(x, ref_component = 1L) {
  assert_tf_mv(x)
  if (is.function(ref_component)) {
    return(ref_component(x))
  }
  if (identical(ref_component, "norm")) {
    return(tf_norm(x))
  }
  # otherwise a component name or index -- tf_component() validates it.
  tf_component(x, ref_component)
}

#' @export
tf_warp.tf_mv <- function(x, warp, ...) {
  map_components(x, \(comp) tf_warp(comp, warp, ...))
}

#' @export
tf_align.tf_mv <- function(x, warp, ...) {
  map_components(x, \(comp) tf_align(comp, warp, ...))
}

#' @export
tf_estimate_warps.tf_mv <- function(
  x,
  ...,
  template = NULL,
  method = c("srvf", "srvf_mv", "cc", "affine", "landmark"),
  max_iter = 3L,
  tol = 1e-2,
  ref_component = 1L
) {
  method <- match.arg(method)
  assert_count(max_iter, positive = TRUE)
  assert_number(tol, lower = 0, finite = TRUE)
  if (method == "srvf_mv") {
    return(tf_register_srvf_mv(
      x = x,
      template = template,
      max_iter = max_iter,
      ...
    ))
  }
  signal <- mv_registration_signal(x, ref_component)
  tmpl <- if (is_tf_mv(template)) {
    mv_registration_signal(template, ref_component)
  } else {
    template
  }
  warps <- tf_estimate_warps(
    signal,
    ...,
    template = tmpl,
    method = method,
    max_iter = max_iter,
    tol = tol
  )
  # drop the (univariate) template attribute so tf_register() derives a
  # multivariate template via mean() of the aligned components instead.
  attr(warps, "template") <- NULL
  warps
}

# True multivariate SRVF time registration -------------------------------------

srvf_mv_check_dots <- function(dots, arg = "method = \"srvf_mv\"") {
  forbidden <- c(
    "alignment",
    "rotation",
    "rotated",
    "scale",
    "mode",
    "isclosed",
    "maxit"
  )
  supplied <- intersect(names(dots), forbidden)
  if (length(supplied)) {
    cli::cli_abort(c(
      "{arg} keeps rotation = FALSE, scale = FALSE, and mode = \"O\".",
      "x" = "Unsupported argument{?s}: {.arg {supplied}}.",
      "i" = "Use {.fn tf_register_shape} when curves may be rotated or rescaled."
    ))
  }
}

srvf_mv_validate_regular <- function(x, arg = "x") {
  assert_tf_mv(x, .var.name = arg)
  if (!is_tfd_mv(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a regular shared-grid {.cls tfd_mv} object.",
      "i" = "Convert basis or irregular data to {.cls tfd_mv} on a common grid first."
    ))
  }
  comps <- tf_components(x)
  if (!length(comps)) {
    cli::cli_abort("{.arg {arg}} must have at least one component.")
  }
  if (!all(map_lgl(comps, is_reg))) {
    cli::cli_abort(c(
      "{.arg {arg}} must use regular component grids.",
      "i" = "Irregular {.cls tf_mv} inputs are not supported for SRVF multivariate registration yet."
    ))
  }
  grid <- tf_arg(x)
  if (!is.numeric(grid)) {
    cli::cli_abort(c(
      "{.arg {arg}} must have one shared argument grid across all components.",
      "i" = "Re-evaluate all components on the same grid before registration."
    ))
  }
  if (length(grid) < 2L) {
    cli::cli_abort("{.arg {arg}} must contain at least two argument values.")
  }
  values <- as.matrix(x)
  if (anyNA(values) || any(!is.finite(values))) {
    cli::cli_abort(
      "{.arg {arg}} must not contain missing or non-finite evaluations."
    )
  }
  invisible(x)
}

srvf_mv_validate_template <- function(template, x) {
  if (is.null(template)) {
    return(NULL)
  }
  srvf_mv_validate_regular(template, arg = "template")
  check_compatible_mv(x, template)
  if (length(template) != 1L && length(template) != length(x)) {
    cli::cli_abort(
      "{.arg template} must be of length 1 or the same length as {.arg x}."
    )
  }
  if (!all(tf_domain(x) == tf_domain(template))) {
    cli::cli_abort("{.arg x} and {.arg template} must have the same domain.")
  }
  if (!isTRUE(all.equal(tf_arg(x), tf_arg(template)))) {
    cli::cli_abort("{.arg x} and {.arg template} must have the same grid.")
  }
  invisible(template)
}

srvf_mv_to_array <- function(x) {
  aperm(as.matrix(x), c(3, 2, 1))
}

srvf_mv_array_to_tfd_mv <- function(
  beta,
  arg,
  comp_names,
  domain,
  curve_names = NULL
) {
  dims <- dim(beta)
  if (length(dims) == 2L) {
    components <- map(seq_len(dims[1]), \(k) {
      mat <- matrix(beta[k, ], nrow = 1L)
      if (!is.null(curve_names)) {
        rownames(mat) <- curve_names
      }
      mat
    })
  } else {
    components <- map(seq_len(dims[1]), \(k) {
      mat <- t(beta[k, , ])
      if (!is.null(curve_names)) {
        rownames(mat) <- curve_names
      }
      mat
    })
  }
  names(components) <- comp_names
  tfd_mv(components, arg = arg, domain = domain)
}

srvf_mv_gamma_to_warps <- function(gamma, arg, domain, curve_names = NULL) {
  gamma <- as.matrix(gamma)
  warp <- domain[1] + diff(domain) * t(gamma)
  warp[, 1] <- arg[1]
  warp[, ncol(warp)] <- arg[length(arg)]
  if (!is.null(curve_names)) {
    rownames(warp) <- curve_names
  }
  tfd(warp, arg = arg, domain = domain)
}

tf_register_srvf_mv <- function(
  x,
  template,
  max_iter,
  lambda = 0,
  ...
) {
  rlang::check_installed("fdasrvf")
  dots <- list(...)
  srvf_mv_check_dots(dots)
  srvf_mv_validate_regular(x)
  srvf_mv_validate_template(template, x)
  assert_count(max_iter, positive = TRUE)
  assert_number(lambda, lower = 0, finite = TRUE)

  arg <- tf_arg(x)
  domain <- tf_domain(x)
  comp_names <- attr(x, "comp_names")
  curve_names <- names(x)
  beta <- srvf_mv_to_array(x)

  if (is.null(template)) {
    if (max_iter < 2L) {
      cli::cli_abort(
        "{.arg max_iter} must be at least 2 for {.val srvf_mv} with {.arg template = NULL}."
      )
    }
    ret <- suppressMessages(do.call(
      fdasrvf::multivariate_karcher_mean,
      c(
        list(
          beta = beta,
          mode = "O",
          alignment = TRUE,
          rotation = FALSE,
          scale = FALSE,
          lambda = lambda,
          maxit = max_iter
        ),
        dots
      )
    ))
    warps <- srvf_mv_gamma_to_warps(ret$gamma, arg, domain, curve_names)
    attr(warps, "template") <- srvf_mv_array_to_tfd_mv(
      ret$betamean,
      arg = arg,
      comp_names = comp_names,
      domain = domain
    )
    return(warps)
  }

  template_beta <- srvf_mv_to_array(template)
  is_single_template <- length(template) == 1L
  gamma <- matrix(NA_real_, nrow = length(arg), ncol = length(x))
  for (i in seq_along(x)) {
    beta1 <- if (is_single_template) {
      template_beta[,, 1]
    } else {
      template_beta[,, i]
    }
    ret <- suppressMessages(do.call(
      fdasrvf::reparam_curve,
      c(
        list(
          beta1 = beta1,
          beta2 = beta[,, i],
          lambda = lambda,
          rotated = FALSE,
          mode = "O"
        ),
        dots
      )
    ))
    gamma[, i] <- ret$gam
  }
  warps <- srvf_mv_gamma_to_warps(gamma, arg, domain, curve_names)
  attr(warps, "template") <- template
  warps
}

# Shape registration -----------------------------------------------------------

#' Register vector-valued curves in elastic shape space
#'
#' `tf_register_shape()` aligns vector-valued `tf_mv` curves with SRVF curve
#' alignment, optionally allowing rotations and rescaling in addition to time
#' warping. Unlike [tf_register()], this is a shape-registration interface: the
#' aligned curves live in centered shape space and the result stores rotations
#' and scale factors.
#'
#' @details
#' The per-curve scale factors returned by [tf_scales()] are expressed
#' *relative to the template*: each is the ratio of the template's SRVF norm to
#' the curve's own SRVF norm, so a value `> 1` means the curve was scaled up to
#' match the template and `< 1` means it was scaled down. With `template = NULL`
#' the returned [tf_template()] is the empirical mean of the aligned shape-space
#' curves rather than any single input curve.
#'
#' Only open curves (`mode = "O"`) are supported. Closed curves (`mode = "C"`)
#' additionally optimise over a circular seed shift that the returned warping
#' functions do not represent, which would make the stored warps inconsistent
#' with the aligned curves; `mode = "C"` is therefore rejected for now.
#'
#' @param x a regular shared-grid `tfd_mv` object.
#' @param ... additional arguments passed to fdasrvf alignment routines, such as
#'   `ncores` or `verbose`.
#' @param template optional length-one `tf_mv` template. If `NULL`, a template
#'   is refined from the first curve by iterative group alignment.
#' @param max_iter integer: maximum template refinement iterations when
#'   `template = NULL`. Default `3L`.
#' @param tol numeric: relative template convergence tolerance.
#' @param rotation logical: allow rotations? Default `TRUE`.
#' @param scale logical: allow scale changes? Default `TRUE`.
#' @param mode character: curve mode for fdasrvf. Only open curves (`"O"`,
#'   the default) are currently supported; `"C"` (closed curves) is reserved
#'   for a future release (see Details).
#' @param lambda numeric: non-negative elastic penalty passed to the fdasrvf
#'   alignment routines. Larger values penalise warping. Default `0`.
#' @param store_x logical: store original data in the result object?
#' @returns A `tf_shape_registration` object. Access aligned curves with
#'   [tf_aligned()], inverse warps with [tf_inv_warps()], the template with
#'   [tf_template()], rotations with [tf_rotations()], and scales with
#'   [tf_scales()].
#' @examplesIf rlang::is_installed("fdasrvf")
#' t <- seq(0, 1, length.out = 51)
#' base <- rbind(t, t^2)
#' beta <- array(NA_real_, dim = c(3, length(t), 2))
#' for (i in 1:3) {
#'   beta[i,, 1] <- base[1, ]
#'   beta[i,, 2] <- base[2, ]
#' }
#' curves <- tfd_mv(beta, arg = t)
#' reg <- tf_register_shape(curves, max_iter = 1)
#' tf_rotations(reg)
#' tf_scales(reg)
#' @export
#' @family registration functions
tf_register_shape <- function(
  x,
  ...,
  template = NULL,
  max_iter = 3L,
  tol = 1e-2,
  rotation = TRUE,
  scale = TRUE,
  mode = c("O", "C"),
  lambda = 0,
  store_x = TRUE
) {
  cl <- match.call()
  rlang::check_installed("fdasrvf")
  srvf_mv_validate_regular(x)
  if (!is.null(template)) {
    srvf_mv_validate_regular(template, arg = "template")
    check_compatible_mv(x, template)
    if (length(template) != 1L) {
      cli::cli_abort(
        "{.arg template} must be a length-one {.cls tf_mv} object."
      )
    }
    if (!all(tf_domain(x) == tf_domain(template))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same domain.")
    }
    if (!isTRUE(all.equal(tf_arg(x), tf_arg(template)))) {
      cli::cli_abort("{.arg x} and {.arg template} must have the same grid.")
    }
  }
  assert_count(max_iter, positive = TRUE)
  assert_number(tol, lower = 0, finite = TRUE)
  assert_flag(rotation)
  assert_flag(scale)
  assert_flag(store_x)
  assert_number(lambda, lower = 0, finite = TRUE)
  mode <- match.arg(mode)
  if (mode == "C") {
    cli::cli_abort(c(
      "Closed-curve shape registration ({.code mode = \"C\"}) is not supported yet.",
      "i" = "Closed curves optimise over a circular seed shift that the returned warps cannot represent.",
      "i" = "Use {.code mode = \"O\"} for open curves."
    ))
  }

  shape <- tf_register_shape_srvf_mv(
    x = x,
    template = template,
    max_iter = max_iter,
    tol = tol,
    rotation = rotation,
    scale = scale,
    mode = mode,
    lambda = lambda,
    ...
  )
  inv_warps <- tf_invert(shape$warps)
  names(inv_warps) <- names(shape$warps)
  new_tf_shape_registration(
    registered = shape$registered,
    warps = shape$warps,
    inv_warps = inv_warps,
    template = shape$template,
    x = if (store_x) x else NULL,
    rotations = shape$rotations,
    scales = shape$scales,
    call = cl
  )
}

tf_register_shape_srvf_mv <- function(
  x,
  template,
  max_iter,
  tol,
  rotation,
  scale,
  mode,
  lambda = 0,
  ...
) {
  dots <- list(...)
  if ("maxit" %in% names(dots)) {
    cli::cli_abort(
      "Use {.arg max_iter}, not fdasrvf's {.arg maxit}, in {.fn tf_register_shape}."
    )
  }
  assert_number(lambda, lower = 0, finite = TRUE)

  arg <- tf_arg(x)
  domain <- tf_domain(x)
  comp_names <- attr(x, "comp_names")
  curve_names <- names(x)
  beta <- srvf_mv_to_array(x)
  current_template <- if (is.null(template)) {
    beta[,, 1]
  } else {
    srvf_mv_to_array(template)[,, 1]
  }
  best <- NULL
  best_template <- current_template
  iterations <- if (is.null(template)) max_iter else 1L

  for (iter in seq_len(iterations)) {
    ret <- suppressMessages(do.call(
      fdasrvf::multiple_align_multivariate,
      c(
        list(
          beta = beta,
          mu = current_template,
          mode = mode,
          alignment = TRUE,
          rotation = rotation,
          scale = scale,
          lambda = lambda
        ),
        dots
      )
    ))
    new_template <- rowMeans(ret$betan, dims = 2)
    best <- ret
    best_template <- new_template

    if (!is.null(template) || iter == iterations) {
      break
    }
    delta <- mean((new_template - current_template)^2)
    norm_sq <- mean(current_template^2)
    if (is.finite(delta) && delta / max(norm_sq, .Machine$double.eps) < tol^2) {
      break
    }
    current_template <- new_template
  }

  warps <- srvf_mv_gamma_to_warps(best$gamma, arg, domain, curve_names)
  scales <- if (scale) best$qmean_norm / best$len_q else rep(1, length(x))
  names(scales) <- curve_names
  rotations <- best$R
  dimnames(rotations) <- list(comp_names, comp_names, curve_names)
  list(
    registered = srvf_mv_array_to_tfd_mv(
      best$betan,
      arg = arg,
      comp_names = comp_names,
      domain = domain,
      curve_names = curve_names
    ),
    warps = warps,
    template = srvf_mv_array_to_tfd_mv(
      best_template,
      arg = arg,
      comp_names = comp_names,
      domain = domain
    ),
    rotations = rotations,
    scales = scales
  )
}
