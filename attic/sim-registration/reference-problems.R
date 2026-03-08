new_registration_warp_basis <- function(
  domain,
  nbasis = 6L,
  degree = 3L,
  internal_knots = numeric(0)
) {
  if (length(internal_knots) == 0L) {
    n_internal <- nbasis - degree - 1L
    if (n_internal > 0L) {
      internal_knots <- seq(domain[1], domain[2], length.out = n_internal + 2L)
      internal_knots <- internal_knots[-c(1L, n_internal + 2L)]
    }
  }

  list(
    domain = domain,
    nbasis = nbasis,
    degree = degree,
    internal_knots = internal_knots
  )
}

registration_basis_knots <- function(basis) {
  c(
    rep(basis$domain[1], basis$degree + 1L),
    basis$internal_knots,
    rep(basis$domain[2], basis$degree + 1L)
  )
}

registration_basis_matrix <- function(arg, basis) {
  splines::splineDesign(
    knots = registration_basis_knots(basis),
    x = arg,
    ord = basis$degree + 1L,
    derivs = rep.int(0L, length(arg)),
    outer.ok = TRUE
  )
}

registration_cumtrapz <- function(arg, values) {
  increments <- c(0, diff(arg) * (values[-1] + values[-length(values)]) / 2)
  cumsum(increments)
}

new_registration_warp_spec <- function(
  domain,
  n_curves,
  nbasis = 6L,
  lambda = 0,
  degree = 3L
) {
  basis <- new_registration_warp_basis(
    domain = domain,
    nbasis = nbasis,
    degree = degree
  )

  list(
    basis = basis,
    initial_coefs = matrix(0, nrow = basis$nbasis, ncol = n_curves),
    lambda = lambda
  )
}

warp_coefs_to_tfd <- function(coefs, basis, arg) {
  design <- registration_basis_matrix(arg = arg, basis = basis)
  eta <- pmin(design %*% as.matrix(coefs), 50)
  weights <- exp(eta)
  integrated <- apply(weights, 2, registration_cumtrapz, arg = arg)
  if (is.null(dim(integrated))) {
    integrated <- matrix(integrated, ncol = 1)
  }
  width <- diff(basis$domain)
  totals <- integrated[nrow(integrated), ]
  warp <- basis$domain[1] + width * sweep(integrated, 2, totals, `/`)
  warp[1, ] <- basis$domain[1]
  warp[nrow(warp), ] <- basis$domain[2]
  tfd(t(warp), arg = arg, domain = basis$domain)
}

registration_reference_problems <- function() {
  arg <- seq(0, 1, length.out = 81)
  domain <- c(0, 1)
  gait_data <- get("gait", envir = asNamespace("tf"))

  template_easy <- tfd(
    sin(2 * pi * arg) + 0.25 * cos(4 * pi * arg),
    arg = arg,
    domain = domain
  )
  template_easy_values <- tf_evaluate(template_easy, arg = arg)[[1]]
  easy_spec <- new_registration_warp_spec(domain = domain, n_curves = 4L)
  easy_coefs <- matrix(
    c(
      0,
      -0.85,
      -0.30,
      0.08,
      0.35,
      0.80,
      0,
      -0.30,
      -0.10,
      0.02,
      0.12,
      0.28,
      0,
      0.22,
      0.07,
      -0.02,
      -0.10,
      -0.24,
      0,
      0.75,
      0.26,
      -0.08,
      -0.32,
      -0.72
    ),
    nrow = easy_spec$basis$nbasis
  )
  easy_forward_warps <- warp_coefs_to_tfd(
    coefs = easy_coefs,
    basis = easy_spec$basis,
    arg = arg
  )
  easy_x <- tf_warp(
    rep(template_easy, length(easy_forward_warps)),
    easy_forward_warps
  )

  template_wiggly <- tfd(
    0.6 *
      sin(2 * pi * arg) +
      0.5 * sin(5 * pi * arg + 0.3) +
      0.45 * cos(8 * pi * arg - 0.7) +
      0.3 * sin(11 * pi * arg + 0.9),
    arg = arg,
    domain = domain
  )
  wiggly_spec <- new_registration_warp_spec(domain = domain, n_curves = 4L)
  wiggly_coefs <- matrix(
    c(
      0,
      -1.10,
      -0.50,
      0.14,
      0.55,
      1.00,
      0,
      -0.45,
      -0.18,
      0.04,
      0.18,
      0.45,
      0,
      0.30,
      0.12,
      -0.04,
      -0.16,
      -0.34,
      0,
      0.95,
      0.40,
      -0.12,
      -0.48,
      -0.95
    ),
    nrow = wiggly_spec$basis$nbasis
  )
  wiggly_forward_warps <- warp_coefs_to_tfd(
    coefs = wiggly_coefs,
    basis = wiggly_spec$basis,
    arg = arg
  )
  wiggly_x <- tf_warp(
    rep(template_wiggly, length(wiggly_forward_warps)),
    wiggly_forward_warps
  )

  amplitude_spec <- new_registration_warp_spec(domain = domain, n_curves = 4L)
  amplitude_coefs <- matrix(
    c(
      0,
      -0.75,
      -0.24,
      0.08,
      0.28,
      0.70,
      0,
      -0.18,
      -0.05,
      0.01,
      0.08,
      0.18,
      0,
      0.28,
      0.10,
      -0.03,
      -0.12,
      -0.28,
      0,
      0.88,
      0.34,
      -0.10,
      -0.42,
      -0.88
    ),
    nrow = amplitude_spec$basis$nbasis
  )
  amplitude_forward_warps <- warp_coefs_to_tfd(
    coefs = amplitude_coefs,
    basis = amplitude_spec$basis,
    arg = arg
  )
  amp_scale <- c(0.85, 1, 1.1, 1.25)
  aligned_amplitude <- tfd(
    t(vapply(
      seq_along(amp_scale),
      \(i)
        amp_scale[i] * template_easy_values + 0.08 * cos(6 * pi * arg + i / 3),
      numeric(length(arg))
    )),
    arg = arg,
    domain = domain
  )
  amplitude_x <- tf_warp(aligned_amplitude, amplitude_forward_warps)

  real_idx <- c(1, 6, 11, 16, 21)
  real_x <- gait_data$hip_angle[real_idx]
  real_spec <- new_registration_warp_spec(
    domain = tf_domain(real_x),
    n_curves = length(real_x)
  )

  list(
    easy_phase = list(
      name = "easy_phase",
      x = easy_x,
      template = template_easy,
      aligned = rep(template_easy, length(easy_forward_warps)),
      true_forward_warps = easy_forward_warps,
      warp_spec = easy_spec,
      true_warp_coefs = easy_coefs
    ),
    wiggly_phase = list(
      name = "wiggly_phase",
      x = wiggly_x,
      template = template_wiggly,
      aligned = rep(template_wiggly, length(wiggly_forward_warps)),
      true_forward_warps = wiggly_forward_warps,
      warp_spec = wiggly_spec,
      true_warp_coefs = wiggly_coefs
    ),
    amplitude_phase = list(
      name = "amplitude_phase",
      x = amplitude_x,
      template = template_easy,
      aligned = aligned_amplitude,
      true_forward_warps = amplitude_forward_warps,
      warp_spec = amplitude_spec,
      true_warp_coefs = amplitude_coefs
    ),
    real_gait = list(
      name = "real_gait",
      x = real_x,
      template = mean(real_x),
      aligned = NULL,
      true_forward_warps = NULL,
      warp_spec = real_spec,
      true_warp_coefs = NULL
    )
  )
}

registration_problem_wfd_par <- function(problem) {
  basis <- problem$warp_spec$basis
  fda_basis <- fda::create.bspline.basis(
    rangeval = basis$domain,
    nbasis = basis$nbasis,
    norder = basis$degree + 1L,
    breaks = c(basis$domain[1], basis$internal_knots, basis$domain[2])
  )
  Wfd0 <- fda::fd(problem$warp_spec$initial_coefs, fda_basis)
  fda::fdPar(Wfd0, lambda = problem$warp_spec$lambda)
}

registration_fit_forward_warp_matrix <- function(fit, arg) {
  shift <- fit$shift
  if (is.null(shift)) {
    shift <- rep(0, ncol(as.matrix(fit$Wfd$coefs)))
  }
  basis <- fit$Wfd$basis
  basis_info <- new_registration_warp_basis(
    domain = basis$rangeval,
    nbasis = basis$nbasis,
    degree = basis$nbasis - length(basis$params) - 1L,
    internal_knots = basis$params
  )
  warp_matrix <- warp_coefs_to_tfd(
    coefs = fit$Wfd$coefs,
    basis = basis_info,
    arg = arg
  ) |>
    as.matrix() |>
    t()
  sweep(warp_matrix, 2, shift, `+`)
}
