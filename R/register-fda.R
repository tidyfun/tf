# FDA-style continuous registration backend -----------------------------------

new_regular_grid_info <- function(arg) {
  assert_numeric(arg, min.len = 2, any.missing = FALSE, finite = TRUE)
  step <- diff(arg)
  ref_step <- step[1]
  if (!all(abs(step - ref_step) <= max(1, abs(ref_step)) * 1e-10)) {
    cli::cli_abort(
      "{.arg arg} must be equally spaced for regular interpolation."
    )
  }

  list(start = arg[1], step = ref_step, n = length(arg))
}

interpolate_regular_grid <- function(grid_info, values, arg_out) {
  left_index <- floor((arg_out - grid_info$start) / grid_info$step) + 1L
  left_index <- pmin(pmax(left_index, 1L), grid_info$n - 1L)
  left_arg <- grid_info$start + (left_index - 1L) * grid_info$step
  alpha <- (arg_out - left_arg) / grid_info$step
  alpha <- pmin(pmax(alpha, 0), 1)

  values[left_index] + alpha * (values[left_index + 1L] - values[left_index])
}

cumulative_trapezoid <- function(arg, values) {
  cumsum(quad_trapez(arg, values))
}

cumulative_trapezoid_matrix <- function(arg, values) {
  integrated <- apply(as.matrix(values), 2, \(x) cumulative_trapezoid(arg, x))
  if (is.null(dim(integrated))) {
    matrix(integrated, ncol = 1)
  } else {
    integrated
  }
}

new_register_fda_problem <- function(
  x,
  template,
  nbasis,
  lambda,
  crit,
  conv,
  iterlim
) {
  assert_tfd(x)
  assert_int(nbasis, lower = 2)
  assert_number(lambda, lower = 0)
  assert_choice(crit, c(1L, 2L))
  assert_number(conv, lower = 0)
  assert_count(iterlim, positive = FALSE)

  template <- template %||% mean(x)
  arg <- tf_arg(x)
  domain <- tf_domain(x)
  arg_fine <- seq(domain[1], domain[2], length.out = max(201L, length(arg)))
  x_fine <- suppressWarnings(tf_interpolate(x, arg = arg_fine)) |> as.matrix()
  template_fine <- suppressWarnings(tf_interpolate(template, arg = arg_fine)) |>
    as.matrix()
  if (nrow(template_fine) == 1L && nrow(x_fine) > 1L) {
    template_fine <- template_fine[rep(1L, nrow(x_fine)), , drop = FALSE]
  }
  warp_smooth <- smooth.construct(
    s(arg, bs = "bs", k = nbasis, m = c(4, 2)),
    data = data_frame0(arg = arg_fine),
    knots = NULL
  )

  list(
    arg = arg,
    arg_fine = arg_fine,
    domain = domain,
    grid_info = new_regular_grid_info(arg_fine),
    x_fine = x_fine,
    dx_fine = derive_matrix(x_fine, arg = arg_fine, order = 1)$data,
    template = template,
    template_fine = template_fine,
    warp_design = warp_smooth[["X"]],
    warp_penalty = warp_smooth[["S"]][[1]],
    initial_coefs = matrix(0, nrow = nbasis, ncol = nrow(x_fine)),
    lambda = lambda,
    crit = crit,
    conv = conv,
    iterlim = iterlim
  )
}

build_monotone_warp <- function(coefs, problem) {
  eta <- drop(problem$warp_design %*% coefs)
  weights <- exp(pmin(eta, 50))
  integrated <- cumulative_trapezoid(problem$arg_fine, weights)
  total <- integrated[length(integrated)]

  if (!is.finite(total) || total <= 0) {
    return(list(ok = FALSE))
  }

  integrated_basis <- cumulative_trapezoid_matrix(
    problem$arg_fine,
    sweep(problem$warp_design, 1, weights, `*`)
  )
  derivative_total <- integrated_basis[nrow(integrated_basis), , drop = TRUE]
  width <- diff(problem$domain)
  warp_derivative <- width *
    (sweep(integrated_basis, 2, total, `*`) -
      integrated %o% derivative_total) /
    total^2
  warp <- problem$domain[1] + width * integrated / total
  warp[c(1, length(warp))] <- problem$domain

  list(ok = TRUE, warp = warp, derivative = warp_derivative)
}

register_fda_curve_objective <- function(
  par,
  coefs_start,
  curve_index,
  problem
) {
  active <- seq.int(2L, length(coefs_start))
  coefs <- coefs_start
  coefs[active] <- par
  warp_fit <- build_monotone_warp(coefs, problem)

  if (!warp_fit$ok) {
    return(list(
      value = Inf,
      gradient = rep(NA_real_, length(par)),
      coefs = coefs
    ))
  }

  template_values <- problem$template_fine[curve_index, ]
  aligned_values <- interpolate_regular_grid(
    problem$grid_info,
    problem$x_fine[curve_index, ],
    warp_fit$warp
  )
  aligned_derivatives <- interpolate_regular_grid(
    problem$grid_info,
    problem$dx_fine[curve_index, ],
    warp_fit$warp
  )
  dy_active <- warp_fit$derivative[, active, drop = FALSE] * aligned_derivatives

  aa <- mean(template_values^2)
  bb <- mean(template_values * aligned_values)
  cc <- mean(aligned_values^2)

  if (problem$crit == 1L) {
    residuals <- template_values - aligned_values
    value <- aa - 2 * bb + cc
    gradient <- -2 *
      drop(crossprod(dy_active, residuals)) /
      length(problem$arg_fine)
  } else {
    diff_part <- aa - cc
    discr <- sqrt(max(diff_part^2 + 4 * bb^2, .Machine$double.eps))
    d_bb <- drop(crossprod(dy_active, template_values)) /
      length(problem$arg_fine)
    d_cc <- 2 *
      drop(crossprod(dy_active, aligned_values)) /
      length(problem$arg_fine)
    value <- aa + cc - discr
    gradient <- d_cc - (4 * bb * d_bb - diff_part * d_cc) / discr
  }

  if (problem$lambda > 0) {
    penalty <- problem$lambda * problem$warp_penalty[-1, -1, drop = FALSE]
    value <- value + drop(crossprod(par, penalty %*% par))
    gradient <- gradient + 2 * drop(penalty %*% par)
  }

  list(value = value, gradient = gradient, coefs = coefs, warp = warp_fit$warp)
}

optimize_registration_curve <- function(curve_index, coefs_start, problem) {
  active <- seq.int(2L, length(coefs_start))
  if (length(active) == 0L || problem$iterlim <= 0L) {
    return(register_fda_curve_objective(
      par = numeric(0),
      coefs_start = coefs_start,
      curve_index = curve_index,
      problem = problem
    ))
  }

  cache <- new.env(parent = emptyenv())
  cache$par <- NULL
  cache$result <- NULL
  evaluate_objective <- function(par) {
    if (
      !is.null(cache$par) &&
        length(par) == length(cache$par) &&
        all(par == cache$par)
    ) {
      return(cache$result)
    }

    result <- register_fda_curve_objective(
      par = par,
      coefs_start = coefs_start,
      curve_index = curve_index,
      problem = problem
    )
    cache$par <- par
    cache$result <- result
    result
  }

  fit <- stats::optim(
    par = coefs_start[active],
    fn = \(par) evaluate_objective(par)$value,
    gr = \(par) evaluate_objective(par)$gradient,
    method = "L-BFGS-B",
    lower = rep(-50, length(active)),
    upper = rep(50, length(active)),
    control = list(maxit = problem$iterlim, pgtol = problem$conv)
  )

  evaluate_objective(fit$par)
}

solve_register_fda_problem <- function(problem) {
  coefs <- problem$initial_coefs
  warp_fine <- matrix(
    NA_real_,
    nrow = nrow(problem$x_fine),
    ncol = ncol(problem$x_fine)
  )

  for (curve_index in seq_len(nrow(problem$x_fine))) {
    fit <- optimize_registration_curve(
      curve_index,
      coefs[, curve_index],
      problem
    )
    coefs[, curve_index] <- fit$coefs
    warp_fine[curve_index, ] <- fit$warp
  }

  list(coefs = coefs, warp_fine = warp_fine)
}

tf_register_fda <- function(
  x,
  template,
  nbasis = 6L,
  lambda = 0,
  crit = 2L,
  conv = 1e-4,
  iterlim = 20L
) {
  problem <- new_register_fda_problem(
    x = x,
    template = template,
    nbasis = nbasis,
    lambda = lambda,
    crit = crit,
    conv = conv,
    iterlim = iterlim
  )
  solved <- solve_register_fda_problem(problem)
  warp_out <- t(apply(
    solved$warp_fine,
    1,
    \(warp) interpolate_regular_grid(problem$grid_info, warp, problem$arg)
  ))
  warp_out <- t(apply(
    warp_out,
    1,
    strictify_domain_preserving_warp,
    domain = problem$domain
  ))

  result <- tfd(warp_out, arg = problem$arg, domain = problem$domain)
  attr(result, "template") <- problem$template
  result
}
