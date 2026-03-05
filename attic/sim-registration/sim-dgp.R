# sim-dgp.R -- Data-Generating Processes for Registration Benchmark
#
# Provides:
#   make_template()       - two template types (sigbump, harmonic)
#   generate_shift_warps()    - Type A: pure horizontal shifts
#   generate_affine_warps()   - Type B: full affine (scale + shift)
#   generate_smooth_warps()   - Type C: GP-based elastic warps
#   generate_landmark_warps() - Type D: piecewise-linear landmark warps
#   generate_amplitude()      - amplitude variation (6 types)
#   generate_data()           - main entry point combining template + warps + amplitude + noise

devtools::load_all()

# --- Templates ----------------------------------------------------------------

#' Create template function on [0,1]
#'
#' @param type "sigbump" or "harmonic"
#' @param arg evaluation grid (numeric vector in [0,1])
#' @return tfd of length 1
make_template <- function(type = c("sigbump", "harmonic"), arg) {
  type <- match.arg(type)
  vals <- switch(
    type,
    sigbump = pnorm(arg, 0.2, 0.03) + 0.3 * dnorm(arg, 0.7, 0.08) - 0.5,
    harmonic = sin(2 * pi * arg) + 0.5 * sin(4 * pi * arg)
  )
  tfd(matrix(vals, nrow = 1), arg = arg)
}

#' Create template function from bundled tf dataset
#'
#' Extracts mean curve from a real dataset, rescales domain to [0,1],
#' interpolates to benchmark grid, and z-scores for comparability.
#'
#' @param source one of "pinch", "growth_velocity", "gait_hip"
#' @param arg evaluation grid (numeric vector in [0,1])
#' @return tfd of length 1
make_template_from_data <- function(
  source = c("pinch", "growth_velocity", "gait_hip"),
  arg
) {
  source <- match.arg(source)

  raw <- switch(
    source,
    pinch = {
      mean(pinch)
    },
    growth_velocity = {
      # Smooth height curves with basis, then differentiate to get velocity
      height_smooth <- tfb(growth[["height"]], k = 20)
      velocity <- tf_derive(height_smooth)
      mean(velocity)
    },
    gait_hip = {
      mean(gait[["hip_angle"]])
    }
  )

  # Rescale domain to [0,1]
  orig_arg <- tf_arg(raw)
  if (is.list(orig_arg)) orig_arg <- orig_arg[[1]]
  orig_vals <- tf_evaluations(raw)[[1]]
  orig_domain <- range(orig_arg)
  rescaled_arg <- (orig_arg - orig_domain[1]) / diff(orig_domain)

  # Interpolate to benchmark grid
  vals <- approx(rescaled_arg, orig_vals, xout = arg, rule = 2)$y

  # Z-score: center and scale to unit variance
  template_sd <- sd(vals)
  if (template_sd < 1e-10) {
    cli::cli_abort(
      "Template {.val {source}} has zero variance; cannot z-score."
    )
  }
  vals <- (vals - mean(vals)) / template_sd

  tfd(matrix(vals, nrow = 1), arg = arg)
}

# --- Warp Generators ----------------------------------------------------------

#' Type A: Pure horizontal shifts
#'
#' h_i(s) = s + b_i, b_i ~ Uniform(-severity*range/4, severity*range/4)
#' Non-domain-preserving. Post-hoc re-center: b_i <- b_i - mean(b_i).
#'
#' @param n number of curves
#' @param arg evaluation grid
#' @param severity controls shift magnitude
#' @return tfd of warping functions
generate_shift_warps <- function(n, arg, severity = 0.5) {
  range_width <- diff(range(arg))
  half_width <- severity * range_width / 4
  b <- runif(n, -half_width, half_width)
  b <- b - mean(b) # identifiability: E[b] = 0

  warp_mat <- outer(b, arg, function(bi, s) s + bi)
  tfd(warp_mat, arg = arg)
}

#' Type B: Full affine warps
#'
#' h_i(s) = a_i * s + b_i with severity-responsive parameters.
#' a_i ~ TruncNorm(1, (0.05*severity)^2, [1-0.15*severity, 1+0.15*severity])
#' b_i ~ TruncNorm(0, (0.03*severity)^2, [-0.07*severity, 0.07*severity])
#' Post-hoc re-center: a_i <- a_i - mean(a_i) + 1, b_i <- b_i - mean(b_i)
#'
#' @param n number of curves
#' @param arg evaluation grid
#' @param severity controls warp magnitude
#' @return tfd of warping functions
generate_affine_warps <- function(n, arg, severity = 0.5) {
  sd_a <- 0.05 * severity
  sd_b <- 0.03 * severity
  lower_a <- 1 - 0.15 * severity
  upper_a <- 1 + 0.15 * severity
  lower_b <- -0.07 * severity
  upper_b <- 0.07 * severity

  # Truncated normal draws
  a <- rtruncnorm(n, mean = 1, sd = sd_a, lower = lower_a, upper = upper_a)
  b <- rtruncnorm(n, mean = 0, sd = sd_b, lower = lower_b, upper = upper_b)

  # Re-center after truncation
  a <- a - mean(a) + 1
  b <- b - mean(b)

  warp_mat <- t(sapply(seq_len(n), function(i) a[i] * arg + b[i]))
  tfd(warp_mat, arg = arg)
}

#' Truncated normal distribution (rejection sampling)
#' @keywords internal
rtruncnorm <- function(n, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
  out <- numeric(n)
  for (i in seq_len(n)) {
    repeat {
      x <- rnorm(1, mean, sd)
      if (x >= lower && x <= upper) {
        out[i] <- x
        break
      }
    }
  }
  out
}

#' Type C: Smooth elastic warps (GP-based)
#'
#' GP -> exp -> integrate -> normalize. Domain-preserving (h(0)=0, h(1)=1).
#' Pattern from demo-registration.R.
#'
#' @param n number of curves
#' @param arg evaluation grid
#' @param severity controls GP amplitude (higher = more warping)
#' @return tfd of warping functions
generate_smooth_warps <- function(n, arg, severity = 0.5) {
  gp <- tf_rgp(n, arg = arg, nugget = 0.001)
  positive_funcs <- exp(severity * (gp - mean(gp)))
  warps <- tf_integrate(positive_funcs, definite = FALSE)
  max_vals <- tf_fmax(warps)
  warps / max_vals
}

#' Type D: Landmark warps (piecewise-linear)
#'
#' Perturb known landmark positions of the template.
#' Domain-preserving (h(0)=0, h(1)=1).
#'
#' @param n number of curves
#' @param arg evaluation grid
#' @param template_landmarks numeric vector of true landmark positions in template
#' @param severity controls perturbation magnitude
#' @return list with $warps (tfd) and $observed_landmarks (matrix)
generate_landmark_warps <- function(
  n,
  arg,
  template_landmarks,
  severity = 0.5
) {
  domain <- range(arg)
  n_lm <- length(template_landmarks)

  # Perturb each landmark by severity-scaled noise
  # Ensure landmarks stay within domain and remain sorted
  obs_landmarks <- matrix(nrow = n, ncol = n_lm)
  for (i in seq_len(n)) {
    lm_i <- template_landmarks + rnorm(n_lm, 0, 0.05 * severity)
    # Clamp to domain interior and ensure sorted
    lm_i <- pmax(domain[1] + 0.02, pmin(domain[2] - 0.02, lm_i))
    lm_i <- sort(lm_i)
    obs_landmarks[i, ] <- lm_i
  }

  # Build piecewise-linear warps:
  # warp maps template_landmarks -> observed_landmarks
  # h(template_lm_k) = observed_lm_k for each k
  # h(domain[1]) = domain[1], h(domain[2]) = domain[2]
  template_knots <- c(domain[1], template_landmarks, domain[2])
  warp_mat <- t(sapply(seq_len(n), function(i) {
    obs_knots <- c(domain[1], obs_landmarks[i, ], domain[2])
    approx(template_knots, obs_knots, xout = arg, rule = 2)$y
  }))

  list(
    warps = tfd(warp_mat, arg = arg),
    observed_landmarks = obs_landmarks
  )
}

# --- Amplitude Variation ------------------------------------------------------

#' Generate amplitude coefficients
#'
#' @param type one of "none", "mult", "corr_deriv", "corr_orth",
#'   "additive", "rank2"
#' @param n number of curves
#' @param amp_sd standard deviation for amplitude parameters
#' @param warp_distances numeric(n) per-curve ||h_i - id||_2 (for correlated types)
#' @param rho correlation between amplitude and warp distance (for correlated types)
#' @return list with amplitude parameters ($a, $b, $c as appropriate)
generate_amplitude <- function(
  type = c("none", "mult", "corr_deriv", "corr_orth", "additive", "rank2"),
  n,
  amp_sd = 0.3,
  warp_distances = NULL,
  rho = 0.6
) {
  type <- match.arg(type)

  switch(
    type,
    none = list(type = "none"),

    mult = {
      # LogNormal(-sigma^2/2, sigma^2) has mean 1
      a <- rlnorm(n, meanlog = -amp_sd^2 / 2, sdlog = amp_sd)
      list(type = "mult", a = a)
    },

    corr_deriv = {
      # c_i correlated with warp distance
      checkmate::assert_numeric(warp_distances, len = n, any.missing = FALSE)
      sd_d <- sd(warp_distances)
      if (n < 3 || sd_d < 1e-10) {
        # Degenerate: fall back to independent amplitude
        c_coefs <- amp_sd * rnorm(n)
      } else {
        z_d <- (warp_distances - mean(warp_distances)) / sd_d
        z_indep <- rnorm(n)
        c_coefs <- amp_sd * (rho * z_d + sqrt(1 - rho^2) * z_indep)
      }
      list(type = "corr_deriv", c = c_coefs)
    },

    corr_orth = {
      # c_i correlated with warp distance, direction orthogonal to m'
      checkmate::assert_numeric(warp_distances, len = n, any.missing = FALSE)
      sd_d <- sd(warp_distances)
      if (n < 3 || sd_d < 1e-10) {
        c_coefs <- amp_sd * rnorm(n)
      } else {
        z_d <- (warp_distances - mean(warp_distances)) / sd_d
        z_indep <- rnorm(n)
        c_coefs <- amp_sd * (rho * z_d + sqrt(1 - rho^2) * z_indep)
      }
      list(type = "corr_orth", c = c_coefs)
    },

    additive = {
      b <- rnorm(n, 0, amp_sd)
      list(type = "additive", b = b)
    },

    rank2 = {
      a <- rlnorm(n, meanlog = -amp_sd^2 / 2, sdlog = amp_sd)
      c_coefs <- rnorm(n, 0, amp_sd)
      list(type = "rank2", a = a, c = c_coefs)
    }
  )
}

# --- Eigenfunction Constructors -----------------------------------------------

#' Compute derivative-based eigenfunction phi_deriv = m'/||m'||
#' @param template tfd of length 1
#' @param arg evaluation grid
#' @return numeric vector of length(arg), L2-normalized
compute_phi_deriv <- function(template, arg) {
  m_deriv <- tf_derive(template)
  # tf_derive returns midpoints; interpolate back to original grid
  d_arg <- tf_arg(m_deriv)
  if (is.list(d_arg)) d_arg <- d_arg[[1]]
  d_vals <- tf_evaluations(m_deriv)[[1]]
  phi <- approx(d_arg, d_vals, xout = arg, rule = 2)$y
  # L2-normalize
  l2_norm <- sqrt(trapez_int(arg, phi^2))
  if (l2_norm < 1e-10) {
    warning("Template derivative near-zero; phi_deriv may be degenerate")
    return(phi)
  }
  phi / l2_norm
}

#' Compute orthogonal eigenfunction phi_orth
#'
#' phi_orth = [(t-0.5)^2 - proj_{m'} (t-0.5)^2] / ||...||_2
#' Orthogonalized to m'(t), then normalized to unit L2 norm.
#'
#' @param template tfd of length 1
#' @param arg evaluation grid
#' @return numeric vector of length(arg), L2-normalized
compute_phi_orth <- function(template, arg) {
  # Base function: (t - 0.5)^2
  g <- (arg - 0.5)^2

  # Get m'(t) on the same grid
  m_deriv <- tf_derive(template)
  d_arg <- tf_arg(m_deriv)
  if (is.list(d_arg)) d_arg <- d_arg[[1]]
  d_vals <- tf_evaluations(m_deriv)[[1]]
  m_prime <- approx(d_arg, d_vals, xout = arg, rule = 2)$y

  # Project g onto m': proj = <g, m'> / <m', m'> * m'
  inner_gm <- trapez_int(arg, g * m_prime)
  inner_mm <- trapez_int(arg, m_prime * m_prime)
  if (inner_mm < 1e-10) {
    phi <- g
  } else {
    phi <- g - (inner_gm / inner_mm) * m_prime
  }

  # L2-normalize
  l2_norm <- sqrt(trapez_int(arg, phi^2))
  if (l2_norm < 1e-10) {
    warning("Orthogonal eigenfunction near-zero")
    return(phi)
  }
  phi / l2_norm
}

#' Trapezoidal integration helper
#' @keywords internal
trapez_int <- function(arg, vals) {
  sum(diff(arg) * (head(vals, -1) + vals[-1]) / 2)
}

# --- Compute Warp Distances ---------------------------------------------------

#' Compute per-curve L2 distance from warp to identity
#' @param warps tfd of warps
#' @param arg evaluation grid
#' @return numeric vector of distances
compute_warp_distances <- function(warps, arg) {
  identity_vals <- arg
  warp_evals <- tf_evaluations(warps)
  sapply(warp_evals, function(w) {
    sqrt(trapez_int(arg, (w - identity_vals)^2))
  })
}

# --- Main Data Generator ------------------------------------------------------

#' Generate a complete dataset for one DGP
#'
#' @param dgp character DGP identifier (S1-S11)
#' @param n number of curves
#' @param n_grid number of grid points
#' @param severity warp severity
#' @param noise_sd noise standard deviation
#' @param seed random seed
#' @param amp_sd amplitude effect size (default 0.3)
#' @return list with components:
#'   x (tfd): observed curves
#'   template (tfd): true template
#'   warps (tfd): true warping functions
#'   arg (numeric): evaluation grid
#'   amp_coefs (list): amplitude coefficients used
#'   true_landmarks (matrix or NULL): true landmark positions
#'   seed (integer): seed used
generate_data <- function(
  dgp,
  n = 50,
  n_grid = 101,
  severity = 0.5,
  noise_sd = 0,
  seed = NULL,
  amp_sd = 0.3
) {
  if (!is.null(seed)) set.seed(seed)
  arg <- seq(0, 1, length.out = n_grid)

  # Look up DGP specification
  spec <- dgp_spec(dgp)

  # Override amp_sd from spec if present (R-prefixed DGPs have amp_sd = 0.5)
  if (!is.null(spec$amp_sd)) amp_sd <- spec$amp_sd

  # 1. Template
  if (!is.null(spec$template_source)) {
    template <- make_template_from_data(spec$template_source, arg)
  } else {
    template <- make_template(spec$template, arg)
  }

  # 2. Warps
  warp_result <- switch(
    spec$phase,
    shift = {
      w <- generate_shift_warps(n, arg, severity)
      list(warps = w, true_landmarks = NULL)
    },
    affine = {
      w <- generate_affine_warps(n, arg, severity)
      list(warps = w, true_landmarks = NULL)
    },
    smooth = {
      w <- generate_smooth_warps(n, arg, severity)
      list(warps = w, true_landmarks = NULL)
    },
    landmark = {
      # Find local extrema of template (works for any template type:
      # harmonic, real-data, etc.)
      template_lm <- find_template_landmarks(template, arg)
      result <- generate_landmark_warps(n, arg, template_lm, severity)
      list(
        warps = result$warps,
        true_landmarks = result$observed_landmarks,
        template_landmarks = template_lm
      )
    }
  )
  warps <- warp_result$warps

  # 3. Apply warps to template: x_i(t) = m(h_i(t))
  # For non-domain-preserving warps (shift, affine), warp values can extend
  # outside [0,1]. We evaluate m at h_i(t), getting NA where h_i(t) is outside
  # the template domain. We construct the result directly on the regular grid.
  is_non_dp <- spec$phase %in% c("shift", "affine")

  if (is_non_dp) {
    # For non-domain-preserving warps, the observation model is:
    # x_i has the template's shape but is observed on a shifted/scaled domain.
    # We construct x_i(t) = m(h_i(t)) on the regular grid.
    # Where h_i(t) falls outside [0,1], we use rule=2 (nearest-value
    # extrapolation) so the curve stays "flat" at the boundary template value.
    # This models realistic boundary behavior: the measurement continues
    # but the interesting template shape has shifted out of view.
    template_vals <- tf_evaluations(template)[[1]]
    warp_evals <- tf_evaluations(warps)
    warped_mat <- t(sapply(warp_evals, function(w) {
      approx(arg, template_vals, xout = w, rule = 2)$y
    }))
    warped <- tfd(warped_mat, arg = arg)
  } else {
    warped <- tf_warp(rep(template, n), warps)
  }

  # 4. Amplitude variation
  warp_dists <- compute_warp_distances(warps, arg)
  amp <- generate_amplitude(
    type = spec$amplitude,
    n = n,
    amp_sd = amp_sd,
    warp_distances = warp_dists,
    rho = 0.6
  )

  x <- apply_amplitude(warped, amp, template, arg)

  # 5. Noise
  if (noise_sd > 0) {
    noise_mat <- matrix(rnorm(n * n_grid, 0, noise_sd), nrow = n)
    x <- x + tfd(noise_mat, arg = arg)
  }

  # Ensure x is regular tfd on the original grid
  if (inherits(x, "tfd_irreg")) {
    x <- tfd(x, arg = arg)
  }

  list(
    x = x,
    template = template,
    warps = warps,
    arg = arg,
    amp_coefs = amp,
    true_landmarks = warp_result$true_landmarks,
    template_landmarks = warp_result$template_landmarks,
    warp_distances = warp_dists,
    seed = seed,
    dgp = dgp,
    spec = spec
  )
}

# --- DGP Specification Table --------------------------------------------------

#' Get DGP specification
#' @param dgp character: S1-S11
#' @return list with template, phase, amplitude
dgp_spec <- function(dgp) {
  specs <- list(
    S1 = list(template = "sigbump", phase = "shift", amplitude = "none"),
    S2 = list(template = "sigbump", phase = "shift", amplitude = "mult"),
    S3 = list(template = "harmonic", phase = "smooth", amplitude = "none"),
    S4 = list(template = "sigbump", phase = "smooth", amplitude = "none"),
    S5 = list(template = "sigbump", phase = "smooth", amplitude = "mult"),
    S6a = list(
      template = "sigbump",
      phase = "smooth",
      amplitude = "corr_deriv"
    ),
    S6b = list(template = "sigbump", phase = "smooth", amplitude = "corr_orth"),
    S7 = list(template = "sigbump", phase = "smooth", amplitude = "additive"),
    S8 = list(template = "harmonic", phase = "landmark", amplitude = "none"),
    S9 = list(template = "harmonic", phase = "landmark", amplitude = "mult"),
    S10 = list(template = "sigbump", phase = "smooth", amplitude = "rank2"),
    S11 = list(template = "sigbump", phase = "affine", amplitude = "none"),

    # Real-data DGPs (Study 4): template_source instead of template, amp_sd = 0.5
    R1a = list(
      template_source = "pinch",
      phase = "smooth",
      amplitude = "rank2",
      amp_sd = 0.5
    ),
    R1b = list(
      template_source = "pinch",
      phase = "shift",
      amplitude = "mult",
      amp_sd = 0.5
    ),
    R2a = list(
      template_source = "growth_velocity",
      phase = "smooth",
      amplitude = "rank2",
      amp_sd = 0.5
    ),
    R2b = list(
      template_source = "growth_velocity",
      phase = "smooth",
      amplitude = "corr_orth",
      amp_sd = 0.5
    ),
    R3a = list(
      template_source = "gait_hip",
      phase = "smooth",
      amplitude = "rank2",
      amp_sd = 0.5
    ),
    R3b = list(
      template_source = "gait_hip",
      phase = "landmark",
      amplitude = "mult",
      amp_sd = 0.5
    )
  )
  if (!dgp %in% names(specs)) {
    cli::cli_abort(
      "Unknown DGP: {dgp}. Must be one of: {paste(names(specs), collapse = ', ')}"
    )
  }
  specs[[dgp]]
}

# --- Apply Amplitude Variation ------------------------------------------------

#' Apply amplitude variation to warped curves
#'
#' @param warped tfd of warped curves (m(h_i(t)))
#' @param amp list from generate_amplitude()
#' @param template tfd of length 1
#' @param arg evaluation grid
#' @return tfd of observed curves (before noise)
apply_amplitude <- function(warped, amp, template, arg) {
  n <- length(warped)
  switch(
    amp$type,
    none = warped,

    mult = {
      # x_i(t) = a_i * m(h_i(t))
      warped * amp$a
    },

    corr_deriv = {
      # Intentional deviation from plan: we use phi_deriv(t), not phi_deriv(h_i(t)).
      # The plan specifies phi_deriv(h_i(t)), but since S6a is a stress test for
      # near-unidentifiability (Taylor: m(h+c) ~ m(h) + c*m'(h)), using the
      # un-warped phi(t) still creates confounding -- just slightly less than the
      # fully-warped version. The un-warped version is simpler and still serves
      # its purpose as a stress test. S6b (orthogonal) also uses phi(t).
      phi <- compute_phi_deriv(template, arg)
      phi_tf <- tfd(matrix(phi, nrow = 1), arg = arg)
      warped + outer_multiply(amp$c, phi_tf)
    },

    corr_orth = {
      # x_i(t) = m(h_i(t)) + c_i * phi_orth(t)
      phi <- compute_phi_orth(template, arg)
      phi_tf <- tfd(matrix(phi, nrow = 1), arg = arg)
      warped + outer_multiply(amp$c, phi_tf)
    },

    additive = {
      # x_i(t) = m(h_i(t)) + b_i
      warped + amp$b
    },

    rank2 = {
      # x_i(t) = a_i * m(h_i(t)) + c_i
      warped * amp$a + amp$c
    }
  )
}

#' Multiply scalar vector by a single tf function
#' @keywords internal
outer_multiply <- function(scalars, tf_single) {
  rep(tf_single, length(scalars)) * scalars
}

# --- Outlier Contamination ----------------------------------------------------

#' Contaminate a generated dataset with outliers
#'
#' Applied AFTER generate_data(). Modifies a fraction of curves in-place.
#' Truth (warps, template) is still known for clean curves.
#' Metrics should be computed on ALL curves including outliers (realistic).
#'
#' @param data list from generate_data()
#' @param contam_frac fraction of curves to contaminate (e.g. 0.10)
#' @param outlier_type one of "amplitude", "phase", "shape"
#' @param seed seed offset for deterministic outlier selection
#' @return modified data list with added $outlier_mask (logical vector)
contaminate_data <- function(
  data,
  contam_frac = 0.10,
  outlier_type = c("amplitude", "phase", "shape"),
  seed = NULL
) {
  outlier_type <- match.arg(outlier_type)
  checkmate::assert_number(contam_frac, lower = 0, upper = 1)
  n <- length(data$x)
  n_outliers <- floor(n * contam_frac)
  if (n_outliers == 0) {
    data$outlier_mask <- rep(FALSE, n)
    return(data)
  }

  # Save and restore RNG state so contamination doesn't pollute downstream RNG
  # (seed + 10000L offset avoids collision with data generation seeds 1..n_reps)
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
      .Random.seed else NULL
    on.exit(
      {
        if (!is.null(old_seed)) {
          .Random.seed <<- old_seed
        }
      },
      add = TRUE
    )
    set.seed(seed + 10000L)
  }
  outlier_idx <- sample.int(n, n_outliers)
  outlier_mask <- seq_len(n) %in% outlier_idx

  arg <- data$arg
  x_mat <- as.matrix(data$x)

  switch(
    outlier_type,

    amplitude = {
      # Scale outlier curves by random factor: Uniform(3,5) * random sign
      scale_factors <- runif(n_outliers, 3, 5) *
        sample(c(-1, 1), n_outliers, replace = TRUE)
      for (j in seq_along(outlier_idx)) {
        x_mat[outlier_idx[j], ] <- x_mat[outlier_idx[j], ] * scale_factors[j]
      }
    },

    phase = {
      # Replace outlier warps with extreme warps (severity = 3.0)
      extreme_warps <- generate_smooth_warps(n_outliers, arg, severity = 3.0)
      extreme_warp_evals <- tf_evaluations(extreme_warps)
      template_vals <- tf_evaluations(data$template)[[1]]
      for (j in seq_along(outlier_idx)) {
        w <- extreme_warp_evals[[j]]
        x_mat[outlier_idx[j], ] <- approx(
          arg,
          template_vals,
          xout = w,
          rule = 2
        )$y
      }
    },

    shape = {
      # Replace template: sigbump curves get harmonic, harmonic get sigbump.
      # For R-prefix DGPs (template_source, no template field), defaults to
      # harmonic as the "wrong" shape. Study 3 only uses synthetic DGPs.
      spec <- data$spec
      opposite_type <- if (
        is.null(spec$template) || spec$template == "sigbump"
      ) {
        "harmonic"
      } else {
        "sigbump"
      }
      wrong_template <- make_template(opposite_type, arg)
      wrong_vals <- tf_evaluations(wrong_template)[[1]]
      # Apply the outlier curve's existing warp to the wrong template
      warp_evals <- tf_evaluations(data$warps)
      for (j in seq_along(outlier_idx)) {
        i <- outlier_idx[j]
        w <- warp_evals[[i]]
        x_mat[i, ] <- approx(arg, wrong_vals, xout = w, rule = 2)$y
      }
    }
  )

  data$x <- tfd(x_mat, arg = arg)
  data$outlier_mask <- outlier_mask
  data
}

# --- Find Template Landmarks -------------------------------------------------

#' Find landmark positions in template
#' @param template tfd of length 1
#' @param arg evaluation grid
#' @return numeric vector of landmark positions
find_template_landmarks <- function(template, arg) {
  # Find all local extrema (maxima and minima)
  vals <- tf_evaluations(template)[[1]]
  n <- length(vals)

  # Find local maxima and minima
  landmarks <- numeric(0)
  for (i in 2:(n - 1)) {
    if (vals[i] > vals[i - 1] && vals[i] > vals[i + 1]) {
      landmarks <- c(landmarks, arg[i])
    } else if (vals[i] < vals[i - 1] && vals[i] < vals[i + 1]) {
      landmarks <- c(landmarks, arg[i])
    }
  }
  landmarks
}
