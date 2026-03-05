#' ---
#' title: "Registration Methods: Demo & Stress Tests"
#' author: "tidyfun"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#'
#' Demonstrates `tf_register()` with methods: "srvf", "fda", "affine", "landmark"
#' and stress-tests various scenarios to inform simulation study design.
#'

#+ setup, include = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 9,
  fig.height = 4
)

#+ load-packages
devtools::load_all()
library(ggplot2)
set.seed(42)

#' ## Helper Functions
#'
#' ### Alignment Quality Metric
#' Cross-correlation between aligned functions and template (higher = better).

alignment_quality <- function(aligned, template) {
  # Expand template to match aligned length if needed
  if (length(template) == 1) {
    template <- rep(template, length(aligned))
  }
  mean(tf_crosscor(aligned, template))
}

#' ### Warp Recovery Error
#' Integrated squared difference between estimated and true warps (lower = better).

warp_error <- function(warp_est, warp_true) {
  # Evaluate both on common grid
  arg <- tf_arg(warp_est)
  if (is.list(arg)) arg <- arg[[1]]
  warp_est <- tfd(warp_est, arg = arg)
  warp_true <- tfd(warp_true, arg = arg)
  # Mean integrated squared error
  mean(tf_integrate((warp_est - warp_true)^2))
}

#' ### Quick Visualization
#' Three-panel plot showing original functions, warping functions, and aligned result.

plot_registration <- function(x, warp, aligned, title = "", template = NULL) {
  op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  on.exit(par(op))

  n <- length(x)
  cols <- if (n <= 10) 1:n else rep(1:8, length.out = n)

  plot(x, col = cols, main = paste(title, "- Original"))
  if (!is.null(template)) lines(template, lwd = 2, lty = 2)

  plot(warp, col = cols, main = "Warping Functions")
  # Add identity line
  domain <- tf_domain(warp)
  abline(a = 0, b = 1, lty = 2, col = "gray50")

  plot(aligned, col = cols, main = "Aligned")
  if (!is.null(template)) lines(template, lwd = 2, lty = 2)
}

#' ### Run All Methods and Compare

run_all_methods <- function(x, template = NULL, ...) {
  results <- list()

  # SRVF
  tryCatch(
    {
      t0 <- Sys.time()
      warp_srvf <- tf_register(x, template = template, method = "srvf")
      results$srvf <- list(
        warp = warp_srvf,
        aligned = tf_unwarp(x, warp_srvf),
        time = as.numeric(Sys.time() - t0, units = "secs")
      )
    },
    error = function(e) message("SRVF failed: ", e$message)
  )

  # FDA
  tryCatch(
    {
      t0 <- Sys.time()
      warp_fda <- tf_register(x, template = template, method = "fda", ...)
      results$fda <- list(
        warp = warp_fda,
        aligned = tf_unwarp(x, warp_fda),
        time = as.numeric(Sys.time() - t0, units = "secs")
      )
    },
    error = function(e) message("FDA failed: ", e$message)
  )

  # Affine (shift)
  tryCatch(
    {
      t0 <- Sys.time()
      warp_affine <- tf_register(
        x,
        method = "affine",
        template = template,
        type = "shift"
      )
      results$affine_shift <- list(
        warp = warp_affine,
        aligned = tf_unwarp(x, warp_affine),
        time = as.numeric(Sys.time() - t0, units = "secs")
      )
    },
    error = function(e) message("Affine (shift) failed: ", e$message)
  )

  results
}

#' ### Summarize Results

summarize_results <- function(results, template = NULL, true_warp = NULL) {
  summary_df <- data.frame(
    method = character(),
    alignment_quality = numeric(),
    warp_error = numeric(),
    time_secs = numeric(),
    stringsAsFactors = FALSE
  )

  for (method in names(results)) {
    res <- results[[method]]
    aq <- if (!is.null(template)) alignment_quality(res$aligned, template) else
      NA
    we <- if (!is.null(true_warp)) warp_error(res$warp, true_warp) else NA

    summary_df <- rbind(
      summary_df,
      data.frame(
        method = method,
        alignment_quality = round(aq, 4),
        warp_error = round(we, 6),
        time_secs = round(res$time, 3)
      )
    )
  }

  summary_df
}

#' ---
#'
#' # Scenario A: Simple Phase Shifts
#'
#' Functions differ only by horizontal translation. Affine "shift" should
#' recover perfectly; SRVF/FDA are overkill but should still work.
#'

#+ scenario-a-setup
# Generate: sin(t + phi) with random phi ~ Uniform(-0.5, 0.5)
set.seed(123)
n <- 10
t <- seq(0, 2 * pi, length.out = 101)
shifts <- runif(n, -0.5, 0.5)

# Template is unshifted sinusoid
template_a <- tfd(matrix(sin(t), nrow = 1), arg = t)

# Create shifted functions
x_shifted <- tfd(
  t(sapply(shifts, function(s) sin(t + s))),
  arg = t
)

# True warping functions: h(t) = t + shift (but clamped to domain)
# For pure shifts, warp(s) = s + shift maps system time s to observed time t
# So inverse warp (for unwarp) would map t to s = t - shift
# But tf_register returns warp h such that x_aligned(s) = x(h(s))
# For shift, h(s) = s + shift (observed time as function of system time)
true_warp_a <- tfd(
  t(sapply(shifts, function(s) {
    pmin(pmax(t + s, min(t)), max(t)) # clamp to domain
  })),
  arg = t
)

#+ scenario-a-plot-orig
plot(x_shifted, col = 1:n, main = "Scenario A: Shifted Sinusoids")
lines(template_a, lwd = 3, lty = 2)

#' ## Test: Affine Registration (shift)

#+ scenario-a-affine
warp_affine_a <- tf_register(
  x_shifted,
  method = "affine",
  template = template_a,
  type = "shift"
)
aligned_affine_a <- tf_unwarp(x_shifted, warp_affine_a)

plot_registration(
  x_shifted,
  warp_affine_a,
  aligned_affine_a,
  title = "Affine (shift)",
  template = template_a
)

#' ## Compare All Methods

#+ scenario-a-compare
results_a <- run_all_methods(x_shifted, template = template_a)

#+ scenario-a-srvf-plot, fig.height = 4
if (!is.null(results_a$srvf)) {
  plot_registration(
    x_shifted,
    results_a$srvf$warp,
    results_a$srvf$aligned,
    title = "SRVF",
    template = template_a
  )
}

#+ scenario-a-summary
summary_a <- summarize_results(
  results_a,
  template = template_a,
  true_warp = true_warp_a
)
knitr::kable(summary_a, caption = "Scenario A: Simple Phase Shifts")

#' **Observations**:
#'
#' - Affine registration with `type = "shift"` should recover the true shifts well
#' - SRVF may introduce slight nonlinear warping even though shifts are linear
#' - FDA may struggle or produce similar results to SRVF

#' ---
#'
#' # Scenario B: Smooth Elastic Deformation
#'
#' Non-affine but smooth time warping. SRVF is designed for this case;
#' FDA may struggle with default settings.
#'

#+ scenario-b-setup
set.seed(456)
n <- 15
t <- seq(0, 1, length.out = 101)

# Template: interesting shape with multiple features
template_b <- tfd(
  matrix(sin(2 * pi * t) + 0.5 * sin(4 * pi * t), nrow = 1),
  arg = t
)

# Generate smooth warping functions using GP approach (from dev-register.R)
# Warps are h: [0,1] -> [0,1], strictly monotone increasing
generate_smooth_warps <- function(n, t, severity = 1) {
  # Generate random positive functions, integrate and normalize
  gp_realizations <- tf_rgp(n, arg = t, nugget = 0.001)
  # Scale by severity and add baseline to keep positive
  positive_funcs <- exp(severity * (gp_realizations - mean(gp_realizations)))
  # Integrate to get monotone warps
  warps <- tf_integrate(positive_funcs, definite = FALSE)
  # Normalize to [0, 1]
  max_vals <- tf_fmax(warps)
  warps / max_vals
}

true_warp_b <- generate_smooth_warps(n, t, severity = 0.8)

# Apply warps to template to create warped functions
x_elastic <- tf_warp(rep(template_b, n), true_warp_b)

#+ scenario-b-plot-orig
plot(x_elastic, col = 1:n, main = "Scenario B: Smooth Elastic Deformation")
lines(template_b, lwd = 3, lty = 2)

#+ scenario-b-warp-true
plot(true_warp_b, col = 1:n, main = "True Warping Functions")
abline(a = 0, b = 1, lty = 2, col = "gray50")

#' ## Test: SRVF Registration

#+ scenario-b-srvf
warp_srvf_b <- tf_register(x_elastic, template = template_b, method = "srvf")
aligned_srvf_b <- tf_unwarp(x_elastic, warp_srvf_b)

plot_registration(
  x_elastic,
  warp_srvf_b,
  aligned_srvf_b,
  title = "SRVF",
  template = template_b
)

#' ## Test: FDA Registration

#+ scenario-b-fda
warp_fda_b <- tf_register(x_elastic, template = template_b, method = "fda")
aligned_fda_b <- tf_unwarp(x_elastic, warp_fda_b)

plot_registration(
  x_elastic,
  warp_fda_b,
  aligned_fda_b,
  title = "FDA",
  template = template_b
)

#' ## Test: Affine Registration (should fail)
#'
#' Affine registration is the wrong model here - warps are nonlinear.

#+ scenario-b-affine
warp_affine_b <- tf_register(
  x_elastic,
  method = "affine",
  template = template_b,
  type = "shift_scale"
)
aligned_affine_b <- tf_unwarp(x_elastic, warp_affine_b)

plot_registration(
  x_elastic,
  warp_affine_b,
  aligned_affine_b,
  title = "Affine (shift+scale)",
  template = template_b
)

#' ## Summary

#+ scenario-b-summary
results_b <- list(
  srvf = list(warp = warp_srvf_b, aligned = aligned_srvf_b, time = NA),
  fda = list(warp = warp_fda_b, aligned = aligned_fda_b, time = NA),
  affine = list(warp = warp_affine_b, aligned = aligned_affine_b, time = NA)
)
summary_b <- summarize_results(
  results_b,
  template = template_b,
  true_warp = true_warp_b
)
knitr::kable(summary_b, caption = "Scenario B: Smooth Elastic Deformation")

#' **Observations**:
#'
#' - SRVF should recover warps well for smooth elastic deformations
#' - FDA often needs parameter tuning (crit argument) for good results
#' - Affine registration fails because the warp model is wrong

#' ---
#'
#' # Scenario C: Sharp/Kinked Deformation with Clear Landmarks
#'
#' Piecewise linear warping with detectable peaks. Landmark registration
#' should recover exactly when landmarks are correctly identified.
#'

#+ scenario-c-setup
set.seed(789)
n <- 8
t <- seq(0, 1, length.out = 101)

# Template: function with clear peak and valley
template_c <- tfd(
  matrix(
    dnorm(t, mean = 0.3, sd = 0.1) - 0.5 * dnorm(t, mean = 0.7, sd = 0.1),
    nrow = 1
  ),
  arg = t
)

# True landmark positions: peak and valley locations vary
true_peak_locs <- runif(n, 0.2, 0.4)
true_valley_locs <- runif(n, 0.6, 0.8)

# Create functions with shifted landmarks
x_kinked <- tfd(
  t(mapply(
    function(pk, vl) {
      dnorm(t, mean = pk, sd = 0.1) - 0.5 * dnorm(t, mean = vl, sd = 0.1)
    },
    true_peak_locs,
    true_valley_locs
  )),
  arg = t
)

#+ scenario-c-plot-orig
plot(
  x_kinked,
  col = 1:n,
  main = "Scenario C: Functions with Variable Landmark Positions"
)
lines(template_c, lwd = 3, lty = 2)

#' ## Detect Landmarks

#+ scenario-c-landmarks
# Find peaks (maxima) and valleys (minima)
peaks <- tf_landmarks_extrema(x_kinked, which = "max")
valleys <- tf_landmarks_extrema(x_kinked, which = "min")

# Combine into landmark matrix
landmarks_c <- cbind(peaks, valleys)
colnames(landmarks_c) <- c("peak", "valley")
landmarks_c

#' Detected landmarks:
#+ scenario-c-landmark-compare
data.frame(
  true_peak = true_peak_locs,
  detected_peak = peaks[, 1],
  true_valley = true_valley_locs,
  detected_valley = valleys[, 1]
)

#' ## Test: Landmark Registration

#+ scenario-c-landmark
# Template landmarks (where we want to align to)
template_landmarks_c <- c(0.3, 0.7) # peak at 0.3, valley at 0.7

warp_landmark_c <- tf_register(
  x_kinked,
  method = "landmark",
  landmarks = landmarks_c,
  template_landmarks = template_landmarks_c
)
aligned_landmark_c <- tf_unwarp(x_kinked, warp_landmark_c)

plot_registration(
  x_kinked,
  warp_landmark_c,
  aligned_landmark_c,
  title = "Landmark",
  template = template_c
)

#' ## Compare with Other Methods

#+ scenario-c-srvf
warp_srvf_c <- tf_register(x_kinked, template = template_c, method = "srvf")
aligned_srvf_c <- tf_unwarp(x_kinked, warp_srvf_c)

plot_registration(
  x_kinked,
  warp_srvf_c,
  aligned_srvf_c,
  title = "SRVF",
  template = template_c
)

#' ## Summary

#+ scenario-c-summary
# Alignment quality for each method
cat(
  "Landmark alignment quality:",
  alignment_quality(aligned_landmark_c, template_c),
  "\n"
)
cat(
  "SRVF alignment quality:",
  alignment_quality(aligned_srvf_c, template_c),
  "\n"
)

#' **Observations**:
#'
#' - Landmark registration aligns features exactly when landmarks are correctly detected
#' - SRVF may smooth across landmarks, potentially misaligning sharp features
#' - Landmark method is preferred when features are clearly identifiable

#' ---
#'
#' # Scenario D: Real Data (Berkeley Growth Curves)
#'
#' Real data stress test using growth velocity curves.
#' Tests different input representations and all registration methods.
#'

#+ scenario-d-setup
# Load growth data
height <- tfd(growth$height, evaluator = tf_approx_spline)

# Compute growth velocity (derivative) - note: domain shrinks slightly
growth_velocity <- tf_derive(height)

# Get domain of derived functions (smaller than original due to finite diff)
domain_vel <- tf_domain(growth_velocity)

# Regular grid within the derivative's domain
arg_reg <- seq(domain_vel[1], domain_vel[2], length.out = 101)
# Irregular grid (variable spacing)
arg_irreg <- {
  # Create irregular spacing within domain
  n_pts <- 120
  base_grid <- seq(domain_vel[1], domain_vel[2], length.out = n_pts)
  # Add small random perturbations (keeping sorted)
  jitter <- runif(n_pts, -0.05, 0.05)
  jitter[1] <- jitter[n_pts] <- 0 # keep endpoints fixed
  sort(pmax(domain_vel[1], pmin(domain_vel[2], base_grid + jitter)))
}

# Different representations
growth_reg <- tfd(growth_velocity, arg = arg_reg)
growth_irreg <- tfd(growth_velocity, arg = arg_irreg)
growth_tfb <- tfb(height, k = 15, bs = "ps") |> tf_derive()

#+ scenario-d-plot-orig, fig.height = 5
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(growth_velocity, main = "Original grid", ylim = c(0, 25))
plot(growth_reg, main = "Regular grid (n=101)", ylim = c(0, 25))
plot(growth_irreg, main = "Irregular grid", ylim = c(0, 25))
plot(growth_tfb, main = "tfb (spline basis)", ylim = c(0, 25))
par(op)

#' ## Test: SRVF Registration on Regular Grid

#+ scenario-d-srvf
warp_growth_srvf <- tf_register(growth_reg, method = "srvf")
aligned_growth_srvf <- tf_unwarp(growth_reg, warp_growth_srvf)

plot_registration(
  growth_reg,
  warp_growth_srvf,
  aligned_growth_srvf,
  title = "Growth Data - SRVF"
)

#' ## Test: FDA Registration on Regular Grid

#+ scenario-d-fda
warp_growth_fda <- tf_register(growth_reg, method = "fda")
aligned_growth_fda <- tf_unwarp(growth_reg, warp_growth_fda)

plot_registration(
  growth_reg,
  warp_growth_fda,
  aligned_growth_fda,
  title = "Growth Data - FDA"
)

#' ## Test: Landmark Registration (using pubertal growth spurt)
#'
#' Align all curves so their pubertal growth spurt (peak velocity) occurs at the same age.

#+ scenario-d-landmark
# Find the pubertal growth spurt (maximum) - the main feature to align
growth_peaks <- tf_landmarks_extrema(growth_reg, which = "max")

# Get domain for clamping
domain_growth <- tf_domain(growth_reg)
eps <- 0.1 # buffer from domain boundaries

# For growth data, just use the peak (pubertal spurt) as the single landmark
# Ensure peaks are within domain with buffer
growth_landmarks <- matrix(
  pmax(domain_growth[1] + eps, pmin(domain_growth[2] - eps, growth_peaks[, 1])),
  ncol = 1
)
colnames(growth_landmarks) <- "peak"
cat("Peak range:", range(growth_landmarks), "\n")

# Template landmark (mean peak location)
template_landmarks_growth <- mean(growth_landmarks)
cat(
  "Mean pubertal growth spurt age:",
  round(template_landmarks_growth, 2),
  "years\n"
)

warp_growth_landmark <- tf_register(
  growth_reg,
  method = "landmark",
  landmarks = growth_landmarks,
  template_landmarks = template_landmarks_growth
)
aligned_growth_landmark <- tf_unwarp(growth_reg, warp_growth_landmark)

plot_registration(
  growth_reg,
  warp_growth_landmark,
  aligned_growth_landmark,
  title = "Growth Data - Landmark (peak)"
)

#' ## Compare Representations: tfb Input

#+ scenario-d-tfb
# Registration on tfb representation (converts internally to tfd)
warp_growth_tfb <- tf_register(growth_tfb, method = "srvf")
aligned_growth_tfb <- tf_unwarp(growth_tfb, warp_growth_tfb)

plot_registration(
  growth_tfb,
  warp_growth_tfb,
  aligned_growth_tfb,
  title = "Growth Data (tfb) - SRVF"
)

#' **Observations**:
#'
#' - SRVF generally produces good alignment for growth data
#' - Landmark registration using the pubertal growth spurt aligns that feature well
#' - Different input representations may produce slightly different results

#' ---
#'
#' # Stress Tests: Edge Cases
#'
#' Testing various edge cases to identify failure modes for simulation design.
#'

#' ## 6.1 Domain Boundaries
#'
#' Test registration when domain is not [0, 1].
#'
#' Note from dev-register.R: "not yet working for -.1 !!"

#+ stress-domain
# Domain [0, 2]
set.seed(111)
t_wide <- seq(0, 2, length.out = 101)
template_wide <- tfd(matrix(sin(pi * t_wide), nrow = 1), arg = t_wide)
x_wide <- tfd(
  t(sapply(runif(5, -0.3, 0.3), function(s) sin(pi * (t_wide + s)))),
  arg = t_wide
)

warp_wide <- tryCatch(
  tf_register(x_wide, template = template_wide, method = "srvf"),
  error = function(e) {
    message("Domain [0,2] failed: ", e$message)
    NULL
  }
)

if (!is.null(warp_wide)) {
  aligned_wide <- tf_unwarp(x_wide, warp_wide)
  plot_registration(
    x_wide,
    warp_wide,
    aligned_wide,
    title = "Domain [0, 2]",
    template = template_wide
  )
}

# Domain starting at non-zero
set.seed(112)
t_offset <- seq(1, 3, length.out = 101)
template_offset <- tfd(
  matrix(sin(pi * (t_offset - 1)), nrow = 1),
  arg = t_offset
)
x_offset <- tfd(
  t(sapply(runif(5, -0.3, 0.3), function(s) sin(pi * (t_offset - 1 + s)))),
  arg = t_offset
)

warp_offset <- tryCatch(
  tf_register(x_offset, template = template_offset, method = "srvf"),
  error = function(e) {
    message("Domain [1,3] failed: ", e$message)
    NULL
  }
)

if (!is.null(warp_offset)) {
  aligned_offset <- tf_unwarp(x_offset, warp_offset)
  plot_registration(
    x_offset,
    warp_offset,
    aligned_offset,
    title = "Domain [1, 3]",
    template = template_offset
  )
}

#' ## 6.2 Grid Density

#+ stress-grid-density
set.seed(222)
# Sparse grid (n=21)
t_sparse <- seq(0, 1, length.out = 21)
template_sparse <- tfd(matrix(sin(2 * pi * t_sparse), nrow = 1), arg = t_sparse)
x_sparse <- tfd(
  t(sapply(runif(5, -0.2, 0.2), function(s) sin(2 * pi * (t_sparse + s)))),
  arg = t_sparse
)

warp_sparse <- tryCatch(
  tf_register(x_sparse, template = template_sparse, method = "srvf"),
  error = function(e) {
    message("Sparse grid failed: ", e$message)
    NULL
  }
)

if (!is.null(warp_sparse)) {
  aligned_sparse <- tf_unwarp(x_sparse, warp_sparse)
  plot_registration(
    x_sparse,
    warp_sparse,
    aligned_sparse,
    title = "Sparse Grid (n=21)",
    template = template_sparse
  )
}

# Dense grid (n=501)
t_dense <- seq(0, 1, length.out = 501)
template_dense <- tfd(matrix(sin(2 * pi * t_dense), nrow = 1), arg = t_dense)
x_dense <- tfd(
  t(sapply(runif(5, -0.2, 0.2), function(s) sin(2 * pi * (t_dense + s)))),
  arg = t_dense
)

warp_dense <- tryCatch(
  tf_register(x_dense, template = template_dense, method = "srvf"),
  error = function(e) {
    message("Dense grid failed: ", e$message)
    NULL
  }
)

if (!is.null(warp_dense)) {
  aligned_dense <- tf_unwarp(x_dense, warp_dense)
  plot_registration(
    x_dense,
    warp_dense,
    aligned_dense,
    title = "Dense Grid (n=501)",
    template = template_dense
  )
}

#' ## 6.3 Noise Levels

#+ stress-noise
set.seed(333)
t <- seq(0, 1, length.out = 101)
template_clean <- tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
shifts <- runif(5, -0.2, 0.2)

# Low noise
x_low_noise <- tfd(
  t(sapply(
    shifts,
    function(s) sin(2 * pi * (t + s)) + rnorm(length(t), 0, 0.05)
  )),
  arg = t
)

warp_low_noise <- tf_register(
  x_low_noise,
  template = template_clean,
  method = "srvf"
)
aligned_low_noise <- tf_unwarp(x_low_noise, warp_low_noise)

plot_registration(
  x_low_noise,
  warp_low_noise,
  aligned_low_noise,
  title = "Low Noise (sd=0.05)",
  template = template_clean
)

# High noise
x_high_noise <- tfd(
  t(sapply(
    shifts,
    function(s) sin(2 * pi * (t + s)) + rnorm(length(t), 0, 0.3)
  )),
  arg = t
)

warp_high_noise <- tf_register(
  x_high_noise,
  template = template_clean,
  method = "srvf"
)
aligned_high_noise <- tf_unwarp(x_high_noise, warp_high_noise)

plot_registration(
  x_high_noise,
  warp_high_noise,
  aligned_high_noise,
  title = "High Noise (sd=0.3)",
  template = template_clean
)

#' ## 6.4 Extreme Warps

#+ stress-extreme-warps
set.seed(444)
t <- seq(0, 1, length.out = 101)
template_base <- tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)

# Near-identity warps (very mild deformation)
warp_mild <- generate_smooth_warps(5, t, severity = 0.1)
x_mild <- tf_warp(rep(template_base, 5), warp_mild)

warp_est_mild <- tf_register(x_mild, template = template_base, method = "srvf")
aligned_mild <- tf_unwarp(x_mild, warp_est_mild)

plot_registration(
  x_mild,
  warp_est_mild,
  aligned_mild,
  title = "Near-Identity Warps",
  template = template_base
)
cat("Mild warp recovery error:", warp_error(warp_est_mild, warp_mild), "\n")

# Severe warps (strong compression/expansion)
warp_severe <- generate_smooth_warps(5, t, severity = 2.0)
x_severe <- tf_warp(rep(template_base, 5), warp_severe)

warp_est_severe <- tf_register(
  x_severe,
  template = template_base,
  method = "srvf"
)
aligned_severe <- tf_unwarp(x_severe, warp_est_severe)

plot_registration(
  x_severe,
  warp_est_severe,
  aligned_severe,
  title = "Severe Warps",
  template = template_base
)
cat(
  "Severe warp recovery error:",
  warp_error(warp_est_severe, warp_severe),
  "\n"
)

#' ## 6.5 Method-Specific Edge Cases

#+ stress-srvf-constant
# SRVF: Nearly constant functions
set.seed(555)
t <- seq(0, 1, length.out = 101)
x_nearly_const <- tfd(
  t(sapply(1:5, function(i) 1 + 0.01 * sin(2 * pi * t + runif(1, 0, 2 * pi)))),
  arg = t
)

warp_const <- tryCatch(
  tf_register(x_nearly_const, method = "srvf"),
  error = function(e) {
    message("Nearly constant failed: ", e$message)
    NULL
  }
)

if (!is.null(warp_const)) {
  plot(
    x_nearly_const,
    main = "Nearly Constant Functions - Original",
    ylim = c(0.98, 1.02)
  )
  plot(warp_const, main = "Estimated Warps")
}

#+ stress-fda-crit
# FDA: crit parameter sensitivity
set.seed(556)
t <- seq(0, 1, length.out = 101)
template_fda_test <- tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)
x_fda_test <- tfd(
  t(sapply(runif(10, -0.3, 0.3), function(s) sin(2 * pi * (t + s)))),
  arg = t
)

# Default crit (usually 1e-3)
warp_fda_default <- tf_register(
  x_fda_test,
  template = template_fda_test,
  method = "fda"
)
aligned_fda_default <- tf_unwarp(x_fda_test, warp_fda_default)

# Higher crit for stronger penalization
warp_fda_crit1 <- tf_register(
  x_fda_test,
  template = template_fda_test,
  method = "fda",
  crit = 1
)
aligned_fda_crit1 <- tf_unwarp(x_fda_test, warp_fda_crit1)

op <- par(mfrow = c(2, 2))
plot(aligned_fda_default, main = "FDA (default crit)", col = 1:10)
lines(template_fda_test, lwd = 2, lty = 2)
plot(warp_fda_default, main = "Warps (default crit)")
plot(aligned_fda_crit1, main = "FDA (crit = 1)", col = 1:10)
lines(template_fda_test, lwd = 2, lty = 2)
plot(warp_fda_crit1, main = "Warps (crit = 1)")
par(op)

#+ stress-landmark-close
# Landmark: Closely spaced landmarks
set.seed(557)
t <- seq(0, 1, length.out = 201)
# Function with two close peaks
x_close_peaks <- tfd(
  t(sapply(1:5, function(i) {
    pk1 <- 0.45 + runif(1, -0.03, 0.03)
    pk2 <- 0.55 + runif(1, -0.03, 0.03)
    dnorm(t, mean = pk1, sd = 0.05) + dnorm(t, mean = pk2, sd = 0.05)
  })),
  arg = t
)

plot(x_close_peaks, main = "Functions with Closely Spaced Peaks")

# Try to detect both peaks - this is challenging
# tf_landmarks_extrema only finds the global maximum
peaks_close <- tf_landmarks_extrema(x_close_peaks, which = "max")
cat("Detected single peak locations:", peaks_close[, 1], "\n")

#+ stress-affine-bounds
# Affine: Scale bounds
set.seed(558)
t <- seq(0, 1, length.out = 101)
template_affine_test <- tfd(matrix(sin(2 * pi * t), nrow = 1), arg = t)

# Create functions that need scaling outside default bounds [0.5, 2]
x_extreme_scale <- tfd(
  t(sapply(c(0.3, 0.4, 2.5, 3.0), function(s) {
    # Compress or expand time axis
    t_warped <- pmin(pmax(s * t, 0), 1)
    sin(2 * pi * t_warped)
  })),
  arg = t
)

warp_affine_scale <- tf_register(
  x_extreme_scale,
  method = "affine",
  template = template_affine_test,
  type = "scale"
)
aligned_affine_scale <- tf_unwarp(x_extreme_scale, warp_affine_scale)

plot_registration(
  x_extreme_scale,
  warp_affine_scale,
  aligned_affine_scale,
  title = "Affine Scale (extreme)",
  template = template_affine_test
)

#' ---
#'
#' # Summary & Recommendations
#'

#' ## Method Comparison Summary
#'
#' | Scenario | Best Method | Notes |
#' |----------|-------------|-------|
#' | Simple phase shifts | Affine (shift) | Perfect recovery if model correct |
#' | Smooth elastic deformation | SRVF | Handles general warps, robust |
#' | Sharp/landmark-driven | Landmark | Exact if features detectable |
#' | Real data (growth) | SRVF | Most robust default choice |
#' | Noisy data | SRVF | More robust than FDA |
#' | Nearly constant functions | Caution | All methods may struggle |
#'
#' ## Failure Modes Identified
#'
#' 1. **Domain boundaries**: Registration works for non-[0,1] domains, but
#'    care needed with boundary clamping
#'
#' 2. **Grid density**: Sparse grids (n < 30) may have insufficient resolution
#'    for accurate warp estimation; very dense grids increase computation time
#'
#' 3. **Noise**: High noise levels degrade all methods; pre-smoothing may help
#'
#' 4. **Extreme warps**: Severe warping (strong compression/expansion) harder
#'    to recover accurately
#'
#' 5. **Nearly constant functions**: SRVF may produce unstable warps when
#'    derivatives are near zero
#'
#' 6. **FDA crit parameter**: Default may be too aggressive; `crit = 1` often
#'    produces more stable results
#'
#' 7. **Closely spaced landmarks**: `tf_landmarks_extrema()` only finds global
#'    extrema; custom landmark detection needed for multiple local features
#'
#' 8. **Affine scale bounds**: Default bounds [0.5, 2] may be too restrictive
#'    for some applications
#'
#' ## Simulation Study Recommendations
#'
#' Based on these stress tests, recommended parameter ranges for simulation:
#'
#' - **Sample sizes**: n = 20, 50, 100
#' - **Grid density**: 51, 101, 201 points
#' - **Noise levels**: sd = 0, 0.05, 0.1, 0.2
#' - **Warp severity**: mild (0.2), moderate (0.5), strong (1.0), extreme (2.0)
#' - **n_sim**: 200-500 replications for stable Monte Carlo SE
#'
#' Key metrics to track:
#'
#' - Warp recovery error (integrated squared difference from true warp)
#' - Alignment quality (cross-correlation with template)
#' - Computation time
#' - Convergence failures

#' ---
#'
#' # Session Info
#'

#+ session-info
sessionInfo()
