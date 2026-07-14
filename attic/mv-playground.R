#!/usr/bin/env Rscript
# ============================================================================
# tfd_mv / tfb_mv playground  (PR #233 -- vector-valued functional data)
# ----------------------------------------------------------------------------
# A guided tour of the new f: R -> R^d functionality. Source the whole file
# (`Rscript mv-playground.R`) for a non-stop run, or step through it line by
# line in an interactive session to poke at each piece. Plots only pop up in
# interactive sessions; under Rscript they are silently skipped.
#
# Sections:
#   1. Construction (list-of-tf, matrices, 3-d array, long data.frame, basis)
#   2. Accessors and component (re)assignment
#   3. Subsetting & bracket-evaluation ([ , component=, matrix-index)
#   4. Evaluation / interop (tf_arg, tf_evaluations, as.matrix, as.data.frame)
#   5. Arithmetic, Math, Summary, mean/sd/var
#   6. Geometry (norm, speed, inner, distance, tangent, arclength reparam)
#   7. Arc length (polyline vs derive; definite vs cumulative)
#   8. Calculus & verbs (derive, integrate, smooth, zoom, rebase)
#   9. Registration (one shared warp/curve; affine + srvf; ref_component)
#  10. Plotting (facet + trajectory)
#  11. tibble / dplyr integration
#  12. Edge cases (irregular, mixed grids, NA, empty)
# ============================================================================

devtools::load_all(
  "/home/fabians/fda/tidyfun-pkgs/tf/.worktrees/pr-233",
  quiet = TRUE
)
set.seed(1)

# tiny helper: only plot when a screen device is available
show <- function(expr) if (interactive()) print(expr)
plt <- function(code) if (interactive()) eval.parent(substitute(code))
hr <- function(title) cat("\n========== ", title, " ==========\n", sep = "")

# ============================================================================
hr("1. CONSTRUCTION")
# ============================================================================

## (a) from a named list of univariate tf vectors -- the canonical way
traj <- tfd_mv(list(x = tf_rgp(4), y = tf_rgp(4)))
traj # note the <d=2> header + per-component value ranges
tf_ncomp(traj) # 2 output dimensions

## (b) from a list of matrices (one matrix per component, [curve, arg])
t <- seq(0, 1, length.out = 50)
mx <- matrix(sin(2 * pi * outer(1:3, t)), nrow = 3)
my <- matrix(cos(2 * pi * outer(1:3, t)), nrow = 3)
traj_mat <- tfd_mv(list(x = mx, y = my), arg = t)
traj_mat

## (c) from a 3-d array [curve, arg, component]
arr <- array(
  rnorm(3 * 50 * 2),
  dim = c(3, 50, 2),
  dimnames = list(NULL, NULL, c("x", "y"))
)
traj_arr <- tfd_mv(arr, arg = t)
traj_arr

## (d) from a long data.frame (id, arg, one value column per component)
df <- data.frame(
  id = rep(1:3, each = 50),
  arg = rep(t, times = 3),
  x = as.vector(t(mx)),
  y = as.vector(t(my))
)
traj_df <- tfd_mv(df, id = "id", arg = "arg", value = c("x", "y"))
traj_df

## (e) basis representation: fit a spline basis per component
tb <- tfb_mv(traj_mat, k = 31, verbose = FALSE)
tb
## per-component basis specs via component-named lists:
tb2 <- tfb_mv(traj_mat, k = list(x = 7, y = 15), verbose = TRUE)
tb2

# ============================================================================
hr("2. ACCESSORS")
# ============================================================================

tf_ncomp(traj) # number of output dimensions d
names(tf_components(traj)) # component names
traj$x # extract a component (univariate tfd) -- via $
tf_component(traj, "y") # ... or via tf_component()
tf_component(traj, 2) # ... by index

## replace / add a component
traj$x <- traj$x * 2 # replace an existing component
traj$z <- tf_rgp(4) # add a third dimension by name
tf_ncomp(traj) # now 3
traj <- tfd_mv(list(x = traj$x, y = traj$y)) # rebuild as 2-d for what follows

# ============================================================================
hr("3. SUBSETTING & BRACKET-EVALUATION")
# ============================================================================

traj[1:2] # subset curves -> tfd_mv of length 2
length(traj[1:2])

## evaluate on a grid -> array [curve, arg, component]
ev <- traj[1, c(0.25, 0.5, 0.75)]
dim(ev)
dimnames(ev)[[3]]

## restrict to one component -> univariate result
traj[, c(0.25, 0.5, 0.75), component = "x"]

## matrix index: (curve, arg) pairs -> one row per pair, one col per component
idx <- cbind(curve = c(1, 2), arg = c(0.3, 0.6))
traj[idx]

# ============================================================================
hr("4. EVALUATION / INTEROP")
# ============================================================================

tf_arg(traj_mat) # shared regular grid -> plain numeric vector
str(tf_evaluations(traj_mat)[[1]]) # per-curve [arg x d] matrix
A <- as.matrix(traj_mat) # [curve, arg, component] array
dim(A)
head(as.data.frame(traj_mat, unnest = TRUE)) # long (id, arg, x, y)

# ============================================================================
hr("5. ARITHMETIC / MATH / SUMMARY")
# ============================================================================

(2 * traj_mat)$x # scalar * mv  (component-wise)
(traj_mat + traj_mat) # mv + mv
(-traj_mat) # unary minus
abs(traj_mat) # Math group generic, component-wise
show(mean(traj_mat)) # pointwise mean curve (length-1 tfd_mv)
sd(traj_mat)
var(traj_mat)
traj_mat == traj_mat # all TRUE
identical(traj_mat != traj_mat, rep(FALSE, 3))

# ============================================================================
hr("6. GEOMETRY")
# ============================================================================

## unit circle on [0,1]: norm == 1 everywhere, speed == 2*pi
tt <- seq(0, 1, length.out = 401)
circ <- tfd_mv(list(
  x = tfd(matrix(cos(2 * pi * tt), nrow = 1), arg = tt),
  y = tfd(matrix(sin(2 * pi * tt), nrow = 1), arg = tt)
))
range(as.matrix(tf_norm(circ))) # ~ c(1, 1)
mean(as.matrix(tf_speed(circ))) # ~ 2*pi = 6.283
tf_inner(circ, circ) # == tf_norm(circ)^2 == 1
tf_distance(traj_mat, traj_mat) # 0 everywhere
tf_tangent(circ) # unit tangent, a tfd_mv
tf_reparam_arclength(circ) # constant-speed reparam (domain [0,1]!)

# ============================================================================
hr("7. ARC LENGTH")
# ============================================================================

tf_arclength(circ) # total length ~ 2*pi
tf_arclength(circ, lower = 0, upper = 0.25) # quarter circle ~ pi/2
tf_arclength(circ, definite = FALSE) # cumulative s(t), a univariate tfd
tf_arclength(circ, method = "derive") # via tf_derive + tf_speed + integrate

# ============================================================================
hr("8. CALCULUS & VERBS")
# ============================================================================

tf_derive(traj_mat) # component-wise derivative
tf_integrate(traj_mat) # definite integral -> n x d matrix
tf_integrate(traj_mat, definite = FALSE) # indefinite -> tfd_mv
tf_smooth(traj_mat, verbose = FALSE)
tf_zoom(traj_mat, 0.2, 0.8) # restrict the time window
tf_rebase(traj_mat, tfb(traj_mat$x, verbose = FALSE)) # to a basis
as.tfd_mv(tb) # tfb_mv -> tfd_mv round-trip

# ============================================================================
hr("9. REGISTRATION  (single shared warp per curve, applied to all dims)")
# ============================================================================

## build curves with genuine per-curve phase variation shared across x and y
n <- 8
tg <- seq(0, 1, length.out = 80)
wps <- lapply(1:n, function(i) tg^runif(1, 0.7, 1.4))
mk <- function(f) tfd(do.call(rbind, lapply(wps, f)), arg = tg)
fmv <- tfd_mv(list(
  x = mk(function(s) sin(2 * pi * s)),
  y = mk(function(s) cos(2 * pi * s))
))

## affine registration (no extra deps)
reg_aff <- tf_register(fmv, method = "affine", type = "shift_scale")
tf_aligned(reg_aff) # a tfd_mv
tf_template(reg_aff) # length-1 tfd_mv template

## elastic SRVF registration (needs the {fdasrvf} Suggests)
if (requireNamespace("fdasrvf", quietly = TRUE)) {
  reg_srvf <- tf_register(fmv, method = "srvf")
  cat(
    "srvf: phase var x",
    round(mean(apply(as.matrix(fmv$x), 2, var)), 4),
    "->",
    round(mean(apply(as.matrix(tf_aligned(reg_srvf)$x), 2, var)), 4),
    "\n"
  )
  ## the warp is a SINGLE univariate function per curve, shared by all components:
  w <- tf_estimate_warps(fmv, method = "srvf")
  cat("warps are univariate:", !is_tf_mv(w), " length:", length(w), "\n")
  ## choose a different registration signal:
  tf_estimate_warps(fmv, method = "srvf", ref_component = "y") # by name
  tf_estimate_warps(fmv, method = "srvf", ref_component = "norm") # pointwise ||f||
  tf_estimate_warps(
    fmv,
    method = "srvf",
    ref_component = function(z) tf_norm(z)
  ) # custom signal
} else {
  cat("(skipping srvf -- install 'fdasrvf' to try elastic registration)\n")
}

# ============================================================================
hr("10. PLOTTING")
# ============================================================================

plt({
  # for d == 2, the default is "trajectory" (y(t) vs x(t), the movement view);
  # per-curve graphical params (col, lty, lwd, ...) are recycled across curves
  plot(fmv, col = 1:length(fmv))
  lines(circ, col = "red", lwd = 2) # add another trajectory on top
  plot(fmv, type = "facet", col = 1:length(fmv)) # one panel per component
})

# ============================================================================
hr("11. tibble / dplyr INTEGRATION")
# ============================================================================

if (
  requireNamespace("tibble", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)
) {
  tbl <- tibble::tibble(
    id = 1:n,
    grp = rep(c("A", "B"), length.out = n),
    path = fmv
  )
  print(tbl)
  tbl |>
    dplyr::mutate(len = tf_arclength(path), speed = tf_speed(path)) |>
    dplyr::group_by(grp) |>
    dplyr::summarize(mean_path = mean(path), mean_len = mean(len)) |>
    print()
} else {
  cat("(skipping tibble/dplyr demo -- install 'tibble' and 'dplyr')\n")
}

# ============================================================================
hr("12. EDGE CASES")
# ============================================================================

## components on DIFFERENT argument grids (gappy movement data)
mixed <- tfd_mv(list(
  x = tfd(matrix(rnorm(2 * 10), 2), arg = seq(0, 1, length.out = 10)),
  y = tfd(matrix(rnorm(2 * 25), 2), arg = seq(0, 1, length.out = 25))
))
mixed
tf_arclength(mixed) # polyline copes with per-component grids
str(tf_arg(mixed)) # returns per-component args when they differ

## irregular components
irr <- tfd_mv(list(
  x = tfd(list(c(0, 0.5, 1)), list(c(1, 2, 3))),
  y = tfd(list(c(0, 0.5, 1)), list(c(4, 5, 6)))
))
irr

## NA propagation: an NA in any component marks the curve NA
withna <- traj_mat
withna$x <- withna$x * c(1, NA_real_, 1) # blank out the 2nd curve of comp x
is.na(withna) # -> FALSE TRUE FALSE (whole curve NA)

## empty / prototype
empty <- tfd_mv(list())
length(empty)
tf_ncomp(empty)

hr("DONE -- happy exploring")
