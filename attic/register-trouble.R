# height <- tfd(tf::growth$height)
# a little better with this:
height <- tfd(tf::growth$height, evaluator = tf_approx_spline)

arg <- tf_arg(height)
arg_reg <- seq(min(arg), max(arg), l = 101)
arg_irreg <- {
  d <- diff(arg)
  cumsum(c(0, rep(d / 4, e = 4))) + min(arg)
}

growth <- tf_derive(height)
growth_reg <- tfd(height, arg = arg_reg) |> tf_derive()
growth_irreg <- tfd(height, arg = arg_irreg) |> tf_derive()
growth_tfb <- tfb(height, k = 15, bs = "ps") |> tf_derive()

layout(matrix(1:12, 3, 4))
g_list <- list(
  orig = growth,
  reg = growth_reg,
  irreg = growth_irreg,
  spline = growth_tfb
)

for (n in names(g_list)) {
  g <- g_list[[n]]
  warp <- tf_register(g)
  aligned <- tf_unwarp(g, warp)
  plot(g, main = n, ylim = c(0, 30))
  plot(tf_invert(warp), points = FALSE)
  plot(aligned, ylim = c(0, 30))
}

for (n in names(g_list)) {
  g <- g_list[[n]]
  warp <- tf_register(g, .method = "fda")
  aligned <- tf_unwarp(g, warp)
  plot(g, main = n, ylim = c(0, 30))
  plot(tf_invert(warp), points = FALSE)
  plot(aligned, ylim = c(0, 30))
}


for (n in names(g_list)) {
  g <- g_list[[n]]
  warp <- tf_register(g, .method = "fda", crit = 1)
  aligned <- tf_unwarp(g, warp)
  plot(g, main = n, ylim = c(0, 30))
  plot(tf_invert(warp), points = FALSE)
  plot(aligned, ylim = c(0, 30))
}

##########################################
# sanity checks for  keep_new_arg:

layout(matrix(1:6, 2, 3))

#
growth_tfd <- tf_derive(growth$height)
warp <- tf_register(growth_tfd)
aligned_new <- tf_unwarp(growth_tfd, warp, keep_new_arg = TRUE)
tf_arg(aligned_new[i])
aligned_old <- tf_unwarp(growth_tfd, warp, keep_new_arg = FALSE)
tf_arg(aligned_old[i])

i <- 37
plot(growth_tfd[i], points = TRUE)
lines(aligned_new[i], col = 2)
points(aligned_new[i], col = 2)
lines(aligned_old[i], col = 3)
points(aligned_old[i], col = 3)
plot(warp[i], points = TRUE)

# with smoother tfb-derivative w/o domain shrinkage:
growth_tfb <- tfb(growth$height, k = 25) |> tf_derive()
warp <- tf_register(growth_tfb)
aligned_new <- tf_unwarp(growth_tfb, warp, keep_new_arg = TRUE)
tf_arg(aligned_new[i]) #TODO: this is 1000 long because tf_rebase for tfd_irreg tries to preserve all unique gridpoints...
aligned_old <- tf_unwarp(growth_tfb, warp, keep_new_arg = FALSE)
tf_arg(aligned_old[i])

i <- 37
plot(growth_tfb[i], points = TRUE)
lines(aligned_new[i], col = 2, n_grid = 0)
lines(aligned_old[i], col = 3, n_grid = 0)
points(aligned_old[i], col = 3, n_grid = 0)
plot(warp[i], points = TRUE)
# !! weird deviations !!

# interpolate on finer, equidistant grid AFTER derivative:
growth_fine <- tfb(growth$height, k = 8, global = TRUE) |>
  tf_derive() |>
  tfd(arg = seq(1, 18, l = 100))
warp <- tf_register(growth_fine)
aligned_new <- tf_unwarp(growth_fine, warp, keep_new_arg = TRUE)
tf_arg(aligned_new[i])
aligned_old <- tf_unwarp(growth_fine, warp, keep_new_arg = FALSE)
tf_arg(aligned_old[i])

i <- 37
plot(growth_fine[i])
lines(aligned_new[i], col = 2)
lines(aligned_old[i], col = 3)
plot(warp[i])

# interpolate on finer, equidistant grid BEFORE derivative:
growth_fine_tfd <- tfd(height, arg = seq(1, 18, l = 100)) |> tf_derive()
warp <- tf_register(growth_fine_tfd)
aligned_new <- tf_unwarp(growth_fine_tfd, warp, keep_new_arg = TRUE)
tf_arg(aligned_new[i])
aligned_old <- tf_unwarp(growth_fine_tfd, warp, keep_new_arg = FALSE)
tf_arg(aligned_old[i])

i <- 37
plot(growth_fine_tfd[i])
lines(aligned_new[i], col = 2)
lines(aligned_old[i], col = 3)
plot(warp[i])
