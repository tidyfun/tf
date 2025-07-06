devtools::load_all()
set.seed(1312)
min_t <- 0 # not yet working for -.1 !!
max_t <- 2 # sollte auch alles mit max_t != 1 funktionieren (... und min_t != 0)

# reguläres gitter
t <- seq(min_t, max_t, l = 101)
# template function
f0 <- tfd(sin(t * pi), t)

# generate random warping functions proportional to \int exp(x(t)) dt
w <- (tf_rgp(10, arg = t) + rnorm(10)) |>
  exp() |>
  tf_integrate(definite = FALSE)
w <- w / tf_fmax(w) * (max_t - min_t)
plot(w)

(warped <- tf_warp(x = rep(f0, 10), warp = w)) |> plot()
tf_warp(x = rep(f0, 10), warp = w, keep_arg = TRUE) |> plot()

tf_unwarp(x = warped, warp = w) |> plot()

# WORKS :)

# registering against Karcher mean
w_est <- tf_register(warped, .method = "srvf") |> plot()

layout(t(1:2))
plot(w, col = 1:10)
plot(w_est, col = 1:10)
layout(1)

tf_unwarp(x = warped, warp = w_est) |> plot(col = 1:10)
lines(f0, col = 2)


#WORKS :)

# tf_register against given template
w_est2 <- tf_register(warped, .template = f0)

layout(t(1:2))
plot(w, col = 1:10)
plot(w_est2, col = 1:10)

layout(1)
tf_unwarp(x = warped, warp = w_est2) |> plot(col = 1:10)

#  WORKS :)

w_est3 <- tf_register(warped, .method = "fda")
layout(t(1:2))
plot(w, col = 1:10)
plot(w_est3, col = 1:10) # :(
layout(1)

tf_unwarp(x = warped, warp = w_est3) |> plot(col = 1:10)
# WORKS (badly!)

w_est4 <- tf_register(warped, .template = f0, .method = "fda")

layout(t(1:2))
plot(w, col = 1:10)
plot(w_est4, col = 1:10)
layout(1)

tf_unwarp(x = warped, warp = w_est4) |> plot(col = 1:10)

# WORKS (even more badly!)

# handrolled landmark registration

maxima <- tf_where(f = warped, cond = value == max(value), "first")
zero_cross <- tf_where(
  f = tf_zoom(warped, tf_arg(warped)[10], tf_arg(warped)[90]),
  cond = abs(value) == min(abs(value)),
  "first"
)
minima <- tf_where(f = warped, cond = value == min(value), "first")

landmark_warps <- cbind(0, maxima, zero_cross, minima, 2) |>
  tfd(arg = c(0, .5, 1, 1.5, 2))

layout(t(1:2))
plot(w, col = 1:10)
plot(landmark_warps, col = 1:10)
layout(1)

tf_unwarp(x = warped, warp = landmark_warps) |> plot()

### sanity check for fdasrvf time warping

set.seed(123)

x <- seq(-3, 3, length.out = 51)
curves <- sapply(1:20, function(i) {
  phase_shift <- runif(1, -1, 1)
  amplitude <- runif(1, 0.8, 1.2)
  baseline <- runif(1, 0, 0.2)
  amplitude * sin(x + phase_shift) + baseline
})

res <- fdasrvf::time_warping(curves, time = x)
tab <- list2DF(list(
  x = as.tfd(t(curves)),
  reg = as.tfd(t(res$fn)),
  warp = as.tfd(t(res$warping_functions))
))
tab$tf_warp <- tf_register(tab$x)
tab$tf_reg <- tf_unwarp(tab$x, warp = tab$tf_warp)

# looks the same
layout(t(1:2))
plot(tab$warp)
plot(tab$tf_warp)

# looks the same
plot(tab$reg)
plot(tab$tf_reg)
