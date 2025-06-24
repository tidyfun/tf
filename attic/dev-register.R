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
