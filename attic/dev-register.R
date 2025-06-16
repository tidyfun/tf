devtools::load_all("~/lehre/bachelor/MaxMücke-tf-registration/tf/")

max_t <- 2 # sollte auch alles mit max_t != 1 funktionieren (... und min_t != 0)

# reguläres gitter
t <- seq(0, max_t, l = 101)
# template function
f0 <- tfd(sin(t * pi), t)

# generate random warping functions proportional to \int exp(x(t)) dt
w <- (tf_rgp(10, t) + rnorm(10)) |> exp() |> tf_integrate(definite = FALSE)
w <- w / tf_fmax(w) * max_t
plot(w)


#warping not working yet -- tf_warp just re-evaluates same function on new grids:
(warped <- tf_warp(x = rep(f0, 10), warp = w)) |> plot()
tf_warp(x = rep(f0, 10), warp = w, keep_arg = TRUE) |> plot()

tf_unwarp(x = warped, warp = w) |> plot()

# WORKS :)

# registering against Karcher mean
w_est <- tf_register_template(warped) |> plot()

layout(t(1:2))
plot(w, col = 1:10)
plot(w_est, col = 1:10)
layout(1)

tf_unwarp(x = warped, warp = w_est) |> plot()
lines(f0, col = 2)


#WORKS :)

# tf_register against given template
w_est2 <- tf_register_template(warped, template = f0)

layout(t(1:2))
plot(w, col = 1:10)
plot(w_est2, col = 1:10) #!! returns aligning functions not warping functions !!

tf_unwarp(x = warped, warp = w_est2) |> plot()
tf_warp(x = warped, warp = w_est2) |> plot()
layout(1)

# (mostly) WORKS :)

w_est3 <- tf_register_template(warped, method = "fda")
layout(t(1:2))
plot(w)
plot(w_est3) # :(

# tf_unwarp(x = warped, warp = w_est3) |> plot()
# tf_warp(x = warped, warp = w_est3) |> plot()

w_est4 <- tf_register_template(warped, template = f0, method = "fda")

plot(w)
plot(w_est4) # :(

# tf_unwarp(x = warped, warp = w_est4) |> plot()
# tf_warp(x = warped, warp = w_est4) |> plot()
