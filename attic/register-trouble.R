# height <- tfd(tf::growth$height)
# a little better with this:
height <- tfd(tf::growth$height, evaluator = tf_approx_spline)

arg <- tf_arg(height)
arg_reg <- seq(min(arg), max(arg), l = 101)
arg_irreg <- {
  d <- diff(arg1)
  cumsum(c(0, rep(d / 4, e = 4))) + min(arg)
}

growth <- tf_derive(height)
growth_reg <- tfd(height, arg = arg_reg) |> tf_derive()
growth_irreg <- tfd(height, arg = arg_irreg) |> tf_derive()
growth_tfb <- tfb(height, k = 25) |> tf_derive()

layout(matrix(1:12, 3, 4))
for (g in list(growth, growth_reg, growth_irreg, growth_tfb)) {
  warp <- tf_register(g)
  aligned <- tf_unwarp(g, warp)
  plot(g)
  plot(warp)
  plot(aligned)
}
