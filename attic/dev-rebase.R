
# things it should do:
# tf_rebase(object, basis_from = object) == object
#
set.seed(11331)
x <- tf_rgp(5,  arg = 201L) |> tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend)

l <- list(
  b = tfb(x, k = 45),
  b2 = tfb(x, k = 15, bs = "tp", sp= .1),
  bu = tfb(x, k = 15, penalized = FALSE),
  bg = tfb(x, k = 5, global = TRUE),
  fp = tfb_fpc(x, pve = 1),
  fp = tfb_fpc(x, pve = .95),
  x_sp = tf_sparsify(x),
  x_ir = tf_sparsify(x) |> tf_jiggle()
)

tf_rebase(x, l$x_sp) == l$x_sp
identical(tf_arg(tf_rebase(x, l$x_ir)), tf_arg(l$x_ir))

object <- x
basis_from <- l$b2

tf_rebase(x, l$b) == l$b
tf_rebase(x, l$b2) #!! NOO


exp <- exp(x)


<- tf_rgp(3, arg = 101L) |> tfd(evaluator = tf_approx_spline)

|> tf_jiggle()
tf_rebase(raw, v)


b <- tfb(raw, penalized = FALSE)
b2 <- tfb(exp(raw), sp = .01, family = "Gamma")
pc <- tfb_fpc(raw)

  #TODO: irregulars!
}
