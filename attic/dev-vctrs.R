x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend) |> suppressMessages()
names(x) <- letters[1:5]
l <- list(
  x = x,
  x_short = x |> tf_zoom(0.1, 0.4),
  x_short_longdom = tfd(x |> tf_zoom(0.1, 0.4), domain = tf_domain(x),
                    evaluator = tf_approx_linear),
  x_sp = tf_sparsify(x, dropout = .1),
  x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
  x_fake_ir = as.tfd_irreg(x |> tf_zoom(0.1, 0.4)),
  b = tfb(x, k = 45, verbose = FALSE),
  b2 = tfb(x, k = 15, bs = "tp", sp= .1, verbose = FALSE),
  bu = tfb(x, k = 15, penalized = FALSE, verbose = FALSE),
  bg = tfb(x, k = 5, global = TRUE, verbose = FALSE),
  fp = tfb_fpc(x, pve = 1),
  fp_low = tfb_fpc(x, pve = .95)
)

vec_ptype2(l$x, l$x_short)
vec_ptype2(l$x, l$x_ir)
vec_ptype2(l$x_ir, l$x)
vec_ptype2(l$x_ir, l$b)
vec_ptype2(l$b2, l$b)
