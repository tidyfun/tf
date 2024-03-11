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

for (i in seq_along(l)) {
  testthat::expect_identical(l[[i]],
                             vec_restore(vec_proxy(l[[i]]), vec_ptype(l[[i]])))
  testthat::expect_identical(vec_ptype2(l[[i]], l[[i]]),
                             vec_ptype(l[[i]]))
  testthat::expect_identical(l[[i]],
                             c(l[[i]], l[[i]])[1:length(l[[i]])])
}


sp <- tf_rgp(5) |> tf_sparsify()
x <- tf_rgp(5)
y <- x |> tf_zoom(.2, .4)
b <- tfb(x)
b2 <- tfb(x, arg = seq(0, 1, l = 21))
xx <- tfd(1:10, arg  = 1:10)

x[1] <- y[2] #no
x[1] <- b[2] #yes
x[1] <- b2[2] #no
x[1] <- xx #no

b[2] <- x[1]
