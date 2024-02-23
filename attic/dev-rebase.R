devtools::load_all(".")
# things it should do:
# tf_rebase(object, basis_from = object) == object
#
set.seed(11331)
x <- tf_rgp(5,  arg = 301L) |> tf_smooth() |>
  tfd(evaluator = tf_approx_fill_extend)

l <- list(
  x_sp = tf_sparsify(x, dropout = .1),
  x_ir = tf_sparsify(x, dropout = .1) |> tf_jiggle(amount = .2),
  b = tfb(x, k = 45),
  b2 = tfb(x, k = 15, bs = "tp", sp= .1),
  bu = tfb(x, k = 15, penalized = FALSE),
  bg = tfb(x, k = 5, global = TRUE),
  fp = tfb_fpc(x, pve = 1),
  fp_low = tfb_fpc(x, pve = .95)
)
for (i in seq_along(l)) {
  # cat(names(l)[i],":\n")
  #expect_identical(
  # tf_rebase(l[[i]], l[[i]]),
  #            l[[i]])

  expect_equal(
    tf_rebase(x, l[[i]]) |> tf_evaluations(),
    l[[i]] |> tf_evaluations(),
    tolerance = .01
  )
  expect_equal(
    tf_rebase(x, l[[i]]) |> tf_arg(),
    l[[i]] |> tf_arg(),
  )
}


basis_from <-  tfb_fpc(x, pve = 1)
object <- x
tf_rebase(x, tfb_fpc(x, pve = 1))

bb <-tf_rebase(x, l$b)
bb == tf_rebase(x, bb) #!!

identical(tf_arg(tf_rebase(x, l$x_ir)), tf_arg(l$x_ir))

object <- x
basis_from <- l$b2

bb2 <- tf_rebase(x, l$b2)
bb2 ==  l$b2


exp <- exp(x)

b <- tfb(raw, penalized = FALSE)
b2 <- tfb(exp(raw), sp = .01, family = "Gamma")
pc <- tfb_fpc(raw)

  #TODO: irregulars!
}
