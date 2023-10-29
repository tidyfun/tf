context("vctrs")

x <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
y <- tf_rgp(2, arg = seq(0, 1, length.out = 21))
x_irr <- tf_jiggle(x) 

test_that("concatenation behaves for tfd", {
  expect_warning(c(x, y), "different grids")
  expect_warning(c(x, y), "different resolutions")
  expect_is(suppressWarnings(c(x, x_irr)), "tfd_irreg")
  expect_is(c(x, x), "tfd_reg")
  expect_error(c(x, tfb(x)))
  expect_numeric(suppressWarnings(tf_evaluate(c(x, y)[1])[[1]]))
})


tfb_k10 = x %>% tfb(k = 10)
tfb_k20 = x_irr %>% tfb(k = 20)
z = tf_rgp(10, arg = seq(0, 1, length.out = 11)) %>% tfb_fpc()
z2 = z %>% tfb_fpc(npc = 10)

test_that("concatenation behaves for tfb", {
  expect_is(c(tfb_k10[1], tfb_k10[2]), "tfb_spline")
  expect_is(c(tfb(tfb_k10[1]), tfb(tfb_k10[2])), "tfb_spline")
  expect_is(c(z, z), "tfb_fpc")
  
  expect_warning(c(tfb_k10, tfb_k20))
  expect_error(c(tfb_k10, tfb_fpc(z)))
  expect_error(c(z, tfb_k10))
  expect_error(c(z, z2))
})
