x <- tf_rgp(2, arg = seq(0, 1, length.out = 11))
y <- tf_rgp(2, arg = seq(0, 1, length.out = 21))
x_irr <- tf_jiggle(x)
tfb_k10 <- x |> tfb(k = 10, verbose = FALSE)
tfb_k20 <- x_irr |> tfb(k = 20, verbose = FALSE)
z <- tf_rgp(10, arg = seq(0, 1, length.out = 11)) |> tfb_fpc()
z2 <- z |> tfb_fpc(npc = 10)

test_that("concatenation behaves for tfd", {
  suppressWarnings(expect_warning(c(x, y), "different grids"))
  expect_error(c(x, tfb(x)))

  expect_s3_class(suppressWarnings(c(x, x_irr)), "tfd_irreg")
  expect_s3_class(c(x, x), "tfd_reg")

  expect_numeric(suppressWarnings(tf_evaluate(c(x, y)[1])[[1]]))
})

test_that("concatenation behaves for tfb", {
  expect_s3_class(c(tfb_k10[1], tfb_k10[2]), "tfb_spline")
  expect_s3_class(c(tfb(tfb_k10[1]), tfb(tfb_k10[2])), "tfb_spline")
  expect_s3_class(c(z, z), "tfb_fpc")

  suppressWarnings(expect_warning(c(tfb_k10, tfb_k20)))
  expect_error(c(tfb_k10, tfb_fpc(z)))
  expect_error(c(z, tfb_k10))
  expect_error(c(z, z2))
})
