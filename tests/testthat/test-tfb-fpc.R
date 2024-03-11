source(system.file("testdata", "make-test-data.R", package = "tf"))

# check constructors from tfd, matrix, data.frame, list
test_that("fpc_wsvd works for smooth equidistant data", {
  expect_type(
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 1.0), "list"
  )
  expect_lt(
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 0.5)$npc,
    fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 0.9)$npc
  )
  fpc_smoo <- fpc_wsvd(smoo_matrix, attr(smoo_matrix, "arg"), pve = 1.0)
  expect_equal(fpc_smoo$npc, length(smoo) - 1)
  expect_equal(fpc_smoo$mu, tf_evaluations(mean(smoo))[[1]], ignore_attr = TRUE)
  # check orthonormality for equidistant:
  fpc_smoo <- tfd(t(fpc_smoo$efunctions), arg = attr(smoo_matrix, "arg"))
  comb <- expand.grid(seq_along(fpc_smoo), seq_along(fpc_smoo))
  expect_true(
    every(seq_len(nrow(comb)), function(i) {
      res <- tf_integrate(fpc_smoo[comb[i, 1]] * fpc_smoo[comb[i, 2]]) |>
        round(digits = 4)
      res %in% c(0, 1)
    })
  )
})

test_that("fpc_wsvd works for smooth non-equidistant data", {
  smoo_arg <- (2 * qbeta(attr(smoo_matrix, "arg"), 0.5, 0.8) + 1)^3
  fpc_smoo <- fpc_wsvd(smoo_matrix, smoo_arg, pve = 1.0)
  expect_equal(fpc_smoo$npc, length(smoo) - 1)
  expect_equal(fpc_smoo$mu, tf_evaluations(mean(smoo))[[1]], ignore_attr = TRUE)
  # check orthonormality for non-equidistant:
  fpc_smoo <- tfd(t(fpc_smoo$efunctions), arg = smoo_arg)
  comb <- expand.grid(seq_along(fpc_smoo), seq_along(fpc_smoo))
  expect_true(
    every(seq_len(nrow(comb)), function(i) {
      res <- tf_integrate(fpc_smoo[comb[i, 1]] * fpc_smoo[comb[i, 2]]) |>
        round(digits = 4)
      res %in% c(0, 1)
    })
  )
})

test_that("fpc_wsvd works for partially missing data", {
  expect_s3_class(tfb_fpc(sparse), "tfb_fpc") |> suppressWarnings() |>
    suppressMessages()
  expect_warning(tfb_fpc(sparse), "High <pve>") |> suppressMessages()
  expect_message(tfb_fpc(sparse), "Using softImpute") |> suppressWarnings()
  set.seed(1312)
  x <- tf_rgp(50)
  x_sp_pc <- x |> tf_sparsify(.02) |> tfb_fpc(pve = .98) |>
    suppressMessages() |> suppressWarnings()
  expect_equal(
    as.matrix(x_sp_pc, arg = tf_arg(x)),
    as.matrix(x, arg = tf_arg(x)),
    tolerance = .1)
})

test_that("tfb_fpc defaults work for all kinds of regular input", {
  expect_s3_class(tfb_fpc(smoo), "tfb_fpc")
  expect_length(tfb_fpc(smoo), length(smoo))
  expect_equal(
    tf_evaluations(tfb_fpc(smoo, pve = 0.9999)), tf_evaluations(smoo),
    tolerance = 1e-1, ignore_attr = TRUE
  )
  for (smoo_ in list(tfb_fpc(smoo_matrix, pve = 0.9999), tfb_fpc(smoo_df))) {
    expect_s3_class(smoo_, "tfb_fpc")
    expect_length(smoo_, length(smoo))
    expect_equal(tf_evaluations(smoo_), tf_evaluations(smoo),
      tolerance = 1e-1, ignore_attr = TRUE
    )
  }
})
