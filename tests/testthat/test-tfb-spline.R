source(system.file("testdata", "make-test-data.R", package = "tf"))

# check constructors from tfd, matrix, data.frame, list
test_that("tfb_spline defaults work for all kinds of regular input", {
  expect_s3_class(tfb_spline(smoo, verbose = FALSE), "tfb_spline")
  expect_message(tfb_spline(smoo), "100")
  expect_length(tfb_spline(smoo, verbose = FALSE), length(smoo))
  expect_equal(
    tf_evaluations(tfb_spline(smoo, verbose = FALSE)), tf_evaluations(smoo),
    tolerance = 1e-3
  )
  for (dat in list(smoo_list, smoo_matrix, smoo_df)) {
    smoo_ <- try(tfb_spline(dat, verbose = FALSE))
    expect_s3_class(smoo_, "tfb_spline")
    expect_length(smoo_, length(smoo))
    expect_equal(
      tf_evaluations(smoo_), tf_evaluations(smoo),
      tolerance = 1e-3, ignore_attr = TRUE
    )
  }
})

test_that("tfb_spline defaults work for all kinds of irregular input", {
  expect_s3_class(tfb_spline(irr, verbose = FALSE), "tfb_spline")
  expect_message(tfb_spline(irr), "100")
  expect_length(tfb_spline(irr, verbose = FALSE), length(irr))
  expect_message(tfb_spline(irr_df), "100")

  irr_tfb_ <- tfb_spline(irr_list, arg = tf_arg(irr), verbose = FALSE)
  expect_s3_class(irr_tfb_, "tfb_spline")
  expect_length(irr_tfb_, length(irr))
  expect_equal(
    tf_evaluate(irr_tfb_, tf_arg(irr)), tf_evaluations(irr),
    tolerance = 1e-1
  )

  for (dat in list(irr_matrix, irr_df)) {
    irr_tfb_ <- tfb_spline(dat, verbose = FALSE)
    expect_s3_class(irr_tfb_, "tfb_spline")
    expect_length(irr_tfb_, length(irr))
    expect_equal(
      tf_evaluate(irr_tfb_, tf_arg(irr)), tf_evaluations(irr),
      tolerance = 1e-1, ignore_attr = TRUE
    )
  }
})

test_that("unpenalized tfb_spline works", {
  expect_error(tfb_spline(narrow, k = 11, penalized = FALSE, verbose = FALSE), "reduce k")
  expect_s3_class(
    tfb_spline(narrow, k = 8, penalized = FALSE, verbose = FALSE), "tfb_spline"
  )
  expect_s3_class(
    tfb_spline(rough, k = 15, penalized = FALSE, verbose = FALSE), "tfb_spline"
  )

  expect_s3_class(
    tfb_spline(exp(smoo),
      family = Gamma(link = "log"), penalized = FALSE, verbose = FALSE
    ),
    "tfb_spline"
  )
  expect_s3_class(
      tfb_spline(narrow^3,
        family = scat(), k = 5, penalized = FALSE, verbose = FALSE) |>
        suppressWarnings() |> suppressMessages(),
      "tfb_spline"
  )

  expect_equal(
    tfb_spline(irr, k = 11, penalized = FALSE, verbose = FALSE),
    tfb_spline(irr, k = 11, verbose = FALSE),
    tolerance = 1e-1, ignore_attr = TRUE
  )

  # GLM case: fitting on exp-scale and transforming back:
  expect_equal(
    tfb_spline(exp(smoo),
      family = gaussian(link = "log"),
      penalized = FALSE, verbose = FALSE
    ) |>
      log() |>
      as.matrix(),
    as.matrix(smoo),
    tolerance = 0.001
  )

  expect_message(
    try(
      tfb_spline(smoo[1], family = Gamma(link = "log"), penalized = FALSE),
      silent = TRUE
    ),
    "non-positive"
  )
  expect_error(
    suppressMessages(
      tfb_spline(smoo[1], family = Gamma(link = "log"), penalized = FALSE)
    ),
    "Basis representation failed"
  )

  approx_penalized <- abs(rough - tfd(tfb(rough, k = 40, verbose = FALSE))) |>
    as.matrix() |>
    sum()
  approx_unpenalized <- abs(
    rough - tfd(tfb(rough, k = 40, penalized = FALSE, verbose = FALSE))
  ) |>
    as.matrix() |>
    sum()
  expect_gt(approx_penalized, approx_unpenalized)
})

test_that("mgcv spline basis options work", {
  for (bs in c("tp", "ds", "gp", "ps")) {
    smoo_ <- try(tfb_spline(smoo, k = 21, bs = bs, verbose = FALSE))
    expect_s3_class(smoo_, "tfb_spline")
    expect_equal(tf_evaluations(smoo_), tf_evaluations(smoo),
      tolerance = 1e-2
    )
    smoo_spec <- environment(attr(smoo_, "basis"))$spec
    expect_equal(smoo_spec$bs.dim, 21)
    expect_s3_class(
      smoo_spec,
      class(smooth.construct(
        s(x, bs = bs),
        data = list(x = 1:40), knots = NULL
      ))
    )
  }
})


test_that("global and pre-specified smoothing options work", {
  rough_global <- try(tfb(rough, global = TRUE, k = 51, verbose = FALSE))
  expect_s3_class(rough_global, "tfb")
  expect_gt(
    system.time(
      tfb(c(rough, rough, rough), k = 51, verbose = FALSE)
    )["elapsed"],
      system.time(
        tfb(c(rough, rough, rough), k = 51, global = TRUE, verbose = FALSE)
      )["elapsed"]
  )

  expect_equal(
    tfb(rough, sp = 1e-15, k = 51, verbose = FALSE) |> tf_evaluations(),
    tfb(rough, penalized = FALSE, k = 51, verbose = FALSE) |> tf_evaluations()
  )
  expect_equal(
    tfb(rough, sp = 0.2, k = 75, verbose = FALSE) |> tf_evaluations() |>
      suppressMessages() |> suppressWarnings(),
    tfb(rough, sp = 0.2, k = 10, verbose = FALSE) |> tf_evaluations() |>
      suppressMessages() |> suppressWarnings(),
    tolerance = 1e-2
  )

  expect_equal(
    tfb(exp(rough),
      sp = 1e-15, k = 51, family = gaussian(link = "log"),
      verbose = FALSE
    ) |>
      tf_evaluations(),
    tfb(exp(rough),
      penalized = FALSE, k = 51, family = gaussian(link = "log"),
      verbose = FALSE
    ) |>
      tf_evaluations(),
    tolerance = 1e-3
  )
  expect_equal(
    tfb(exp(rough),
      sp = 0.2, k = 75, family = gaussian(link = "log"), verbose = FALSE
    ) |> tf_evaluations() |>
      suppressMessages() |> suppressWarnings(),
    tfb(exp(rough),
      sp = 0.2, k = 10, family = gaussian(link = "log"), verbose = FALSE
    ) |> tf_evaluations() |>
      suppressMessages() |> suppressWarnings(),
    tolerance = 1e-2
  )
})
