test_that("plot.tf(points = TRUE) passes col (not 'ol') to matlines (#248)", {
  x <- tf_rgp(3)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  # Capture the call matlines() receives. Use an env in globalenv() so the
  # tracer (which runs in the traced function's frame) can reach it.
  capture_env <- new.env(parent = emptyenv())
  assign(".tf_matlines_call", capture_env, envir = globalenv())
  on.exit(rm(".tf_matlines_call", envir = globalenv()), add = TRUE)

  trace(
    graphics::matlines,
    tracer = quote(
      assign(
        "call",
        match.call(),
        envir = get(".tf_matlines_call", envir = globalenv())
      )
    ),
    print = FALSE
  )
  on.exit(suppressMessages(untrace(graphics::matlines)), add = TRUE)

  plot(x, points = TRUE)

  arg_names <- names(as.list(capture_env$call)[-1])
  expect_true("col" %in% arg_names)
  expect_false("ol" %in% arg_names)
})

test_that("plot.tf smoke test for basic types", {
  x <- tf_rgp(3)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(x))
  expect_no_error(plot(x, points = TRUE))
  expect_no_error(plot(x, type = "lasagna"))
  expect_no_error(lines(x))
})
