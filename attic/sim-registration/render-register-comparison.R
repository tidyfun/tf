devtools::load_all()
source(file.path("attic", "sim-registration", "reference-problems.R"))

build_fda_registration <- function(problem, iterlim = 10L, crit = 2L) {
  arg <- tf_arg(problem$x)
  yfd <- tf_2_fd(problem$x)
  y0fd <- tf_2_fd(problem$template)
  WfdParobj <- registration_problem_wfd_par(problem)
  fit <- fda::register.fd(
    y0fd = y0fd,
    yfd = yfd,
    WfdParobj = WfdParobj,
    iterlim = iterlim,
    dbglev = 0,
    crit = crit
  )

  forward_warps <- tfd(
    t(registration_fit_forward_warp_matrix(fit, arg = arg)),
    arg = arg,
    domain = tf_domain(problem$x)
  )
  aligned <- tfd(
    t(fda::eval.fd(arg, fit$regfd)),
    arg = arg,
    domain = tf_domain(problem$x)
  )
  template <- tfd(
    matrix(fda::eval.fd(arg, fit$y0fd), nrow = 1),
    arg = arg,
    domain = tf_domain(problem$x)
  )

  tf:::new_tf_registration(
    registered = aligned,
    inv_warps = tf_invert(forward_warps),
    template = template,
    x = problem$x,
    call = quote(
      fda::register.fd(
        y0fd = tf_2_fd(template),
        yfd = tf_2_fd(x),
        WfdParobj = bspline_warp_par(nbasis = 6, lambda = 0),
        iterlim = 10,
        dbglev = 0,
        crit = 2
      )
    )
  )
}

build_tf_registration <- function(problem, iterlim = 10L, crit = 2L) {
  tf_register(
    problem$x,
    method = "fda",
    template = problem$template,
    nbasis = problem$warp_spec$basis$nbasis,
    lambda = problem$warp_spec$lambda,
    max_iter = 1,
    iterlim = iterlim,
    crit = crit
  )
}

plot_registration_row <- function(registration, label, call_text) {
  has_orig <- !is.null(registration$x)

  if (has_orig) {
    plot(registration$x, main = "Original", ylab = "x(t)", xlab = "t")
    if (!is.null(registration$template)) {
      graphics::lines(registration$template, lwd = 2, lty = 2)
    }
    graphics::mtext(
      text = paste(label, call_text, sep = "\n"),
      side = 3,
      line = 1,
      adj = 0,
      cex = 0.75
    )
  }

  plot(
    registration$inv_warps,
    main = "Inverse warps",
    xlab = "Observed time t",
    ylab = "Aligned time s",
    points = FALSE
  )
  graphics::abline(a = 0, b = 1, lty = 2, col = "grey40")

  plot(
    registration$registered,
    main = "Aligned",
    xlab = "Aligned time s",
    ylab = "x(s)",
    points = FALSE
  )
  if (!is.null(registration$template)) {
    graphics::lines(registration$template, lwd = 2, lty = 2)
  }
}

plot_one_comparison <- function(problem, tf_reg, fda_reg) {
  old_par <- graphics::par(
    mfrow = c(2, 3),
    oma = c(0, 0, 4, 0),
    mar = c(4, 4, 3, 1)
  )
  on.exit(graphics::par(old_par), add = TRUE)

  plot_registration_row(
    registration = fda_reg,
    label = paste("Upstream fda |", problem$name),
    call_text = "WfdParobj: bspline(nbasis = 6, norder = 4, lambda = 0) | iterlim = 10 | crit = 2"
  )
  plot_registration_row(
    registration = tf_reg,
    label = paste("tf backend |", problem$name),
    call_text = "nbasis = 6 | lambda = 0 | iterlim = 10 | crit = 2"
  )

  graphics::mtext(
    text = paste0(
      "Registration Comparison: ",
      problem$name,
      " | n = ",
      length(problem$x),
      " | grid = ",
      length(tf_arg(problem$x))
    ),
    side = 3,
    outer = TRUE,
    line = 1,
    cex = 1
  )
}

output_path <- file.path(
  "attic",
  "sim-registration",
  "register-comparison-side-by-side.pdf"
)

problems <- registration_reference_problems()[c(
  "easy_phase",
  "amplitude_phase",
  "real_gait"
)]

grDevices::pdf(
  file = output_path,
  width = 14,
  height = 10,
  onefile = TRUE
)

for (problem in problems) {
  tf_reg <- build_tf_registration(problem)
  fda_reg <- build_fda_registration(problem)
  plot_one_comparison(problem, tf_reg = tf_reg, fda_reg = fda_reg)
}

grDevices::dev.off()

cat(normalizePath(output_path), "\n")
