devtools::load_all()
source(file.path("attic", "sim-registration", "reference-problems.R"))

crit_value_tfd <- function(template, aligned, crit = 2L) {
  arg <- tf_arg(aligned)
  template_mat <- as.matrix(suppressWarnings(tfd(template, arg = arg)))
  aligned_mat <- as.matrix(suppressWarnings(tfd(aligned, arg = arg)))

  if (nrow(template_mat) == 1L) {
    template_mat <- template_mat[rep(1L, nrow(aligned_mat)), , drop = FALSE]
  }

  total <- 0
  for (i in seq_len(nrow(aligned_mat))) {
    y0 <- template_mat[min(i, nrow(template_mat)), ]
    y1 <- aligned_mat[i, ]
    aa <- mean(y0^2)
    bb <- mean(y0 * y1)
    cc <- mean(y1^2)
    if (crit == 1L) {
      total <- total + aa - 2 * bb + cc
    } else {
      total <- total + aa + cc - sqrt((aa - cc)^2 + 4 * bb^2)
    }
  }
  total
}

add_white_noise <- function(x, snr = 10, seed = 1L) {
  set.seed(seed)
  x_mat <- as.matrix(x)
  noisy_mat <- t(vapply(
    seq_len(nrow(x_mat)),
    \(i) {
      signal <- x_mat[i, ]
      signal_power <- mean(signal^2)
      noise_sd <- sqrt(signal_power / snr)
      signal + stats::rnorm(length(signal), sd = noise_sd)
    },
    numeric(ncol(x_mat))
  ))
  tfd(noisy_mat, arg = tf_arg(x), domain = tf_domain(x))
}

make_problem_setting <- function(
  problem,
  noise = FALSE,
  template_mode = c("given", "estimated"),
  seed = 1L,
  lambda = NULL
) {
  template_mode <- match.arg(template_mode)
  x <- if (noise) add_white_noise(problem$x, snr = 10, seed = seed) else
    problem$x
  warp_spec <- problem$warp_spec
  if (!is.null(lambda)) {
    warp_spec$lambda <- lambda
  }

  list(
    name = problem$name,
    label = paste(
      problem$name,
      if (!is.null(lambda)) paste0("| lambda = ", lambda) else NULL,
      if (noise) "| noisy (SNR = 10)" else "| clean",
      "|",
      if (template_mode == "given") "template supplied" else
        "template estimated",
      sep = " "
    ),
    x = x,
    template = if (template_mode == "given") problem$template else NULL,
    true_template = if (is.null(problem$true_forward_warps)) NULL else
      problem$template,
    true_aligned = problem$aligned,
    true_forward_warps = problem$true_forward_warps,
    warp_spec = warp_spec,
    template_mode = template_mode,
    noise = noise,
    lambda = warp_spec$lambda
  )
}

new_real_problem <- function(name, x) {
  list(
    name = name,
    x = x,
    template = mean(x),
    aligned = NULL,
    true_template = NULL,
    true_forward_warps = NULL,
    warp_spec = new_registration_warp_spec(
      domain = tf_domain(x),
      n_curves = length(x)
    )
  )
}

new_registration_with_template <- function(registration, template, call) {
  tf:::new_tf_registration(
    registered = registration$registered,
    inv_warps = registration$inv_warps,
    template = template,
    x = registration$x,
    call = call %||% registration$call
  )
}

stabilize_forward_warps <- function(warp) {
  domain <- tf_domain(warp)
  arg <- tf_arg(warp)
  warp_values <- tf_evaluations(warp)
  eps <- diff(domain) * 1e-6 / max(1, length(arg) - 1)

  stabilized <- lapply(warp_values, \(vals) {
    vals <- pmin(pmax(vals, domain[1]), domain[2])
    vals[1] <- domain[1]
    vals[length(vals)] <- domain[2]
    for (i in 2:length(vals)) {
      if (vals[i] <= vals[i - 1]) {
        vals[i] <- vals[i - 1] + eps
      }
    }
    if (vals[length(vals)] <= vals[1]) {
      return(seq(domain[1], domain[2], length.out = length(vals)))
    }
    vals <- domain[1] +
      (vals - vals[1]) * diff(domain) / (vals[length(vals)] - vals[1])
    vals[1] <- domain[1]
    vals[length(vals)] <- domain[2]
    vals
  })

  tfd(stabilized, arg = arg, domain = domain)
}

build_fda_registration_single <- function(
  x,
  template,
  WfdParobj,
  iterlim = 10L,
  crit = 2L
) {
  arg <- tf_arg(x)
  yfd <- tf_2_fd(x)
  y0fd <- tf_2_fd(template)

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
    domain = tf_domain(x)
  )
  forward_warps <- stabilize_forward_warps(forward_warps)
  aligned <- tf_align(x, forward_warps)
  template_out <- template

  tf:::new_tf_registration(
    registered = aligned,
    inv_warps = tf_invert(forward_warps),
    template = template_out,
    x = x,
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

run_outer_iterations <- function(
  x,
  initial_template,
  max_iter,
  tol,
  inner_fun,
  call
) {
  arg <- tf_arg(x)
  domain_length <- diff(tf_domain(x))
  current_template <- initial_template
  best_registration <- NULL
  best_template <- current_template
  best_obj <- Inf

  for (iter in seq_len(max_iter)) {
    registration <- inner_fun(template = current_template)
    aligned <- registration$registered
    aligned_on_arg <- suppressWarnings(tf_interpolate(aligned, arg = arg))
    template_on_arg <- suppressWarnings(tfd(current_template, arg = arg))

    obj <- suppressWarnings(mean(
      tf_integrate((aligned_on_arg - template_on_arg)^2, arg = arg) /
        domain_length,
      na.rm = TRUE
    ))

    if (is.finite(obj)) {
      if (
        is.finite(best_obj) && obj > best_obj * (1 + sqrt(.Machine$double.eps))
      ) {
        registration <- best_registration
        current_template <- best_template
        break
      }
      best_registration <- registration
      best_template <- current_template
      best_obj <- obj
    } else if (is.null(best_registration)) {
      best_registration <- registration
    }

    if (iter == max_iter) {
      break
    }

    new_template <- suppressWarnings(mean(aligned_on_arg, na.rm = TRUE))
    old_vec <- tf_evaluate(template_on_arg, arg = arg)[[1]]
    new_vec <- tf_evaluate(new_template, arg = arg)[[1]]
    missing_tmpl <- !is.finite(new_vec)
    if (any(missing_tmpl)) {
      new_vec[missing_tmpl] <- old_vec[missing_tmpl]
    }
    new_template <- tfd(
      matrix(new_vec, nrow = 1),
      arg = arg,
      domain = tf_domain(x)
    )

    delta <- suppressWarnings(tf_integrate(
      (new_template - template_on_arg)^2,
      arg = arg
    ))
    norm_sq <- suppressWarnings(tf_integrate(template_on_arg^2, arg = arg))
    delta <- as.numeric(delta / domain_length)
    norm_sq <- as.numeric(norm_sq / domain_length)
    current_template <- new_template

    if (is.finite(delta) && delta / max(norm_sq, .Machine$double.eps) < tol^2) {
      break
    }
  }

  new_registration_with_template(
    registration = best_registration %||% registration,
    template = best_template %||% current_template,
    call = call
  )
}

build_fda_registration <- function(
  setting,
  iterlim = 10L,
  crit = 2L,
  max_iter = 5L,
  tol = 1e-2
) {
  WfdParobj <- registration_problem_wfd_par(list(warp_spec = setting$warp_spec))
  if (!is.null(setting$template)) {
    return(build_fda_registration_single(
      x = setting$x,
      template = setting$template,
      WfdParobj = WfdParobj,
      iterlim = iterlim,
      crit = crit
    ))
  }

  run_outer_iterations(
    x = setting$x,
    initial_template = mean(setting$x),
    max_iter = max_iter,
    tol = tol,
    inner_fun = \(template)
      build_fda_registration_single(
        x = setting$x,
        template = template,
        WfdParobj = WfdParobj,
        iterlim = iterlim,
        crit = crit
      ),
    call = quote(
      iterative_register_fda(
        x = x,
        template = NULL,
        max_iter = 5,
        WfdParobj = bspline_warp_par(nbasis = 6, lambda = 0),
        iterlim = 10,
        crit = 2
      )
    )
  )
}

build_tf_registration <- function(
  setting,
  iterlim = 10L,
  crit = 2L,
  max_iter = 5L,
  tol = 1e-2
) {
  warps <- tf_estimate_warps(
    setting$x,
    method = "fda",
    template = setting$template,
    max_iter = max_iter,
    tol = tol,
    nbasis = setting$warp_spec$basis$nbasis,
    lambda = setting$warp_spec$lambda,
    iterlim = iterlim,
    crit = crit
  )
  template <- attr(warps, "template")
  warps <- stabilize_forward_warps(warps)
  tf:::new_tf_registration(
    registered = tf_align(setting$x, warps),
    inv_warps = tf_invert(warps),
    template = template,
    x = setting$x,
    call = quote(
      tf_register(
        x,
        method = "fda",
        template = template,
        max_iter = 5,
        tol = 1e-2,
        nbasis = 6,
        lambda = 0,
        iterlim = 10,
        crit = 2
      )
    )
  )
}

forward_warps_from_registration <- function(registration) {
  tf_invert(registration$inv_warps)
}

registration_metrics <- function(
  registration,
  truth_forward_warps = NULL,
  crit = 2L,
  elapsed = NA_real_
) {
  warp_rmse <- NA_real_
  if (!is.null(truth_forward_warps)) {
    warp_rmse <- sqrt(mean(
      (as.matrix(forward_warps_from_registration(registration)) -
        as.matrix(truth_forward_warps))^2
    ))
  }

  list(
    warp_rmse = warp_rmse,
    crit = crit_value_tfd(
      registration$template,
      registration$registered,
      crit = crit
    ),
    elapsed = elapsed
  )
}

plot_truth_row <- function(setting) {
  plot(
    setting$x,
    main = "Input data",
    ylab = "x(t)",
    xlab = "t",
    points = FALSE
  )
  if (!is.null(setting$true_template)) {
    graphics::lines(setting$true_template, lwd = 2, lty = 2)
  }

  if (is.null(setting$true_forward_warps)) {
    graphics::plot.new()
    graphics::title(main = "True forward warps")
    graphics::text(
      x = 0.5,
      y = 0.5,
      labels = "No ground truth available",
      cex = 1
    )
  } else {
    plot(
      setting$true_forward_warps,
      main = "True forward warps",
      ylab = "t = h(s)",
      xlab = "Aligned time s",
      points = FALSE
    )
    graphics::abline(a = 0, b = 1, lty = 2, col = "grey40")
  }

  if (is.null(setting$true_aligned) || is.null(setting$true_template)) {
    graphics::plot.new()
    graphics::title(main = "True aligned / template")
    graphics::text(
      x = 0.5,
      y = 0.5,
      labels = "No ground truth available",
      cex = 1
    )
  } else {
    plot(
      setting$true_aligned,
      main = "True aligned",
      ylab = "x(s)",
      xlab = "Aligned time s",
      points = FALSE
    )
    graphics::lines(setting$true_template, lwd = 2, lty = 2)
  }

  graphics::mtext(
    text = if (is.null(setting$true_forward_warps)) {
      "Reference row | input data plus ground-truth placeholders"
    } else {
      "Truth | input data, true forward warps, true aligned/template"
    },
    side = 3,
    line = 1,
    adj = 0,
    cex = 0.8
  )
}

plot_registration_row <- function(registration, label, call_text, metrics) {
  inv_warps_plot <- suppressWarnings(tfd(
    registration$inv_warps,
    arg = tf_arg(registration$x),
    domain = tf_domain(registration$x)
  ))

  plot(
    registration$x,
    main = "Input data",
    ylab = "x(t)",
    xlab = "t",
    points = FALSE
  )
  if (!is.null(registration$template)) {
    graphics::lines(registration$template, lwd = 2, lty = 2)
  }
  graphics::mtext(
    text = paste(
      label,
      call_text,
      sprintf(
        "warp RMSE = %.4f | crit = %.4g | elapsed = %.3fs",
        metrics$warp_rmse,
        metrics$crit,
        metrics$elapsed
      ),
      sep = "\n"
    ),
    side = 3,
    line = 1,
    adj = 0,
    cex = 0.78
  )

  plot(
    inv_warps_plot,
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

safe_metric_text <- function(metrics) {
  sprintf(
    "warp RMSE = %s | crit = %.4g | elapsed = %.3fs",
    if (is.na(metrics$warp_rmse)) "NA" else sprintf("%.4f", metrics$warp_rmse),
    metrics$crit,
    metrics$elapsed
  )
}

render_one_setting <- function(
  setting,
  iterlim = 10L,
  crit = 2L,
  max_iter = 5L,
  tol = 1e-2
) {
  fda_time <- system.time({
    fda_reg <- build_fda_registration(
      setting,
      iterlim = iterlim,
      crit = crit,
      max_iter = max_iter,
      tol = tol
    )
  })[["elapsed"]]
  tf_time <- system.time({
    tf_reg <- build_tf_registration(
      setting,
      iterlim = iterlim,
      crit = crit,
      max_iter = max_iter,
      tol = tol
    )
  })[["elapsed"]]

  fda_metrics <- registration_metrics(
    registration = fda_reg,
    truth_forward_warps = setting$true_forward_warps,
    crit = crit,
    elapsed = fda_time
  )
  tf_metrics <- registration_metrics(
    registration = tf_reg,
    truth_forward_warps = setting$true_forward_warps,
    crit = crit,
    elapsed = tf_time
  )

  old_par <- graphics::par(
    mfrow = c(3, 3),
    oma = c(0, 0, 5, 0),
    mar = c(4, 4, 3, 1)
  )
  on.exit(graphics::par(old_par), add = TRUE)

  plot_truth_row(setting)
  plot_registration_row(
    registration = fda_reg,
    label = "Upstream fda",
    call_text = if (is.null(setting$template)) {
      paste0(
        "outer template update: max_iter = 5 | inner fda::register.fd(..., ",
        "WfdParobj nbasis = 6, lambda = ",
        setting$lambda,
        ", iterlim = 10, crit = 2)"
      )
    } else {
      paste0(
        "fda::register.fd(..., template supplied, WfdParobj nbasis = 6, ",
        "lambda = ",
        setting$lambda,
        ", iterlim = 10, crit = 2)"
      )
    },
    metrics = fda_metrics
  )
  plot_registration_row(
    registration = tf_reg,
    label = "tf backend",
    call_text = if (is.null(setting$template)) {
      paste0(
        "outer template update: max_iter = 5 | inner tf backend (..., ",
        "nbasis = 6, lambda = ",
        setting$lambda,
        ", iterlim = 10, crit = 2)"
      )
    } else {
      paste0(
        "tf backend (..., template supplied, nbasis = 6, ",
        "lambda = ",
        setting$lambda,
        ", iterlim = 10, crit = 2)"
      )
    },
    metrics = tf_metrics
  )

  graphics::mtext(
    text = paste0(
      "Registration Comparison | ",
      setting$label,
      " | n = ",
      length(setting$x),
      " | grid = ",
      length(tf_arg(setting$x))
    ),
    side = 3,
    outer = TRUE,
    line = 2,
    cex = 1
  )
  graphics::mtext(
    text = paste(
      "Summary:",
      paste("fda:", safe_metric_text(fda_metrics)),
      paste("tf:", safe_metric_text(tf_metrics)),
      sep = "   "
    ),
    side = 3,
    outer = TRUE,
    line = 0.5,
    cex = 0.82,
    adj = 0
  )

  list(fda = fda_metrics, tf = tf_metrics)
}

render_appendix_page <- function(metrics_df) {
  metrics_fmt <- within(metrics_df, {
    fda_warp_rmse <- ifelse(
      is.na(fda_warp_rmse),
      "NA",
      sprintf("%.4f", fda_warp_rmse)
    )
    tf_warp_rmse <- ifelse(
      is.na(tf_warp_rmse),
      "NA",
      sprintf("%.4f", tf_warp_rmse)
    )
    fda_crit <- sprintf("%.4g", fda_crit)
    tf_crit <- sprintf("%.4g", tf_crit)
    fda_elapsed <- sprintf("%.3f", fda_elapsed)
    tf_elapsed <- sprintf("%.3f", tf_elapsed)
  })
  lines <- capture.output(print(metrics_fmt, row.names = FALSE, right = FALSE))

  graphics::plot.new()
  graphics::title(main = "Appendix: Metrics for All Settings", line = -1)
  graphics::text(
    x = 0.01,
    y = seq(0.96, 0.04, length.out = length(lines)),
    labels = lines,
    adj = c(0, 1),
    family = "mono",
    cex = 0.6
  )
}

output_path <- file.path(
  "attic",
  "sim-registration",
  "register-comparison-extended.pdf"
)

base_problems <- registration_reference_problems()[c(
  "easy_phase",
  "wiggly_phase",
  "amplitude_phase",
  "real_gait"
)]
base_problems$pinch <- new_real_problem(
  name = "pinch",
  x = get("pinch", envir = asNamespace("tf"))[1:5]
)

settings <- unlist(
  lapply(seq_along(base_problems), \(i) {
    problem <- base_problems[[i]]
    list(
      make_problem_setting(
        problem,
        noise = FALSE,
        template_mode = "given",
        seed = 100 + i
      ),
      make_problem_setting(
        problem,
        noise = TRUE,
        template_mode = "given",
        seed = 200 + i
      ),
      make_problem_setting(
        problem,
        noise = FALSE,
        template_mode = "estimated",
        seed = 300 + i
      ),
      make_problem_setting(
        problem,
        noise = TRUE,
        template_mode = "estimated",
        seed = 400 + i
      )
    )
  }),
  recursive = FALSE
)
wiggly_penalized <- list(
  make_problem_setting(
    base_problems$wiggly_phase,
    noise = FALSE,
    template_mode = "given",
    seed = 5101L,
    lambda = 0.1
  ),
  make_problem_setting(
    base_problems$wiggly_phase,
    noise = TRUE,
    template_mode = "given",
    seed = 5201L,
    lambda = 0.1
  ),
  make_problem_setting(
    base_problems$wiggly_phase,
    noise = FALSE,
    template_mode = "estimated",
    seed = 5301L,
    lambda = 0.1
  ),
  make_problem_setting(
    base_problems$wiggly_phase,
    noise = TRUE,
    template_mode = "estimated",
    seed = 5401L,
    lambda = 0.1
  )
)
settings <- c(settings, wiggly_penalized)

grDevices::pdf(file = output_path, width = 14, height = 14, onefile = TRUE)
metrics <- lapply(settings, render_one_setting)
metrics_df <- do.call(
  rbind,
  lapply(seq_along(settings), \(i) {
    data.frame(
      setting = settings[[i]]$label,
      fda_warp_rmse = metrics[[i]]$fda$warp_rmse,
      fda_crit = metrics[[i]]$fda$crit,
      fda_elapsed = metrics[[i]]$fda$elapsed,
      tf_warp_rmse = metrics[[i]]$tf$warp_rmse,
      tf_crit = metrics[[i]]$tf$crit,
      tf_elapsed = metrics[[i]]$tf$elapsed
    )
  })
)
render_appendix_page(metrics_df)
grDevices::dev.off()

names(metrics) <- vapply(settings, `[[`, character(1), "label")
print(metrics_df)
cat(normalizePath(output_path), "\n")
