devtools::load_all()
source(file.path("attic", "sim-registration", "reference-problems.R"))

compute_crit_value <- function(template_vals, reg_vals, crit = 2L) {
  template_vals <- as.matrix(template_vals)
  reg_vals <- as.matrix(reg_vals)
  if (nrow(template_vals) == 1L) {
    template_vals <- template_vals[rep(1L, nrow(reg_vals)), , drop = FALSE]
  }
  total <- 0
  for (i in seq_len(nrow(reg_vals))) {
    y0 <- template_vals[min(i, nrow(template_vals)), ]
    y1 <- reg_vals[i, ]
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

run_one_problem <- function(problem, iterlim = 15L, crit = 2L) {
  yfd <- tf_2_fd(problem$x)
  y0fd <- tf_2_fd(problem$template)
  arg <- tf_arg(problem$x)
  WfdParobj <- registration_problem_wfd_par(problem)

  old_time <- system.time({
    old <- fda::register.fd(
      y0fd = y0fd,
      yfd = yfd,
      WfdParobj = WfdParobj,
      iterlim = iterlim,
      dbglev = 0,
      crit = crit
    )
  })
  new_time <- system.time({
    new <- tf_estimate_warps(
      problem$x,
      method = "fda",
      template = problem$template,
      max_iter = 1,
      nbasis = problem$warp_spec$basis$nbasis,
      lambda = problem$warp_spec$lambda,
      iterlim = iterlim,
      crit = crit
    )
  })

  warp_old <- registration_fit_forward_warp_matrix(old, arg = arg)
  warp_new <- t(as.matrix(new))
  reg_old <- fda::eval.fd(arg, old$regfd)
  reg_new <- as.matrix(tf_align(problem$x, new))
  template_old <- matrix(fda::eval.fd(arg, old$y0fd), nrow = 1)
  template_new <- as.matrix(problem$template)

  truth_rmse_fda <- NA_real_
  truth_rmse_tf <- NA_real_
  if (!is.null(problem$true_forward_warps)) {
    truth <- t(as.matrix(problem$true_forward_warps))
    truth_rmse_fda <- sqrt(mean((as.vector(warp_old) - as.vector(truth))^2))
    truth_rmse_tf <- sqrt(mean((as.vector(warp_new) - as.vector(truth))^2))
  }

  data.frame(
    problem = problem$name,
    crit = crit,
    elapsed_fda = unname(old_time["elapsed"]),
    elapsed_tf = unname(new_time["elapsed"]),
    speedup = unname(old_time["elapsed"] / new_time["elapsed"]),
    warp_rmse = sqrt(mean((as.vector(warp_old) - as.vector(warp_new))^2)),
    reg_rmse = sqrt(mean((as.vector(reg_old) - as.vector(reg_new))^2)),
    truth_rmse_fda = truth_rmse_fda,
    truth_rmse_tf = truth_rmse_tf,
    crit_fda = compute_crit_value(template_old, t(reg_old), crit = crit),
    crit_tf = compute_crit_value(template_new, reg_new, crit = crit)
  )
}

problems <- registration_reference_problems()
results <- do.call(
  rbind,
  lapply(problems, run_one_problem)
)

print(results)
