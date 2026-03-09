#-------------------------------------------------------------------------------
# (un)warping utilities
coerce_warp_to_tfd <- function(warp) {
  if (is_tfb(warp)) {
    return(as.tfd(warp))
  }
  warp
}

strictify_boundary_ties <- function(values, domain, tol_abs) {
  n_values <- length(values)
  if (n_values <= 1) {
    return(values)
  }

  diffs <- diff(values)
  is_increasing <- all(diffs >= 0)
  is_decreasing <- all(diffs <= 0)
  if (!is_increasing && !is_decreasing) {
    return(values)
  }

  tie_pos <- which(diffs == 0)
  if (length(tie_pos) == 0) {
    return(values)
  }
  near_boundary <- abs(values - domain[1]) <= tol_abs |
    abs(values - domain[2]) <= tol_abs
  tie_near_boundary <- near_boundary[tie_pos] | near_boundary[tie_pos + 1]
  if (!all(tie_near_boundary)) {
    return(values)
  }

  domain_span <- diff(domain)
  if (domain_span <= 0) {
    return(values)
  }
  eps <- min(tol_abs, domain_span / (max(1, n_values - 1) * 2))
  if (eps <= 0) {
    return(values)
  }

  adjusted <- if (is_increasing) {
    cummax(values) + eps * seq.int(0, n_values - 1)
  } else {
    -cummax(-values) - eps * seq.int(0, n_values - 1)
  }

  shift_min <- domain[1] - min(adjusted)
  shift_max <- domain[2] - max(adjusted)
  if (shift_min <= shift_max) {
    shift <- min(max(0, shift_min), shift_max)
    return(adjusted + shift)
  }

  if (is_increasing) {
    return(seq(domain[1], domain[2], length.out = n_values))
  }
  seq(domain[2], domain[1], length.out = n_values)
}

stabilize_warp_values <- function(
  values,
  domain,
  tol = sqrt(.Machine$double.eps)
) {
  tol_abs <- max(1, diff(domain)) * tol
  values[values < domain[1] & values >= domain[1] - tol_abs] <- domain[1]
  values[values > domain[2] & values <= domain[2] + tol_abs] <- domain[2]
  values <- strictify_boundary_ties(values, domain = domain, tol_abs = tol_abs)
  values
}

strictify_domain_preserving_warp <- function(values, domain) {
  n_values <- length(values)
  if (n_values <= 1L) {
    return(values)
  }

  values <- pmin(pmax(values, domain[1]), domain[2])
  values[1] <- domain[1]
  values[n_values] <- domain[2]

  eps <- diff(domain) * 1e-6 / max(1, n_values - 1L)
  for (i in 2:n_values) {
    if (!is.finite(values[i]) || values[i] <= values[i - 1L]) {
      values[i] <- values[i - 1L] + eps
    }
  }

  if (values[n_values] <= values[1]) {
    return(seq(domain[1], domain[2], length.out = n_values))
  }

  values <- domain[1] +
    (values - values[1]) * diff(domain) / (values[n_values] - values[1])
  values[1] <- domain[1]
  values[n_values] <- domain[2]
  values
}

apply_tfb_warp <- function(fun, x, warp, dots = list()) {
  # keep_new_arg forced to FALSE here, otherwise basis matrix blows up:
  # would keep every unique gridpoint & cause plots to fail (resolution too small)
  warp <- coerce_warp_to_tfd(warp)
  if (isTRUE(dots$keep_new_arg)) {
    cli::cli_warn(
      "{.arg keep_new_arg} reset to FALSE - not applicable for {.cls tfb}."
    )
    dots$keep_new_arg <- FALSE
  }
  args <- c(list(x = as.tfd(x), warp = warp), dots)
  do.call(fun, args) |> tf_rebase(x)
}

is_non_domain_preserving_warp <- function(warp_evals, domain) {
  any(map_lgl(warp_evals, \(warp_vals) {
    finite_vals <- warp_vals[is.finite(warp_vals)]
    if (length(finite_vals) == 0) {
      return(TRUE)
    }
    warp_min <- min(finite_vals)
    warp_max <- max(finite_vals)
    # Check for expansion OR shrinkage
    warp_min < domain[1] ||
      warp_max > domain[2] ||
      warp_min > domain[1] + 1e-10 ||
      warp_max < domain[2] - 1e-10
  }))
}

unwarp_non_domain_preserving <- function(
  arg_list,
  x_evals,
  warp_evals,
  domain,
  evaluator_name
) {
  # Build (arg, value) pairs for each function, keeping only valid points
  valid_data <- pmap(
    list(arg_list, x_evals, warp_evals),
    \(arg_i, x_vals, warp_vals) {
      # warp_vals = h(arg_i), we want x(h(arg_i)) at each arg point
      # Use rule=1 to get NA where warp goes outside original domain
      reg_vals <- approx(arg_i, x_vals, xout = warp_vals, rule = 1)$y
      valid <- !is.na(reg_vals)
      list(arg = arg_i[valid], value = reg_vals[valid])
    }
  )

  # Create irregular tfd with only valid points
  new_tfd(
    arg = map(valid_data, "arg"),
    datalist = map(valid_data, "value"),
    regular = FALSE,
    domain = domain,
    evaluator = evaluator_name
  )
}

unwarp_domain_preserving <- function(x_evals, warp_evals, arg_list, domain) {
  inv_warp <- tfd(warp_evals, arg = arg_list, domain = domain) |>
    tf_invert(domain = domain) |>
    tfd(arg = arg_list, domain = domain)
  inv_warp_evals <- tf_evaluations(inv_warp) |>
    map(\(vals) stabilize_warp_values(vals, domain))
  tfd(x_evals, arg = inv_warp_evals, domain = domain)
}

#-------------------------------------------------------------------------------
# Landmark utils

# Helper: validate landmark matrix
validate_landmarks <- function(landmarks, domain, n, n_landmarks) {
  # Check strictly increasing within each row (skip NAs)
  for (i in seq_len(n)) {
    row_vals <- landmarks[i, !is.na(landmarks[i, ])]
    if (length(row_vals) > 1 && !all(diff(row_vals) > 0)) {
      cli::cli_abort(
        "Landmarks must be strictly increasing within each row. Problem at row {i}."
      )
    }
  }
  # Check strictly inside domain (skip NAs).
  # Landmarks at exact domain boundaries would create duplicate knots when
  # boundaries are appended in tf_register_landmark().
  lm_vals <- landmarks[!is.na(landmarks)]
  if (
    length(lm_vals) > 0 &&
      (any(lm_vals <= domain[1]) || any(lm_vals >= domain[2]))
  ) {
    cli::cli_abort(c(
      "All landmarks must be strictly inside the domain ({domain[1]}, {domain[2]}).",
      "i" = "Boundary landmarks are redundant with the domain anchors."
    ))
  }
  invisible(landmarks)
}

# Helper: validate and return template landmarks
validate_template_landmarks <- function(
  template,
  landmarks,
  domain,
  n_landmarks
) {
  if (is.null(template)) {
    return(colMeans(landmarks, na.rm = TRUE))
  }

  assert_numeric(template, len = n_landmarks, any.missing = FALSE)

  if (n_landmarks > 1 && !all(diff(template) > 0)) {
    cli::cli_abort("Template landmarks must be strictly increasing.")
  }
  if (any(template < domain[1]) || any(template > domain[2])) {
    cli::cli_abort(
      "Template landmarks must be within the domain [{domain[1]}, {domain[2]}]."
    )
  }
  template
}
