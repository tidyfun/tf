new_tfb_spline <- function(
  data, # data.frame with id, arg, value
  domain = NULL,
  arg = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = FALSE,
  ...
) {
  if (vec_size(data) == 0) {
    ret <- new_vctr(
      data,
      domain = numeric(2),
      arg = numeric(),
      family = character(),
      class = c("tfb_spline", "tfb", "tf")
    )
    return(ret)
  }

  domain <- domain %||% range(data$arg)
  arg_u <- uniquecombs(data$arg, ordered = TRUE)

  assert_numeric(
    domain,
    finite = TRUE,
    any.missing = FALSE,
    sorted = TRUE,
    len = 2,
    unique = TRUE
  )
  u_args <- unlist(arg_u, use.names = FALSE)
  if (domain[1] > min(u_args) || max(u_args) > domain[2]) {
    cli::cli_abort("Evaluations must be inside the domain.")
  }

  # explicit factor-conversion to avoid reordering:
  data$id <- factor(data$id, unique(as.character(data$id)))

  dots <- list(...)
  s_args <- dots[names(dots) %in% formalArgs(s)]
  if (!has_name(s_args, "bs")) s_args$bs <- "cr"
  if (s_args$bs == "ad") {
    cli::cli_warn(c(
      x = "Adaptive smooths with ({.code bs = 'ad'}) not implemented yet.",
      i = "Return value uses {.code bs = 'cr'} instead."
    ))
    s_args$bs <- "cr"
  }
  if (!has_name(s_args, "k")) s_args$k <- min(25, nrow(arg_u))
  gam_args <- dots[names(dots) %in% c(formalArgs(gam), formalArgs(bam))]
  if (!has_name(gam_args, "sp")) gam_args$sp <- -1

  n_evaluations <- table(data$id)
  arg_list <- split(data$arg, data$id)
  regular <- all(duplicated(arg_list)[-1])

  s_call <- as.call(c(quote(s), quote(arg), s_args))
  s_spec <- eval(s_call)
  spec_object <- smooth.construct(
    s_spec,
    data = data_frame0(arg = arg_u$x),
    knots = NULL
  )
  spec_object$call <- s_call

  if (is.null(gam_args$family)) {
    gam_args$family <- stats::gaussian()
  }
  if (is.character(gam_args$family)) {
    gam_args$family <- get(
      gam_args$family,
      mode = "function",
      envir = parent.frame()
    )
  }
  if (is.function(gam_args$family)) {
    gam_args$family <- gam_args$family()
  }

  ls_fit <- gam_args$family$family == "gaussian" &
    gam_args$family$link == "identity"

  underdetermined <- n_evaluations <= spec_object$bs.dim
  if (!penalized) {
    if (any(underdetermined)) {
      cli::cli_abort(
        c(
          "Can't compute spline coefficients for too sparse data: At least as many basis functions as evaluations for {sum(underdetermined)} of {vec_size(underdetermined)} entries in
        input data.",
          ">" = "Use {.code penalized = TRUE} or reduce {.arg k} for spline interpolation.",
          "i" = "Affected entries: {names(n_evaluations[underdetermined])}"
        )
      )
    }
    fit <-
      fit_unpenalized(
        data = data,
        spec_object = spec_object,
        arg_u = arg_u,
        gam_args = gam_args,
        regular = regular,
        ls_fit = ls_fit
      )
  } else {
    if (any(underdetermined)) {
      cli::cli_warn(
        c(
          "Sparse data: Spline interpolation may be unreliable/infeasible for functions with fewer evaluations than basis functions, {sum(underdetermined)} of {vec_size(underdetermined)} entries affected.",
          ">" = "Check results and consider reducing {.arg k} for spline interpolation.",
          "i" = "Affected entries: {names(n_evaluations[underdetermined])}"
        )
      )
    }
    fit <-
      fit_penalized(
        data = data,
        spec_object = spec_object,
        arg_u = arg_u,
        gam_args = gam_args,
        regular = regular,
        global = global,
        ls_fit = ls_fit
      )
    if (global && verbose) {
      cli::cli_inform(
        "Using global smoothing parameter {.code sp = {round(fit$sp[1], 3)}} estimated on subsample of curves."
      )
    }
  }
  if (!regular) {
    arg_u <- data_frame0(x = arg_u$x)
    spec_object$X <- PredictMat(
      spec_object,
      data = data_frame0(arg = arg_u$x)
    )
  }
  if (isTRUE(min(fit$pve) < 0.5)) {
    cli::cli_warn(c(
      x = "Smooth fit captures less than half of input data variability for {sum(fit$pve < .5)} entries.",
      i = "Consider increasing basis dimension {.arg k} (or decreasing penalization ({.arg sp}))."
    ))
    verbose <- TRUE
  }
  if (verbose) {
    pve_summary <- utils::capture.output(summary(round(100 * fit$pve, 1)))
    cli::cli_inform(c(
      "Percentage of input data variability preserved in basis representation",
      "({if (!ls_fit) 'on inverse link-scale '}per functional observation, approximate):",
      pve_summary[1],
      pve_summary[2]
    ))
  }

  basis_constructor <- smooth_spec_wrapper(spec_object)
  # sp: from fit for global/set, -1 for local smoothing/given as -1, NA for unpen
  if (isTRUE(dots$sp != -1) || global) {
    s_args$sp <- fit$sp[1] |> unname()
  } else if (penalized) {
    s_args$sp <- -1
  } else {
    s_args$sp <- NA
  }
  s_args <- s_args[sort(names(s_args))] # for uniform basis_label for compare_tf_attrib
  s_call <- as.call(c(quote(s), quote(arg), s_args))

  family <- eval(gam_args$family)
  if (family$family == "gaussian" && family$link == "identity") {
    family_label <- ""
  } else {
    family_label <- sprintf("(%s with %s-link)", family$family, family$link)
  }

  ret <- new_vctr(
    fit[["coef"]],
    domain = domain,
    basis = basis_constructor,
    basis_label = deparse1(s_call, width.cutoff = 60),
    basis_args = s_args,
    basis_matrix = spec_object$X,
    arg = arg_u$x,
    family = family,
    family_label = family_label,
    class = c("tfb_spline", "tfb", "tf")
  )
  assert_arg(tf_arg(ret), ret)
  ret
}

#-------------------------------------------------------------------------------

#' Spline-based representation of functional data
#'
#' Represent curves as a weighted sum of spline basis functions.
#'
#' The basis to be used is set up via a call to [mgcv::s()] and all the spline
#' bases discussed in [mgcv::smooth.terms()] are available, in principle.
#' Depending on the value of the `penalized`- and `global`-flags, the
#' coefficient vectors for each observation are then estimated via fitting a GAM
#' (separately for each observation, if `!global`) via [mgcv::magic()] (least
#' square error, the default) or [mgcv::gam()] (if a `family` argument was
#' supplied) or unpenalized least squares / maximum likelihood.
#'
#' After the "smoothed" representation is computed, the amount of smoothing that
#' was performed is reported in terms of the "percentage of variability
#' preserved", which is the variance (or the explained deviance, in the general
#' case if `family` was specified) of the smoothed function values divided by the variance of the original
#' values (the null deviance, in the general case). Reporting can be switched off
#' with `verbose = FALSE`.
#'
#' The `...` arguments supplies arguments to both the
#' spline basis (via [mgcv::s()]) and the estimation (via
#' [mgcv::magic()] or [mgcv::gam()]), the most important arguments are:
#'
#' - **`k`**: how many basis functions should the spline basis use, default is 25.
#' - **`bs`**: which type of spline basis should be used, the default is cubic
#' regression splines (`bs = "cr"`)
#' - **`family`** argument: use this if minimizing squared errors is not
#' a reasonable criterion for the representation accuracy (see
#' [mgcv::family.mgcv()] for what's available) and/or if function values are
#' restricted to be e.g. positive (`family = Gamma()/tw()/...`), in
#' \eqn{[0,1]}  (`family = betar()`), etc.
#' - **`sp`**: numeric value for the smoothness penalty weight, for manually
#' setting the amount of smoothing for all curves, see [mgcv::s()]. This
#' (drastically) reduces computation time. Defaults to `-1`, i.e., automatic
#' optimization of `sp` using [mgcv::magic()] (LS fits) or [mgcv::gam()] (GLM),
#' source code in `R/tfb-spline-utils.R`.
#'
#' If **`global == TRUE`**, this uses a small subset of curves (10`%` of curves,
#' at least 5, at most 100; non-random sample using every j-th curve in the
#' data) on which smoothing parameters per curve are estimated and then takes
#' the mean of the log smoothing parameter of those as `sp` for all curves. This
#' is much faster than optimizing for each curve on large data sets. For very
#' sparse or noisy curves, estimating a common smoothing parameter based on the
#' data for all curves simultaneously is likely to yield better results, this is
#' *not* what's implemented here.
#'
#' @param penalized `TRUE` (default) estimates regularized/penalized basis
#'   coefficients via [mgcv::magic()] or [mgcv::gam.fit()], `FALSE` yields
#'   ordinary least squares / ML estimates for basis coefficients. `FALSE` is
#'   much faster but will overfit for noisy data if `k` is (too) large.
#' @param global Defaults to `FALSE`. If `TRUE` and `penalized = TRUE`, all
#'   functions share the same smoothing parameter (see details).
#' @param verbose `TRUE` (default) outputs statistics about the fit achieved by
#'   the basis and other diagnostic messages.
#' @param ...  arguments to the calls to [mgcv::s()] setting up the basis (and
#'   to [mgcv::magic()] or [mgcv::gam.fit()] if `penalized = TRUE`). Uses `k =
#'   25` cubic regression spline basis functions (`bs = "cr"`) by default, but
#'   should be set appropriately by the user. See details and examples in the
#'   vignettes.
#' @inheritParams tfb
#' @returns a `tfb`-object
#' @seealso [mgcv::smooth.terms()] for spline basis options.
#' @examples
#' arg <- seq(0, 1, length.out = 21)
#' mat <- rbind(sin(2 * pi * arg), cos(2 * pi * arg))
#' fit <- tfb_spline(mat, arg = arg, k = 8, penalized = FALSE, verbose = FALSE)
#' fit
#' @family tfb-class
#' @family tfb_spline-class
#' @export
tfb_spline <- function(data, ...) UseMethod("tfb_spline")

#' @export
#' @inheritParams tfd.data.frame
#' @describeIn tfb_spline convert data frames
tfb_spline.data.frame <- function(
  data,
  id = 1,
  arg = 2,
  value = 3,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  data <- df_2_df(data, id = id, arg = arg, value = value)
  ret <- new_tfb_spline(
    data,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
  names_data <- data[, id] |>
    unique() |>
    as.character() |>
    vec_as_names(repair = "unique")
  setNames(ret, names_data)
}

#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.matrix <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  if (is.null(arg)) arg <- unlist(find_arg(data, arg), use.names = FALSE)
  names_data <- rownames(data)

  data <- mat_2_df(data, arg)
  ret <- new_tfb_spline(
    data,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
  if (!is.null(names_data)) {
    names_data <- names_data |>
      as.character() |>
      vec_as_names(repair = "unique")
    setNames(ret, names_data)
  }
  ret
}

#' @export
#' @describeIn tfb_spline convert matrices
tfb_spline.numeric <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  data <- t(as.matrix(data))
  tfb_spline(
    data = data,
    arg = arg,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
}

#' @export
#' @describeIn tfb_spline convert lists
tfb_spline.list <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  vectors <- map_lgl(data, is.numeric)
  if (any(vectors) && !all(vectors)) {
    cli::cli_abort("{.arg data} must have the same type.")
  }

  if (all(vectors)) {
    lens <- lengths(data)
    if (all(lens == lens[1])) {
      data <- do.call(rbind, data)
      # dispatch to matrix method
      return(tfb_spline(
        data,
        arg,
        domain = domain,
        penalized = penalized,
        global = global,
        verbose = verbose,
        ...
      ))
    }
    if (is.null(arg)) {
      cli::cli_abort("{.arg arg} must be supplied.")
    }
    if (length(arg) != length(data) || any(lengths(arg) != lens)) {
      cli::cli_abort("Length of {.arg arg} does not match {.arg data}.")
    }
    data <- map2(arg, data, \(x, y) data_frame0(arg = x, value = y))
  }
  dims <- map(data, dim)
  if (any(lengths(dims) != 2) || any(map_int(dims, 2) != 2)) {
    cli::cli_abort("{.arg data} cannot be formatted into dimension 2.")
  }
  if (!all(rapply(data, is.numeric))) {
    cli::cli_abort("{.arg data} and {.arg arg} must all be numeric.")
  }
  n_evals <- map(dims, 1)
  tmp <- do.call(rbind, data)
  tmp <- cbind(
    rep(unique_id(names(data)) %||% seq_along(data), times = n_evals),
    tmp
  )
  # dispatch to data.frame method
  tfb_spline(
    tmp,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
}

#' @export
#' @describeIn tfb_spline convert `fd` objects. Almost exact re-representation for
#'    objects using Fourier- or B-spline bases, other `fda`-style bases are not implemented here.
tfb_spline.fd <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = FALSE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  check_installed("fda")
  bs <- switch(
    data$basis$type,
    bspline = "bs",
    fourier = "fourier",
    {
      cli::cli_warn(c(
        "exact {.cls fd} conversion not implemented for this {.pkg fda} basis type.",
        x = "Using {.pkg mgcv} basis {.val bs}, original basis type was {.val {data$basis$type}}.",
        i = "Only {.pkg fda}-compatible B-Spline and Fourier bases are implemented in {.pkg tf}."
      ))
      "bs"
    }
  )
  xt <- list()
  if (bs == "fourier") {
    xt$period <- data$basis$params
  }
  domain <- domain %||% data$basis$rangeval
  if (is.null(arg)) {
    arg <- seq(domain[1], domain[2], length.out = 100)
    if (verbose) {
      cli::cli_inform(
        "No {.arg arg} provided, using {length(arg)} equally spaced points over domain [{domain[1]}, {domain[2]}]."
      )
    }
  }
  k <- data$basis$nbasis
  if (bs == "bs" && k < 4) {
    k <- 4
    cli::cli_warn(
      "No. of basis functions {.arg k} set to 4 (was {data$basis$nbasis})."
    )
  }
  # use (unpenalized) fit to *smoothed* data by default, not to data$y!
  smoothed <- mat_2_df(t(fda::eval.basis(arg, data$basis) %*% data$coefs), arg)

  new_tfb_spline(
    smoothed,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    k = k,
    bs = bs,
    xt = xt,
    ...
  )
}

#' @export
#' @describeIn tfb_spline convert `fdSmooth` objects. Almost exact re-representation for
#'    objects using Fourier- or B-spline bases, other `fda`-style bases are not implemented here.
tfb_spline.fdSmooth <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = FALSE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  arg <- arg %||% as.numeric(data$argvals)
  tfb_spline.fd(
    data = data$fd,
    arg = arg,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
}

#' @export
#' @describeIn tfb_spline convert `tfd` (raw functional data)
tfb_spline.tfd <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)

  tmp <- tf_2_df(data, arg)
  tfb_spline(
    tmp,
    domain = domain,
    penalized = penalized,
    global = global,
    verbose = verbose,
    ...
  )
}

#' @export
#' @describeIn tfb_spline convert `tfb`: modify basis representation, smoothing.
tfb_spline.tfb <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  arg <- arg %||% tf_arg(data)
  domain <- domain %||% tf_domain(data)
  dots <- list(...)
  s_args <- modifyList(
    attr(data, "basis_args"),
    dots[names(dots) %in% formalArgs(s)]
  )
  names_data <- names(data)
  if (vec_size(data) == 0) {
    # data = rep(0, )
    # maybe try to make an empty vector that won't break anything like matrix algebra?

    new_tfb_spline(
      data,
      arg = arg,
      domain = domain,
      penalized = penalized,
      global = global,
      s_args,
      verbose = verbose
    )
  } else {
    data <- tf_2_df(data, arg = arg)
    do.call(
      tfb_spline,
      c(
        list(data),
        domain = domain,
        global = global,
        penalized = penalized,
        s_args,
        verbose = verbose
      )
    )
  }
}

#' @export
#' @describeIn tfb_spline convert `tfb`: default method, returning prototype
#'   when data is missing
tfb_spline.default <- function(
  data,
  arg = NULL,
  domain = NULL,
  penalized = TRUE,
  global = FALSE,
  verbose = TRUE,
  ...
) {
  cli::cli_warn(
    "Input {.arg data} not from a recognized class; returning prototype of length 0."
  )

  data <- data_frame0()
  new_tfb_spline(
    data,
    domain = domain,
    penalized = penalized,
    global = global,
    ...
  )
}
