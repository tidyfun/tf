#' @include tfb-mv.R tfb-fpc.R tfd-mv.R
NULL

# Multivariate FPCA (Happ & Greven, 2018) --------------------------------------
#
# Strategy (see `attic/design/multivariate.md`): run the ordinary univariate
# FPCA on each of the `d` components, then combine the per-component scores into
# a single set of *shared* scalar scores per curve with vector-valued
# eigenfunctions. The result is a normal `tfb_mv` whose `d` components are
# ordinary `tfb_fpc` objects -- but every component stores the *same* score
# vector per curve, and `score_variance` holds the joint (multivariate)
# eigenvalues. Reconstruction, printing and plotting therefore work for free via
# the existing `tfb_mv` machinery; only *re-scoring new data* needs a joint
# code path (see `mfpc_rescore()`), wired into `tf_rebase()` / `vec_cast()`.

#' Multivariate functional principal component analysis (`f: R -> R^d`)
#'
#' `tfb_mfpc()` computes a *multivariate* functional principal component
#' analysis (MFPCA) of vector-valued functional data in the sense of
#' Happ & Greven (2018): a single set of scalar scores per curve, shared across
#' all `d` components, together with vector-valued eigenfunctions
#' \eqn{\Psi_m: \mathcal{T} \to \mathbb{R}^d}, so that
#' \eqn{f_i(t) \approx \mu(t) + \sum_m s_{im}\,\Psi_m(t)}.
#'
#' This is qualitatively different from `tfb_mv(data, basis = "fpc")`, which
#' fits an *independent* FPCA per component (separate eigenfunctions **and**
#' separate scores) and so cannot capture joint variation across dimensions.
#'
#' The estimator first runs the univariate FPCA (see [tfb_fpc()] / [fpc_wsvd()])
#' on each component to obtain univariate scores \eqn{\xi^{(j)}} and
#' eigenfunctions \eqn{\phi^{(j)}}, then eigendecomposes the joint covariance of
#' the (weighted) stacked scores. With component weights \eqn{w_j > 0} the
#' shared scores and multivariate eigenfunctions are
#' \deqn{s_{im} = \sum_j \sqrt{w_j} \sum_l [c_m]^{(j)}_l \xi^{(j)}_{il}, \qquad
#'   \Psi_m^{(j)} = \frac{1}{\sqrt{w_j}} \sum_l [c_m]^{(j)}_l \phi^{(j)}_l,}
#' where \eqn{c_m} are the eigenvectors of the weighted joint score covariance.
#'
#' The returned object is a [tfb_mv()] whose components are [tfb_fpc()] objects
#' sharing identical per-curve scores; cast it back to evaluations with
#' [as.tfd_mv()] / `vec_cast()`, and project **new** `tfd_mv` data onto the
#' fitted basis with [tf_rebase()]. Like the univariate FPCA, the estimator
#' targets data observed on a common grid per component; re-scoring new data
#' evaluates it on each component's estimation grid, so new curves must be
#' observable there.
#'
#' @param data a [tfd_mv()] / `tfb_mv` object, a (named) `list` of univariate
#'   `tf` vectors, or anything [tfd_mv()] accepts.
#' @param weights component weighting scheme for the joint analysis. Either a
#'   string -- `"inverse_variance"` (default; \eqn{w_j = 1 / \sum_l
#'   \lambda^{(j)}_l}, so each component contributes equal total variance),
#'   `"snr"` (signal-to-noise: retained variance over the discarded-variance
#'   tail of the univariate fit), or `"equal"` (\eqn{w_j = 1}) -- or a numeric
#'   vector of `d` non-negative weights. Weights are rescaled to sum to `d`
#'   (so `"equal"` gives all-ones).
#' @param pve proportion of variance explained used to truncate the
#'   *multivariate* components (default `0.995`). Ignored if `npc` is given.
#' @param npc number of multivariate FPCs to retain (overrides `pve`).
#' @param uni_pve proportion of variance explained for the *univariate* FPCA of
#'   each component (default `0.995`); forwarded as `pve` to the univariate
#'   `method`.
#' @param method univariate FPCA method, see [tfb_fpc()]. Defaults to
#'   [fpc_wsvd()].
#' @param ... further arguments forwarded to the univariate `method`. As in
#'   [tfb_mv()], a `...` argument given as a list named by the component names
#'   is distributed per component.
#' @returns a `tfb_mv` object whose `d` components are [tfb_fpc()] objects with
#'   shared per-curve scores; `is_tfb_mfpc()` is `TRUE` for it. Use
#'   [tf_mfpc_scores()] and [tf_mfpc_efunctions()] to extract the shared scores
#'   and the multivariate eigenfunctions.
#' @references `r format_bib("happ2018")`
#' @seealso [tfb_mv()] for independent per-component FPCA, [tfb_fpc()] /
#'   [fpc_wsvd()] for the univariate machinery.
#' @family tf_mv-class
#' @family tfb_fpc-class
#' @examples
#' set.seed(1)
#' g <- tfd_mv(list(hip = tf_rgp(20), knee = tf_rgp(20)))
#' m <- tfb_mfpc(g, pve = 0.99)
#' m
#' dim(tf_mfpc_scores(m))
#' tf_mfpc_efunctions(m)
#' # reconstruct and project new data:
#' plot(as.tfd_mv(m), type = "facet")
#' g_new <- tfd_mv(list(hip = tf_rgp(3), knee = tf_rgp(3)))
#' tf_rebase(g_new, m)
#' @rdname tfb_mfpc
#' @export
tfb_mfpc <- function(data, ...) UseMethod("tfb_mfpc")

#' @rdname tfb_mfpc
#' @export
tfb_mfpc.tf_mv <- function(
  data,
  weights = c("inverse_variance", "snr", "equal"),
  pve = 0.995,
  npc = NULL,
  uni_pve = 0.995,
  method = fpc_wsvd,
  ...
) {
  if (!tf_ncomp(data)) {
    cli::cli_abort("Can't compute MFPCA: {.arg data} has no components.")
  }
  fit <- mfpc_fit(
    data,
    weights = weights,
    pve = pve,
    npc = npc,
    uni_pve = uni_pve,
    method = method,
    ...
  )
  new_tf_mv(
    fit$components,
    domain = tf_domain(data),
    mfpc = fit$mfpc
  )
}

#' @rdname tfb_mfpc
#' @export
tfb_mfpc.list <- function(data, ...) {
  tfb_mfpc(tfd_mv(data), ...)
}

#' @rdname tfb_mfpc
#' @export
tfb_mfpc.default <- function(data, ...) {
  if (missing(data) || vec_size(data) == 0) {
    cli::cli_abort("Can't compute MFPCA on empty input.")
  }
  tfb_mfpc(tfd_mv(data), ...)
}

# Core fitter ------------------------------------------------------------------

# Returns a list with `components` (named list of shared-score tfb_fpc) and
# `mfpc` (the joint spec needed to re-score new data).
mfpc_fit <- function(
  data,
  weights = c("inverse_variance", "snr", "equal"),
  pve = 0.995,
  npc = NULL,
  uni_pve = 0.995,
  method = fpc_wsvd,
  ...
) {
  comps <- tf_components(data)
  comp_names <- attr(data, "comp_names")
  d <- length(comps)
  domain <- tf_domain(data)
  n <- vec_size(data)
  ids <- names(data) %||% as.character(seq_len(n))
  dots <- list(...)

  # 1. univariate FPCA per component (full spec incl. error_variance for SNR)
  specs <- map2(comps, comp_names, function(comp, nm) {
    pcd <- distribute_dots(dots, nm, comp_names)
    mfpc_uni_fit(comp, method = method, uni_pve = uni_pve, pcd = pcd)
  })

  # 2. component weights (rescaled to sum to d)
  w <- mfpc_weights(specs, weights, d = d, comp_names = comp_names)

  # 3. joint eigen-analysis of the weighted, stacked univariate scores
  scores_uni <- map(specs, "scores") # each n x M_j
  m_j <- map_int(scores_uni, ncol)
  xi <- do.call(cbind, scores_uni) # n x M_+
  sqrt_w_cols <- rep(sqrt(w), m_j)
  xi_w <- sweep(xi, 2, sqrt_w_cols, `*`)
  # Happ & Greven combine the *uncentered* univariate scores: they are
  # mean-zero by construction of the univariate FPCA (each time point is
  # centered), so the eigenvectors and the raw `xi_w %*% loadings` scores below
  # are mutually consistent and truncated reconstruction is the optimal rank-M
  # approximation. Using the sum-of-squares (not /(n-1)) keeps the multivariate
  # eigenvalues on the same scale as univariate tfb_fpc `score_variance`;
  # scaling the matrix by a constant leaves the eigenvectors unchanged.
  joint_ss <- crossprod(xi_w)
  eig <- eigen(joint_ss, symmetric = TRUE)
  evalues_all <- pmax(eig$values, 0)
  loadings_all <- eig$vectors # M_+ x M_+, columns are c_m
  m_keep <- mfpc_choose_npc(evalues_all, pve = pve, npc = npc)
  loadings <- loadings_all[, seq_len(m_keep), drop = FALSE]
  evalues <- evalues_all[seq_len(m_keep)]
  # shared scores: raw (uncentered) projection -> exact reconstruction at full
  # rank; mu absorbs the level, univariate scores are ~mean-zero.
  scores_shared <- xi_w %*% loadings # n x M

  # 4. multivariate eigenfunctions per component -> shared-score tfb_fpc
  block_end <- cumsum(m_j)
  block_start <- block_end - m_j + 1L
  basis_label <- paste0(m_keep, " MFPCs")
  components <- map(seq_len(d), function(j) {
    rows <- block_start[j]:block_end[j]
    # Psi_j = (1 / sqrt(w_j)) * phi_j %*% c^(j)   [n_arg_j x M]
    psi_j <- (specs[[j]]$efunctions %*% loadings[rows, , drop = FALSE]) /
      sqrt(w[j])
    basis_matrix <- cbind(specs[[j]]$mu, psi_j)
    new_tfb_fpc_shared(
      basis_matrix = basis_matrix,
      scores = scores_shared,
      arg = specs[[j]]$arg,
      domain = domain,
      evalues = evalues,
      ids = ids,
      scoring_function = mfpc_component_scoring,
      basis_label = basis_label
    )
  })
  names(components) <- comp_names

  # 5. compact spec for joint re-scoring of new data
  mfpc <- list(
    weights = w,
    weight_scheme = if (is.character(weights)) weights[1] else "user",
    loadings = loadings, # M_+ x M
    block_sizes = m_j,
    evalues = evalues,
    npc = m_keep,
    comp_names = comp_names,
    uni = map(specs, function(s) {
      list(
        efunctions = s$efunctions,
        mu = s$mu,
        arg = s$arg,
        scoring_function = s$scoring_function,
        evalues = s$evalues
      )
    })
  )
  list(components = components, mfpc = mfpc)
}

# univariate FPCA of one component, returning the full method spec plus `arg`.
mfpc_uni_fit <- function(comp, method, uni_pve, pcd) {
  comp_df <- tf_2_df(comp, arg = tf_arg(comp))
  arg <- sort_unique(comp_df$arg)
  margs <- get_args(c(list(pve = uni_pve), pcd), method)
  spec <- do.call(method, c(list(data = comp_df, arg = arg), margs))
  if (is.null(spec$scores) || is.null(spec$efunctions) || is.null(spec$mu)) {
    cli::cli_abort(
      "FPCA {.arg method} must return {.field mu}, {.field efunctions} and {.field scores}."
    )
  }
  spec$scores <- as.matrix(spec$scores)
  spec$efunctions <- as.matrix(spec$efunctions)
  if (
    nrow(spec$efunctions) != length(arg) ||
      length(spec$mu) != length(arg) ||
      ncol(spec$scores) != ncol(spec$efunctions) ||
      nrow(spec$scores) != vec_size(comp)
  ) {
    cli::cli_abort(
      "FPCA {.arg method} returned inconsistent dimensions for {.field mu} / {.field efunctions} / {.field scores}."
    )
  }
  spec$arg <- arg
  spec
}

# component weights, rescaled to sum to `d`.
mfpc_weights <- function(specs, weights, d, comp_names) {
  if (is.numeric(weights)) {
    assert_numeric(
      weights,
      len = d,
      finite = TRUE,
      any.missing = FALSE
    )
    # weights enter as 1/sqrt(w_j) in the eigenfunctions, so zero/negative
    # weights are not allowed (a zero would yield Inf basis functions).
    if (any(weights <= 0)) {
      cli::cli_abort("Numeric {.arg weights} must be strictly positive.")
    }
    w <- weights
  } else {
    weights <- match.arg(weights, c("inverse_variance", "snr", "equal"))
    # per-component total signal variance from the univariate scores
    total_var <- map_dbl(specs, function(s) {
      sum(apply(s$scores, 2, var))
    })
    w <- switch(
      weights,
      equal = rep(1, d),
      inverse_variance = {
        if (any(total_var <= 0)) {
          cli::cli_abort(
            "Can't use {.val inverse_variance} weights: a component has zero variance."
          )
        }
        1 / total_var
      },
      snr = {
        # signal-to-noise: total retained variance over discarded variance.
        # `fpc_wsvd()` returns `error_variance` as the *cumulative* tail sums of
        # the dropped eigenvalues, so its last element is the total discarded
        # variance (the noise floor).
        noise <- map_dbl(specs, function(s) {
          ev <- s$error_variance
          if (is.null(ev) || !length(ev)) NA_real_ else tail(ev, 1L)
        })
        if (anyNA(noise) || any(noise <= 0)) {
          cli::cli_abort(c(
            "Can't compute {.val snr} weights.",
            i = "The univariate fit left no discarded-variance tail (lower {.arg uni_pve}), or {.arg method} does not return {.field error_variance}."
          ))
        }
        total_var / noise
      }
    )
  }
  w <- w * d / sum(w)
  names(w) <- comp_names
  w
}

# choose number of multivariate components from positive eigenvalues.
mfpc_choose_npc <- function(evalues, pve, npc) {
  m_plus <- length(evalues)
  if (!is.null(npc)) {
    assert_count(npc, positive = TRUE)
    if (npc > m_plus) {
      cli::cli_warn(
        "Requested {.arg npc} = {npc} exceeds available {m_plus} components; using {m_plus}."
      )
    }
    return(min(as.integer(npc), m_plus))
  }
  assert_number(pve, lower = 0, upper = 1)
  total <- sum(evalues)
  if (total <= 0) {
    return(1L)
  }
  # tolerance guards against pve = 1 rounding cumsum/total to just below 1
  # (which would make which() empty); fall back to all components.
  keep <- which(cumsum(evalues) / total >= pve - 1e-12)
  if (!length(keep)) m_plus else min(keep)
}

# assemble a tfb_fpc from a precomputed (mean + eigenfunctions) basis and a
# score matrix -- mirrors the assembly in new_tfb_fpc() but takes the spec
# directly, so it can carry *shared* multivariate scores.
new_tfb_fpc_shared <- function(
  basis_matrix,
  scores,
  arg,
  domain,
  evalues,
  ids,
  scoring_function,
  basis_label
) {
  fpc_basis <- tfd(t(basis_matrix), arg = arg, domain = domain)
  fpc_constructor <- fpc_wrapper(fpc_basis)
  coefs <- cbind(1, scores)
  coef_list <- split(coefs, row(coefs))
  names(coef_list) <- ids
  new_vctr(
    coef_list,
    domain = domain,
    basis = fpc_constructor,
    basis_label = basis_label,
    basis_matrix = basis_matrix,
    arg = arg,
    score_variance = evalues,
    scoring_function = scoring_function,
    class = c("tfb_fpc", "tfb", "tf")
  )
}


# per-component scoring stub: scoring a *single* MFPC component is ill-defined
# (the eigenfunctions are orthonormal only in the joint weighted product), so
# direct use aborts with a pointer to the multivariate path.
mfpc_component_scoring <- function(...) {
  cli::cli_abort(c(
    "Can't score data onto a single component of a multivariate FPCA basis.",
    i = "Project the full {.cls tfd_mv} with {.fn tf_rebase} (or {.fn vec_cast}) onto the {.fn tfb_mfpc} object instead."
  ))
}

# Demote a `tfb_mfpc` to a plain `tfb_mv` by stripping the joint spec. Used by
# Math/Ops and `$<-` interceptors that warn the user the joint MFPC
# representation is being dropped before continuing along the standard
# `tfb_mv` path. Each shared-score component carries a rank-deficient
# multivariate eigenfunction basis (`Psi_j` lives in an M-dim space but only
# spans `rank <= ncol(phi_j)` directions in the per-component grid), so simply
# swapping the abort-stub `scoring_function` for `.fpc_wsvd_scores` is not
# enough: downstream `tf_rebase()` would fit data via `qr.coef()` on a
# rank-deficient `Psi_j` and return `NA` scores. Instead, rebuild each
# component as a fresh `tfb_fpc` on the *univariate* basis (the full-rank
# orthonormal `phi^(j)` from the univariate FPCA stored in `mfpc$uni`), with
# the component's own univariate `scoring_function` (or `.fpc_wsvd_scores` as
# fallback), so Math/Ops dispatched on `tfb_fpc` can re-fit scores without
# exploding.
tfb_mfpc_demote <- function(x) {
  mfpc <- attr(x, "mfpc")
  attr(x, "mfpc") <- NULL
  comps <- attr(x, "components")
  if (is.null(mfpc) || is.null(mfpc$uni)) {
    # nothing to rebuild against; leave components as-is.
    return(x)
  }
  new_comps <- map2(comps, mfpc$uni, new_tfb_fpc_demoted)
  names(new_comps) <- names(comps)
  attr(x, "components") <- new_comps
  x
}

# Rebuild a single shared-score MFPC component as a plain, full-rank `tfb_fpc`
# on the stored univariate eigenfunctions `phi^(j)`. The component's
# evaluations are reconstructed from the joint basis (`mu_j + Psi_j s^T`) and
# then re-scored onto `phi^(j)` using the univariate `scoring_function`.
new_tfb_fpc_demoted <- function(component, uni) {
  arg <- uni$arg
  mu_j <- uni$mu
  phi_j <- uni$efunctions
  scoring_function <- uni$scoring_function %||% .fpc_wsvd_scores
  # reconstruct evaluations on the univariate grid from the joint basis.
  joint_bm <- attr(component, "basis_matrix") # n_arg x (1 + M)
  coefs_old <- do.call(rbind, unclass(component)) # n x (1 + M)
  data_matrix <- coefs_old %*% t(joint_bm) # n x n_arg
  quad_w <- trapezoid_weights(arg)
  scores <- as.matrix(scoring_function(data_matrix, phi_j, mu_j, quad_w))
  basis_matrix <- unname(cbind(mu_j, phi_j))
  domain <- attr(component, "domain")
  fpc_basis <- suppressMessages(tfd(t(basis_matrix), arg = arg, domain = domain))
  fpc_constructor <- fpc_wrapper(fpc_basis)
  coefs <- cbind(1, scores)
  coef_list <- split(coefs, row(coefs))
  names(coef_list) <- names(component)
  basis_label <- paste0(ncol(phi_j), " FPCs")
  new_vctr(
    coef_list,
    domain = domain,
    basis = fpc_constructor,
    basis_label = basis_label,
    basis_matrix = basis_matrix,
    arg = arg,
    score_variance = uni$evalues %||% rep(NA_real_, ncol(phi_j)),
    scoring_function = scoring_function,
    class = c("tfb_fpc", "tfb", "tf")
  )
}

# Rebuild a single shared-score MFPC component that is assigned as a
# standalone vector (e.g. the `value` in `mf2$x <- mf$x`), so it does not
# retain the abort-stub `scoring_function`. The matching univariate FPCA fit
# is identified by the component's joint basis matrix among the parent fit's
# original components `comps`; components from a foreign fit (no match) are
# returned unchanged.
mfpc_demote_component_value <- function(value, comps, uni) {
  if (
    !is_tfb_fpc(value) ||
      !identical(attr(value, "scoring_function"), mfpc_component_scoring)
  ) {
    return(value)
  }
  match_k <- which(map_lgl(
    comps,
    \(co) identical(attr(co, "basis_matrix"), attr(value, "basis_matrix"))
  ))
  if (!length(match_k)) {
    return(value)
  }
  new_tfb_fpc_demoted(value, uni[[match_k[1]]])
}

# Internal warning shown when an operation forces a `tfb_mfpc` back to a plain
# per-component `tfb_fpc` (`tfb_mv`) representation. Centralised so the message
# stays consistent across Math/Ops, `$<-` and `vec_c()`. Uses class
# `tf_mfpc_demotion` so callers can intercept via `withCallingHandlers()`.
warn_mfpc_demotion <- function(reason) {
  cli::cli_warn(
    c(
      "Demoting to per-component {.cls tfb_fpc} representation; the joint MFPC spec is dropped.",
      i = reason,
      i = "Re-score with {.fn tf_rebase} for joint MFPC arithmetic."
    ),
    class = "tf_mfpc_demotion"
  )
}

# Joint re-scoring of new data onto a fitted MFPC basis ------------------------

# `newdata`: a tf_mv (tfd_mv/tfb_mv) compatible with `mfpc_obj`.
mfpc_rescore <- function(newdata, mfpc_obj, arg = NULL) {
  spec <- attr(mfpc_obj, "mfpc")
  if (is.null(spec)) {
    cli::cli_abort("{.arg mfpc_obj} is not a multivariate FPCA basis.")
  }
  if (!is.null(arg)) {
    cli::cli_abort(c(
      "Can't re-score onto a multivariate FPCA basis at a custom {.arg arg}.",
      i = "The basis is tied to its estimation grid; omit {.arg arg}."
    ))
  }
  check_compatible_mv(newdata, mfpc_obj)
  if (!isTRUE(all.equal(tf_domain(newdata), tf_domain(mfpc_obj)))) {
    cli::cli_abort(
      "{.arg newdata} domain {.val {tf_domain(newdata)}} does not match the MFPCA basis domain {.val {tf_domain(mfpc_obj)}}."
    )
  }
  new_comps <- tf_components(newdata)
  n_new <- vec_size(newdata)
  ids <- names(newdata) %||% as.character(seq_len(n_new))

  # univariate scores of the new data on each component's eigenfunctions
  xi_new <- map2(new_comps, spec$uni, function(comp, u) {
    if (is.null(u$scoring_function)) {
      cli::cli_abort(
        "The univariate FPCA {.arg method} did not provide a scoring function; can't re-score new data."
      )
    }
    comp_df <- tf_2_df(comp, arg = u$arg)
    mat <- df_2_mat(comp_df)
    # align columns to the stored eigenfunction grid
    idx <- match(u$arg, attr(mat, "arg"))
    if (anyNA(idx)) {
      cli::cli_abort(c(
        "New data does not cover the training argument grid of every component.",
        i = "Re-scoring requires each new curve to be observable on the component's eigenfunction grid."
      ))
    }
    mat <- mat[, idx, drop = FALSE]
    quad_w <- trapezoid_weights(u$arg)
    as.matrix(u$scoring_function(mat, u$efunctions, u$mu, quad_w))
  })
  xi_new <- do.call(cbind, xi_new)
  sqrt_w_cols <- rep(sqrt(spec$weights), spec$block_sizes)
  xi_w <- sweep(xi_new, 2, sqrt_w_cols, `*`)
  scores_new <- xi_w %*% spec$loadings # n_new x M

  # rebuild each component with the stored Psi basis + the new shared scores
  base_comps <- tf_components(mfpc_obj)
  out <- map(seq_along(base_comps), function(j) {
    b <- base_comps[[j]]
    new_tfb_fpc_shared(
      basis_matrix = attr(b, "basis_matrix"),
      scores = scores_new,
      arg = attr(b, "arg"),
      domain = attr(b, "domain"),
      evalues = attr(b, "score_variance"),
      ids = ids,
      scoring_function = mfpc_component_scoring,
      basis_label = attr(b, "basis_label")
    )
  })
  names(out) <- spec$comp_names
  new_tf_mv(out, domain = tf_domain(mfpc_obj), mfpc = spec)
}

# Predicate and accessors ------------------------------------------------------

#' @rdname tfb_mfpc
#' @param x a `tfb_mv` object, ideally one returned by [tfb_mfpc()].
#' @returns `is_tfb_mfpc()`: a logical flag.
#' @export
is_tfb_mfpc <- function(x) is_tfb_mv(x) && !is.null(attr(x, "mfpc"))

#' @rdname tfb_mfpc
#' @returns `tf_mfpc_scores()`: an `n x M` matrix of shared multivariate FPC
#'   scores (rows = curves, columns = components).
#' @export
tf_mfpc_scores <- function(x) {
  if (!is_tfb_mfpc(x)) {
    cli::cli_abort(
      "{.arg x} is not a multivariate FPCA basis (see {.fn tfb_mfpc})."
    )
  }
  coefs <- unclass(tf_component(x, 1L))
  scores <- do.call(rbind, lapply(coefs, function(co) co[-1L]))
  rownames(scores) <- names(x)
  colnames(scores) <- paste0("mfpc", seq_len(ncol(scores)))
  scores
}

#' @rdname tfb_mfpc
#' @returns `tf_mfpc_efunctions()`: a `tfd_mv` of length `M` holding the
#'   multivariate eigenfunctions \eqn{\Psi_m} (one "curve" per component).
#' @export
tf_mfpc_efunctions <- function(x) {
  if (!is_tfb_mfpc(x)) {
    cli::cli_abort(
      "{.arg x} is not a multivariate FPCA basis (see {.fn tfb_mfpc})."
    )
  }
  comps <- tf_components(x)
  ef_comps <- map(comps, function(b) {
    psi <- attr(b, "basis_matrix")[, -1L, drop = FALSE]
    tfd(t(psi), arg = attr(b, "arg"), domain = attr(b, "domain"))
  })
  names(ef_comps) <- attr(x, "comp_names")
  ef <- new_tf_mv(ef_comps, domain = tf_domain(x))
  names(ef) <- paste0("mfpc", seq_len(attr(x, "mfpc")$npc))
  ef
}
