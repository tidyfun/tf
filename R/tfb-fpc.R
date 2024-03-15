new_tfb_fpc <- function(data, domain = NULL,
                       method = NULL, basis_from = NULL, ...) {
  if (all(dim(data) == 0)) {
    ret <- vctrs::new_vctr(
      data,
      domain = domain %||% numeric(2),
      arg = numeric(),
      score_variance = numeric(),
      class = c("tfb_fpc", "tfb", "tf"))
    return(ret)
  }
  if (!is.null(method) && !is.null(basis_from)) {
    stop("Can't specify both method *and* basis_from for new_tfb_fpc",
         call. = FALSE)
  }
  arg <- mgcv::uniquecombs(data$arg, ordered = TRUE) |> unlist()

  domain <- domain %||% range(arg)
  if (!isTRUE(all.equal(domain, range(arg)))) {
    warning(
      "domain for tfb_fpc can't be larger than observed arg-range --",
      " extrapolating FPCs is a bad idea.\n domain reset to [", min(arg),
      ",", max(arg), "]",
      call. = FALSE
    )
    domain <- range(arg)
  }

  if (is.null(basis_from)) {
    fpc_args <- get_args(list(...), method)
    fpc_args <- c(fpc_args, list(data = data, arg = arg))
    fpc_spec <- do.call(method, fpc_args)
    basis_matrix <- cbind(fpc_spec$mu, fpc_spec$efunctions)
    basis_label <- paste0(fpc_spec$npc, " FPCs")
    score_variance <- fpc_spec$evalues
    scoring_function <- fpc_spec$scoring_function
    scores <- fpc_spec$scores
  } else {
    basis_matrix <- tf_basis(basis_from)(arg) |> as.matrix()
    basis_label <- attr(basis_from, "basis_label")
    score_variance <-  attr(basis_from, "score_variance")
    scoring_function <- attr(basis_from, "scoring_function")

    # trapezoid integration weights: #TODO generally appropriate or just for wsvd?
    delta <- c(0, diff(arg))
    weights <- 0.5 * c(delta[-1] + head(delta, -1), tail(delta, 1))
    scores <- scoring_function(df_2_mat(data),
                               basis_matrix[, -1], basis_matrix[, 1], weights) #!!
  }
  fpc_basis <- tfd(t(basis_matrix),
                   arg = arg, domain = domain)
  fpc_constructor <- fpc_wrapper(fpc_basis)
  coef_list <- split(cbind(1, scores), row(cbind(1, scores)))
  names(coef_list) <- levels(as.factor(data$id))


  vctrs::new_vctr(coef_list,
    domain = domain,
    basis = fpc_constructor,
    basis_label = basis_label,
    basis_matrix = basis_matrix,
    arg = arg,
    score_variance = score_variance,
    # scoring_fct expects data, weights, mean, efunctions -- for tf_rebase
    scoring_function = scoring_function,
    class = c("tfb_fpc", "tfb", "tf")
  )
}

#-------------------------------------------------------------------------------

#' Functional data in FPC-basis representation
#'
#' These functions perform a (functional) principal component analysis (FPCA) of
#' the input data and return an `tfb_fpc` `tf`-object that uses the empirical
#' eigenfunctions as basis functions for representing the data. The default
#' ("`method = fpc_wsvd`") uses a (truncated) weighted SVD for complete
#' data on a common grid and a nuclear-norm regularized (truncated) weighted SVD
#' for partially missing data on a common grid, see [fpc_wsvd()].
#' The latter is likely to break down for high PVE and/or high amounts of
#' missingness.\cr
#'
#' For the FPC basis, any factorization method that accepts a `data.frame` with
#' columns `id`, `arg`, `value` containing the functional data and returns a
#' list with eigenfunctions and FPC scores structured like the return object
#' of [fpc_wsvd()] can be used for the `method`` argument, see example below.
#' Note that the mean function, with a fixed "score" of 1 for all functions,
#' is used as the first basis function for all FPC bases.
#'
#' @export
#' @param method the function to use that computes eigenfunctions and scores.
#'   Defaults to [fpc_wsvd()], which is quick and easy but returns completely
#'   unsmoothed eigenfunctions unlikely to be suited for noisy data.
#' @param ... arguments to the `method` which computes the
#'  (regularized/smoothed) FPCA - see e.g. [fpc_wsvd()].
#'  Unless set by the user, uses proportion of variance explained
#'  `pve = 0.995` to determine the truncation levels.
#' @inheritParams tfb
#' @returns an object of class `tfb_fpc`, inheriting from `tfb`.
#'    The basis used by `tfb_fpc` is a `tfd`-vector containing the estimated
#'    mean and eigenfunctions.
#' @seealso [fpc_wsvd()] for FPCA options.
#' @rdname tfb_fpc
#' @export
#' @family tfb-class
#' @family tfb_fpc-class
tfb_fpc <- function(data, ...) UseMethod("tfb_fpc")

#' @rdname tfb_fpc
#' @export
#' @inheritParams tfd.data.frame
#' @examples
#' set.seed(13121)
#' x <- tf_rgp(25, nugget = .02)
#' x_pc <- tfb_fpc(x, pve = .9)
#' x_pc
#' plot(x, lwd = 3)
#' lines(x_pc, col = 2, lty = 2)
#' x_pc_full <- tfb_fpc(x, pve = .995)
#' x_pc_full
#' lines(x_pc_full, col = 3, lty = 2)
#' # partially missing data on common grid:
#' x_mis <- x |> tf_sparsify(dropout = .05)
#' x_pc_mis <- tfb_fpc(x_mis, pve = .9)
#' x_pc_mis
#' plot(x_mis, lwd = 3)
#' lines(x_pc_mis, col = 4, lty = 2)
#' # extract FPC basis --
#' # first "eigenvector" in black is (always) the mean function
#' x_pc |> tf_basis(as_tfd = TRUE) |> plot(col = 1:5)
#' \donttest{
#' # Apply FPCA for sparse, irregular data using refund::fpca.sc:
#' set.seed(99290)
#' # create small, sparse, irregular data:
#' x_irreg <- x[1:8] |>
#'   tf_jiggle() |> tf_sparsify(dropout = 0.3)
#' plot(x_irreg)
#' x_df <- x_irreg |>
#'   as.data.frame(unnest = TRUE)
#' # wrap refund::fpca_sc for use as FPCA method in tfb_fpc:
#' fpca_sc_wrapper <- function(data, arg, pve = 0.995, ...) {
#'   data_mat <- tfd(data) |> as.matrix(interpolate = TRUE)
#'   fpca <- refund::fpca.sc(
#'     Y = data_mat, argvals = attr(data_mat, "arg"), pve = pve, ...
#'   )
#'   c(fpca[c("mu", "efunctions", "scores", "npc")],
#'     scoring_function = tf:::.fpc_wsvd_scores)
#' }
#' x_pc <- tfb_fpc(x_df, method = fpca_sc_wrapper)
#' lines(x_pc, col = 2, lty = 2)
#' }
tfb_fpc.data.frame <- function(data, id = 1, arg = 2, value = 3,
                               domain = NULL, method = fpc_wsvd, ...) {
  data <- df_2_df(data, id, arg, value)
  new_tfb_fpc(data, domain = domain, method = method, ...)
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.matrix <- function(data, arg = NULL, domain = NULL,
                           method = fpc_wsvd, ...) {
  arg <- unlist(find_arg(data, arg))
  names_data <- rownames(data)
  data <- mat_2_df(data, arg)
  ret <- new_tfb_fpc(data, domain = domain, method = method, ...)
  setNames(ret, names_data)
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.numeric <- function(data, arg = NULL, domain = NULL,
                            method = fpc_wsvd, ...) {
  data <- t(as.matrix(data))
  tfb_fpc(data = data, arg = arg, method = method, domain = domain, ...)
}

#' @rdname tfb_fpc
#' @export
tfb_fpc.tf <- function(data, arg = NULL, method = fpc_wsvd, ...) {
  # TODO: major computational shortcuts possible here for tfb-inputs:
  #   reduced rank, direct inner prods of basis functions etc...
  arg <- arg %||% tf_arg(data)
  names_data <- names(data)
  ret <- tfb_fpc(
    tf_2_df(data, arg = arg),
    method = method,
    domain = tf_domain(data), ...
  )
  setNames(ret, names_data)
}

#' @export
#' @describeIn tfb_fpc convert `tfb`: default method, returning prototype when
#'   data is NULL
tfb_fpc.default <- function(data, arg = NULL, domain = NULL, method = fpc_wsvd,
                            ...) {
  if (!missing(data)) {
    message("input `data` not recognized class;\nreturning prototype of length 0")
  }

  data <- data.frame()
  new_tfb_spline(data = data, arg = arg, method = method, domain = domain,
                 ...)
}
