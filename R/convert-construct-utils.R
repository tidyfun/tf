#-------------------------------------------------------------------------------

# replaces functionality of tf_unnest.tf
# turn a tf object into a data.frame evaluated on arg with cols id-arg-value
tf_2_df <- function(tf, arg, interpolate = TRUE, ...) {
  assert_class(tf, "tf")
  if (missing(arg)) {
    arg <- tf_arg(tf)
  }
  arg <- ensure_list(arg)
  assert_arg(arg, tf)

  tmp <- do.call(rbind,
    args = tf[, arg, matrix = FALSE, interpolate = interpolate]
  )
  n_evals <- lengths(arg)
  tmp$id <-
    if (length(n_evals) == 1) {
      rep(unique_id(names(tf)) %||% seq_along(tf), each = n_evals)
    } else {
      rep(unique_id(names(tf)) %||% seq_along(tf), times = n_evals)
    }
  # factor id avoids reordering of rows in tfb_fpc constructor and elsewhere..
  tmp$id <- factor(tmp$id, unique(tmp$id))
  tmp[, c("id", "arg", "value")]
}


# from refund
#' @importFrom stats complete.cases filter
df_2_mat <- function(data, binning = FALSE, maxbins = 1000) {
  data <- data[complete.cases(data), ]
  nobs <- length(unique(data$id))
  newid <- as.numeric(as.factor(data$id))
  bins <- sort(unique(data$arg))
  if (binning && (length(bins) > maxbins)) {
    binvalues <- seq(
      (1 - 0.001 * sign(bins[1])) * bins[1],
      (1 + 0.001 * sign(bins[length(bins)])) * bins[length(bins)],
      length.out = maxbins + 1
    )
    bins <- binvalues
    binvalues <- head(stats::filter(binvalues, c(0.5, 0.5)), -1)
  } else {
    binvalues <- bins
    bins <- c(
      (1 - 0.001 * sign(bins[1])) * bins[1], bins[-length(bins)],
      (1 + 0.001 * sign(bins[length(bins)])) * bins[length(bins)]
    )
    if (bins[1] == 0) {
      bins[1] <- -0.001
    }
    if (bins[length(bins)] == 0) {
      bins[length(bins)] <- 0.001
    }
  }
  newindex <- cut(data$arg, breaks = bins, include.lowest = TRUE)
  data_mat <- matrix(NA, nrow = nobs, ncol = nlevels(newindex))
  colnames(data_mat) <- binvalues
  attr(data_mat, "arg") <- binvalues
  data_mat[cbind(newid, as.numeric(newindex))] <- data$value
  return(data_mat)
}

#-------------------------------------------------------------------------------
# input homogenizers for tfb_spline:

df_2_df <- function(data, id = 1, arg = 2, value = 3) {
  data <- na.omit(data[, c(id, arg, value)])
  stopifnot(
    nrow(data) > 0,
    is.numeric(data[[arg]]),
    is.numeric(data[[value]])
  )
  colnames(data) <- c("id", "arg", "value")
  data
}

mat_2_df <- function(x, arg) {
  stopifnot(
    is.numeric(x), is.matrix(x),
    is.numeric(arg), length(arg) == ncol(x)
  )

  id <- unique_id(rownames(x)) %||% seq_len(dim(x)[1])
  id <- ordered(id, levels = unique(id))
  df_2_df(data.frame(
    # use t(x) here so that order of vector remains unchanged...
    id = id[col(t(x))], arg = arg[row(t(x))],
    value = as.vector(t(x)),
    stringsAsFactors = FALSE
  ))
}
