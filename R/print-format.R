# safe range of evaluations, returning c(NA, NA) when all entries are NA/NULL
safe_range_evals <- function(x) {
  evals <- unlist(tf_evaluations(x), use.names = FALSE)
  if (is.null(evals) || length(evals) == 0 || allMissing(evals)) {
    c(NA, NA)
  } else {
    range(evals, na.rm = TRUE)
  }
}

# create string representation for a tf.
string_rep_tf <- function(f, signif_arg = NULL, show = 3, digits = NULL, ...) {
  digits_eval <- digits %||% options()$digits
  digits_arg <- min(digits_eval, signif_arg %||% digits_eval)
  arg_len <- lengths(ensure_list(tf_arg(f)))
  show <- as.list(pmin(show, arg_len))
  # fix weird dots handling by map/format:
  format_args <- modifyList(
    tail(head(formals(format.default), -1), -1),
    list(digits = digits_arg, justify = "right", ...)
  )
  arg_ch <- map2(
    ensure_list(tf_arg(f)),
    show,
    \(x, y) do.call(format, c(format_args, list(x = x[1:y])))
  )
  value_ch <- map2(
    tf_evaluations(f),
    show,
    \(x, y) do.call(format, c(format_args, list(x = x[1:y])))
  )
  arg_nchar <- unlist(arg_ch, use.names = FALSE) |>
    nchar() |>
    max()
  value_nchar <- unlist(value_ch, use.names = FALSE) |>
    nchar() |>
    max()
  # left-pad with spaces:
  arg_ch <- map(arg_ch, \(x) sprintf(paste0("%", arg_nchar, "s"), x))
  value_ch <- map(value_ch, \(x) sprintf(paste0("%", value_nchar, "s"), x))
  str <- map2(
    arg_ch,
    value_ch,
    \(x, y) paste(paste0("(", x, ",", y, ")"), collapse = ";")
  )
  str <- pmap(
    list(str, arg_len, show),
    \(x, y, z) ifelse(y > z, paste0(x, "; ..."), x)
  )
  str <- map_if(str, grepl("NA)", str, fixed = TRUE), \(x) "NA")
  map_if(str, grepl("NULL", str, fixed = TRUE), \(x) "NA")
}

# create a (binned) sparkline representation for a tfd_reg or tfb on
# equidistant grids
spark_rep_tf <- function(
  f,
  bins = -1,
  scale_f = NULL
) {
  arg <- tf_arg(f)
  gridlength <- length(arg)
  if (bins < 0 || bins >= gridlength) {
    evals <- tf_evaluations(f)
  } else {
    binwidth <- ceiling(gridlength / bins)
    use_index <- seq(binwidth, gridlength, length = bins) |>
      round() |>
      unique()
    nas <- is.na(f)
    evals <- vector("list", length(f))
    if (any(!nas)) {
      evals[!nas] <- f[!nas] |>
        tf_smooth(
          method = "rollmean",
          k = binwidth,
          align = "right",
          fill = "extend",
          verbose = FALSE
        ) |>
        tf_evaluations() |>
        map(`[`, use_index)
    }
  }
  if (is.null(scale_f)) {
    evals_unlisted <- unlist(evals, use.names = FALSE)
    if (
      !is.null(evals_unlisted) &&
        length(evals_unlisted) > 0 &&
        any(!is.na(evals_unlisted))
    ) {
      scale_f <- range(evals_unlisted, na.rm = TRUE)
    } else {
      scale_f <- c(0, 1)
    }
  }
  # handle constant functions
  range_f <- diff(scale_f)
  if (range_f == 0 || !is.finite(range_f)) {
    scaled <- map(evals, function(x) rep(0.5, length(x)))
  } else {
    scaled <- map(evals, function(x) {
      ((x - scale_f[1]) / range_f) |>
        # avoid floating point errors that show up as gaps in sparkline:
        pmin(1) |>
        pmax(0)
    })
  }
  sparks <- map(scaled, cli::spark_bar)
  sparks[is.na(f)] <- "NA"
  sparks
}


#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#'
#' Prints and formats `tf`-objects for display. See details / examples for options that
#' give finer control.
#'
#' By default, `tf` objects on regular grids are shown as
#' "sparklines" ([cli::spark_bar()]), set `sparkline = FALSE` for a text
#' representation.
#'
#' Sparklines are based on running mean values of the function values, but
#' these don't check for non-equidistant grids, so the visual impression will be
#' misleading for very unequal grid distances.
#'
#' Sparklines use
#' `options()$width/3` bins for printing/formatting by default, use `bins`
#' argument to set the number of bins explicitly.
#' For [pillar::glimpse()], we use 8 bins by default for compact display.
#' @rdname tfdisplay
#' @param n how many elements of `x` to print out at most, defaults to `6`.
#' @param ... handed over to [format.tf()].
#' @returns **`print`**: prints out `x` and returns it invisibly.
#' @export
#' @family tidyfun print
#' @examples
#' t <- seq(0, 1, l = 201)
#' cosine <- lapply(1:4, \(i) cos(i * pi * t)) |> tfd(arg = t)
#' cosine
#' tf_sparsify(cosine, dropout = .8)
#'
#' format(cosine, sparkline = FALSE)
#' format(cosine, bins = 5)
#' format(cosine, bins = 40)
#'
#' #! very non-equidistant grids --> sparklines can mislead about actual shapes:
#' tfd(cosine, arg = t^3)
print.tf <- function(x, n = 6, ...) {
  domain <- tf_domain(x) |> map_chr(format, ...)
  range <- safe_range_evals(x)
  range <- range |> map_chr(format, ...) |> suppressWarnings()
  cat(paste0(
    ifelse(is_irreg(x), "irregular ", ""),
    class(x)[2],
    "[",
    length(x),
    "]: [",
    domain[1],
    ",",
    domain[2],
    "] -> [",
    range[1],
    ",",
    range[2],
    "]"
  ))
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_reg <- function(x, n = 6, ...) {
  NextMethod()
  cat(" based on", length(tf_arg(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  len <- length(x)
  if (len > 0) {
    range_ <- safe_range_evals(x)
    scale_ <- if (any(!is.na(range_))) range_ else NULL
    format(x[seq_len(min(n, len))], scale_f = scale_, prefix = TRUE, ...) |>
      cat(sep = "\n")
    if (n < len) {
      cat(paste0("    [....]   (", len - n, " not shown)\n"))
    }
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_irreg <- function(x, n = 6, ...) {
  NextMethod()
  nas <- is.na(x)
  n_evals <- tf_count(x[!nas])
  if (length(n_evals) > 0) {
    cat(paste0(
      " based on ",
      min(n_evals),
      " to ",
      max(n_evals),
      " (mean: ",
      round(mean(n_evals)),
      ") evaluations each\n"
    ))
  } else {
    cat(" (irregular) \n")
  }
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  len <- length(x)
  if (len > 0) {
    format(x[seq_len(min(n, len))], prefix = TRUE, ...) |>
      cat(sep = "\n")
    if (n < len) {
      cat(paste0("    [....]   (", len - n, " not shown)\n"))
    }
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfb <- function(x, n = 5, ...) {
  NextMethod()
  cat(" in basis representation")
  len <- vec_size(x)
  if (len > 0) {
    range_ <- safe_range_evals(x)
    scale_ <- if (any(!is.na(range_))) range_ else NULL
    cat(":\n using ", attr(x, "basis_label"), attr(x, "family_label"), "\n")
    format(x[seq_len(min(n, len))], scale_f = scale_, prefix = TRUE, ...) |>
      cat(sep = "\n")
    if (n < len) {
      cat(paste0("    [....]   (", len - n, " not shown)\n"))
    }
    invisible(x)
  } else {
    cat(".\n")
  }
}

#' @rdname tfdisplay
#' @inheritParams base::format.default
#' @param prefix prefix with names / index positions? defaults to `FALSE`.
#' @param sparkline use a sparkline representation? defaults to `TRUE`
#'   (not available for irregular data).
#' @returns a character representation of `x`.
#' @export
format.tf <- function(
  x,
  digits = 2,
  nsmall = 0,
  width = options()$width,
  sparkline = TRUE,
  prefix = FALSE,
  ...
) {
  if (is_irreg(x) || allMissing(x) || !cli::is_utf8_output() || !sparkline) {
    resolution <- get_resolution(tf_arg(x))
    signif_arg <- abs(floor(log10(resolution)))
    str <- string_rep_tf(
      x,
      signif_arg = if (signif_arg == 0) 1 else signif_arg,
      digits = digits,
      nsmall = nsmall,
      ...
    )
  } else {
    dots <- list(...)
    str <- spark_rep_tf(
      x,
      bins = dots$bins %||% floor(width / 3),
      scale_f = dots$scale_f %||% safe_range_evals(x)
    )
  }

  if (prefix) {
    prefix <- if (
      !is.null(names(x)) && isTRUE(all(nzchar(names(x), keepNA = TRUE)))
    ) {
      names(x)[seq_along(str)]
    } else {
      paste0("[", seq_along(str), "]")
    }
    max_length <- max(nchar(prefix))
    str <- format(prefix, justify = "left") |>
      map2(str, \(x, y) paste0(x, ": ", y))
  }
  unlist(
    map_if(
      str,
      \(x) nchar(x) > width,
      \(x) paste0(substr(x, 1, width - 3), "...")
    ),
    use.names = FALSE
  )
}

#' @export
vec_ptype_abbr.tfd_reg <- function(x, ...) "tfd_reg"

#' @export
vec_ptype_abbr.tfd_irreg <- function(x, ...) "tfd_irreg"

#' @export
vec_ptype_abbr.tfb_spline <- function(x, ...) "tfb_spline"

#' @export
vec_ptype_abbr.tfb_fpc <- function(x, ...) "tfb_fpc"

# dynamically exported in zzz.R:
format_glimpse.tf <- function(
  x,
  digits = 2,
  nsmall = 0,
  prefix = FALSE,
  ...
) {
  dots <- list(...)
  if (is.null(dots$bins)) {
    dots$bins <- 8 #compact display by default
  }
  do.call(
    format.tf,
    c(list(x, digits = digits, nsmall = nsmall, prefix = FALSE), dots)
  )
}
