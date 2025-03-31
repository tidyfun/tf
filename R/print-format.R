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
spark_rep_tf <- function(f, bins = -1, scale_f = range(unlist(evals))) {
  arg <- tf_arg(f)
  gridlength <- length(arg)
  if (bins < 0 || bins >= gridlength) {
    evals <- tf_evaluations(f)
  } else {
    binwidth <- ceiling(gridlength / bins)
    use_index <- seq(binwidth, gridlength, length = bins) |>
      round() |>
      unique()
    evals <- f |>
      tf_smooth(method = "rollmean", k = binwidth, align = "right") |>
      tf_evaluations() |>
      map(`[`, use_index) |>
      suppressMessages()
  }
  scaled <- map(evals, function(x) {
    ((x - scale_f[1]) / diff(scale_f)) |>
      # avoid floating point errors that show up as gaps in sparkline:
      pmin(1) |>
      pmax(0)
  })
  sparks <- map(scaled, cli::spark_bar)
  sparks[is.na(f)] <- "NA"
  sparks
}


#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#'
#' Prints and formats `tf`-objects for display. See Details / examples for options that
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
#' For [tibble::glimpse()], we use 8 bins by default for compact display.
#' @rdname tfdisplay
#' @param n how many elements of `x` to print out at most
#' @param ... handed over to [format.tf()]
#' @returns **`print`**: prints out `x` and returns it invisibly
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
print.tf <- function(x, n = 5, ...) {
  domain <- tf_domain(x) |> sapply(format, ...)
  range <- range(tf_evaluations(x)) |> sapply(format, ...)
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
print.tfd_reg <- function(x, n = 5, ...) {
  NextMethod()
  cat(" based on", length(tf_arg(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  len <- length(x)
  if (len > 0) {
    scale_ <- range(tf_evaluations(x))
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
print.tfd_irreg <- function(x, n = 5, ...) {
  NextMethod()
  nas <- map_lgl(tf_evaluations(x), \(x) length(x) == 1 && all(is.na(x)))
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
  len <- length(x)
  if (len > 0) {
    scale_ <- range(tf_evaluations(x))
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
#' @param prefix prefix with names / index positions? defaults to `FALSE`
#' @param sparkline use a sparkline representation? defaults to `TRUE`
#'   (not available for irregular data)
#' @returns a character representation of `x`
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
  if (is_irreg(x) || !cli::is_utf8_output() || !sparkline) {
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
    str <- spark_rep_tf(
      x,
      bins = list(...)$bins %||% floor(width / 3),
      scale_f = list(...)$scale_f %||% range(tf_evaluations(x))
    )
  }

  if (prefix) {
    prefix <- if (!is.null(names(x)) && all(nzchar(names(x), keepNA = TRUE))) {
      names(x)[seq_along(str)]
    } else {
      paste0("[", seq_along(str), "]")
    }
    str <- map2(prefix, str, \(x, y) paste0(x, ": ", y))
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
