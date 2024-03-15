# create string representation for a tf.
string_rep_tf <- function(f, signif_arg = NULL,
                          show = 3, digits = NULL, ...) {
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
    ensure_list(tf_arg(f)), show,
    \(x, y) do.call(format, c(format_args, list(x = x[1:y])))
  )
  value_ch <- map2(
    tf_evaluations(f), show,
    \(x, y) do.call(format, c(format_args, list(x = x[1:y])))
  )
  arg_nchar <- map(arg_ch, nchar) |>
    unlist() |>
    max()
  value_nchar <- map(value_ch, nchar) |>
    unlist() |>
    max()
  # left-pad with spaces:
  arg_ch <- map(arg_ch, \(x) sprintf(paste0("%", arg_nchar, "s"), x))
  value_ch <- map(value_ch, \(x) sprintf(paste0("%", value_nchar, "s"), x))
  str <- map2(
    arg_ch, value_ch, \(x, y) paste(paste0("(", x, ",", y, ")"), collapse = ";")
  )
  str <- pmap(
    list(str, arg_len, show), \(x, y, z) ifelse(y > z, paste0(x, "; ..."), x)
  )
  str <- map_if(str, grepl("NA)", str, fixed = TRUE), \(x) "NA")
  map_if(str, grepl("NULL", str, fixed = TRUE), \(x) "NA")
}

#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#'
#' Print/format `tf`-objects.
#'
#' @rdname tfdisplay
#' @param n how many elements of `x` to print out
#' @returns prints out `x` and returns it invisibly
#' @export
#' @family tidyfun print
print.tf <- function(x, n = 5, ...) {
  cat(paste0(
    ifelse(is_irreg(x), "irregular ", ""), class(x)[2], "[", length(x), "] on (", tf_domain(x)[1], ",",
    tf_domain(x)[2], ")"
  ))
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_reg <- function(x, n = 5, ...) {
  NextMethod()
  cat(" based on", length(tf_arg(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[seq_len(min(n, length(x)))], ...), sep = "\n")
    if (n < length(x)) {
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
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
  if (length(n_evals)) {
    cat(paste0(
      " based on ", min(n_evals), " to ", max(n_evals), " (mean: ",
      round(mean(n_evals)), ") evaluations each\n"
    ))
  } else cat(" (irregular) \n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[seq_len(min(n, length(x)))], ...), sep = "\n")
    if (n < length(x)) {
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    }
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfb <- function(x, n = 5, ...) {
  NextMethod()
  cat(" in basis representation:\n using ", attr(x, "basis_label"), "\n")
  if (length(x)) {
    cat(format(x[seq_len(min(n, length(x)))], ...), sep = "\n")
    if (n < length(x)) {
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    }
    invisible(x)
  }
}

#' @rdname tfdisplay
#' @inheritParams base::format.default
#' @param prefix used internally.
#' @export
format.tf <- function(x, digits = 2, nsmall = 0, width = options()$width,
                      n = 5, prefix = TRUE, ...) {
  long <- length(x) > n
  if (long && width > 0 && width <= 30) {
    x <- head(x, n)
  }
  resolution <- get_resolution(tf_arg(x))
  str <- string_rep_tf(x,
    signif_arg = abs(floor(log10(resolution))),
    digits = digits, nsmall = nsmall, ...
  )
  if (prefix) {
    prefix <- if (!is.null(names(x)) && all(nzchar(names(x), keepNA = TRUE))) {
      names(x)[seq_along(str)]
    } else {
      paste0("[", seq_along(str), "]")
    }
    str <- map2(prefix, str, \(x, y) paste0(x, ": ", y))
  }
  unlist(map_if(
    str, \(x) nchar(x) > width, \(x) paste0(substr(x, 1, width - 3), "...")
  ))
}
