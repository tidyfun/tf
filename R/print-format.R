string_rep_tf <- function(f, signif_arg = NULL,
                          show = 3, digits = NULL, ...) {
  digits_eval <- digits %||% options()$digits
  digits_arg <- min(digits_eval, signif_arg %||% digits_eval)
  arg_len <- map(ensure_list(tf_arg(f)), length)
  show <- as.list(pmin(show, unlist(arg_len)))
  # fix weird dots handling by map/format:
  format_args <- modifyList(
    tail(head(formals(format.default), -1), -1),
    list(digits = digits_arg, justify = "right", ...)
  )
  arg_ch <- map2(
    ensure_list(tf_arg(f)), show,
    ~do.call(format, c(format_args, list(x = .x[1:.y])))
  )
  value_ch <- map2(
    tf_evaluations(f), show,
    ~do.call(format, c(format_args, list(x = .x[1:.y])))
  )
  arg_nchar <- map(arg_ch, ~nchar(.x)) |> unlist() |> max()
  value_nchar <- map(value_ch, ~nchar(.x)) |> unlist() |> max()
  #left-pad with spaces:
  arg_ch <- map(arg_ch, ~ sprintf(paste0("%", arg_nchar, "s"), .)) 
  value_ch <- map(value_ch, ~ sprintf(paste0("%", value_nchar, "s"), .))
  str <- map2(
    arg_ch, value_ch,
    ~paste(paste0("(", .x, ",", .y, ")"), collapse = ";")
  )
  str <- pmap(
    list(str, arg_len, show),
    ~ifelse(..2 > ..3, paste0(..1, "; ..."), ..1)
  )
  map_if(str, grepl("NA\\)", str), ~{
    "NA"
  })
}

#-------------------------------------------------------------------------------

#' Pretty printing and formatting for functional data
#'
#' Print/format `tf`-objects.
#'
#' @rdname tfdisplay
#' @param n how many elements of `x` to print out
#' @export
#' @family tidyfun print
print.tf <- function(x, n = 10, ...) {
  cat(paste0(
    class(x)[2], "[", length(x), "] on (", tf_domain(x)[1], ",",
    tf_domain(x)[2], ")"
  ))
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_reg <- function(x, n = 10, ...) {
  NextMethod()
  cat(" based on", length(tf_arg(x)), "evaluations each\n")
  cat("interpolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) {
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    }
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfd_irreg <- function(x, n = 10, ...) {
  NextMethod()
  nas <- map_lgl(tf_evaluations(x), ~length(.) == 1 && all(is.na(.)))
  n_evals <- tf_count(x[!nas])
  cat(paste0(
    " based on ", min(n_evals), " to ", max(n_evals), " (mean: ",
    round(mean(n_evals)), ") evaluations each\n"
  ))
  cat("inter-/extrapolation by", attr(x, "evaluator_name"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
    if (n < length(x)) {
      cat(paste0("    [....]   (", length(x) - n, " not shown)\n"))
    }
  }
  invisible(x)
}

#' @rdname tfdisplay
#' @export
print.tfb <- function(x, n = 10, ...) {
  NextMethod()
  cat(" in basis representation:\n using ", attr(x, "basis_label"), "\n")
  if (length(x)) {
    cat(format(x[1:min(n, length(x))], ...), sep = "\n")
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
                      n = 10, prefix = TRUE, ...) {
  long <- length(x) > n
  if (long && width > 0 && width <= 30) {
    x <- head(x, n)
  }
  str <- string_rep_tf(x,
    signif_arg = abs(floor(log10(attr(x, "resolution")))),
    digits = digits, nsmall = nsmall, ...
  )
  if (prefix) {
    prefix <- if (!all(names(x) == "")) {
      names(x)[seq_along(str)]
    } else {
      paste0("[", seq_along(str), "]")
    }
    str <- map2(prefix, str, ~paste0(.x, ": ", .y))
  }
  unlist(map_if(
    str, ~nchar(.x) > width,
    ~paste0(substr(.x, 1, width - 3), "...")
  ))
}
