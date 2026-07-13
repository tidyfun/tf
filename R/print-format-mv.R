# Printing / formatting --------------------------------------------------------

#' @export
format.tf_mv <- function(x, ...) {
  comps <- tf_components(x)
  if (!length(comps)) return(character(0))
  per_comp <- map(comps, \(comp) format(comp, ...))
  do.call(paste, c(unname(per_comp), list(sep = " | ")))
}

# one-line description of a single component's representation, mirroring the
# "evaluations / interpolation / basis" lines of print.tfd / print.tfb.
mv_component_info <- function(comp) {
  if (is_tfb(comp)) {
    return(paste0(
      "in basis representation: ",
      trimws(paste(attr(comp, "basis_label"), attr(comp, "family_label")))
    ))
  }
  evaluator <- paste0("interpolation by ", attr(comp, "evaluator_name"))
  if (is_irreg(comp)) {
    n_evals <- tf_count(comp[!is.na(comp)])
    grid <- if (length(n_evals)) {
      paste0(
        "based on ",
        min(n_evals),
        " to ",
        max(n_evals),
        " evaluations each"
      )
    } else {
      "irregular"
    }
    return(paste0(grid, ", ", evaluator))
  }
  paste0("based on ", length(tf_arg(comp)), " evaluations each, ", evaluator)
}

#' @export
print.tf_mv <- function(x, n = 6, ...) {
  comp_names <- attr(x, "comp_names")
  d <- tf_ncomp(x)
  domain <- tf_domain(x) |> map_chr(format)
  if (d == 0L) {
    range_str <- "R^0"
  } else {
    range_str <- map_chr(tf_components(x), function(comp) {
      r <- suppressWarnings(safe_range_evals(comp)) |> map_chr(format)
      paste0("[", r[1], ", ", r[2], "]")
    }) |>
      paste(collapse = " x ")
  }
  cat(paste0(
    class(x)[1],
    "<d=",
    d,
    ">[",
    length(x),
    "] (",
    paste(comp_names, collapse = ", "),
    "): [",
    domain[1],
    ", ",
    domain[2],
    "] -> ",
    range_str,
    "\n"
  ))
  if (d > 0L) {
    info <- map_chr(tf_components(x), mv_component_info)
    if (length(unique(info)) == 1L) {
      # all components share the same grid / interpolator / basis
      cat(paste0("components ", info[1], "\n"))
    } else {
      for (k in seq_along(info)) {
        cat(paste0("  ", comp_names[k], ": ", info[k], "\n"))
      }
    }
  }
  len <- length(x)
  if (len > 0) {
    n_show <- min(n, len)
    formatted <- format(x[seq_len(n_show)], ...)
    paste0("[", seq_len(n_show), "]: ", formatted) |>
      cat(sep = "\n")
    cat("\n")
    if (n < len) {
      cat(paste0("    [....]   (", len - n, " not shown)\n"))
    }
  }
  invisible(x)
}

# dynamically exported in zzz.R (pillar glimpse), mirrors format_glimpse.tf
format_glimpse.tf_mv <- function(x, ...) {
  format.tf_mv(x, ...)
}
