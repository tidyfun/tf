# Conversion / interop ---------------------------------------------------------

#' @export
as.matrix.tf_mv <- function(x, arg, interpolate = FALSE, ...) {
  if (missing(arg)) {
    x[,, interpolate = interpolate, matrix = TRUE]
  } else {
    x[, arg, interpolate = interpolate, matrix = TRUE]
  }
}

#' @export
as.data.frame.tf_mv <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  unnest = FALSE,
  ...
) {
  if (!unnest) {
    out <- vctrs::new_data_frame(list(x), n = vec_size(x))
    names(out) <- "data"
    return(out)
  }
  comps <- tf_components(x)
  comp_names <- attr(x, "comp_names")
  # one long (id, arg, <comp_name>) per component, then full-outer-join on
  # (id, arg). For components that share arg structure this gives the same
  # rows as a side-by-side cbind would; for mixed regular/irregular or
  # otherwise-misaligned components NAs are filled where a component has no
  # observation at that (id, arg).
  per_comp <- map2(comps, comp_names, function(comp, nm) {
    df <- as.data.frame(comp, unnest = TRUE)
    names(df)[names(df) == "value"] <- nm
    df
  })
  out <- per_comp[[1]]
  for (k in seq_along(per_comp)[-1]) {
    out <- merge(
      out,
      per_comp[[k]],
      by = c("id", "arg"),
      all = TRUE,
      sort = FALSE
    )
  }
  out[order(out$id, out$arg), , drop = FALSE]
}
