#' @rdname tf_evaluate
#' @import tidyr
#' @importFrom tidyselect vars_select quos
#' @importFrom rlang quo_text
#' @export
#' @examples 
#' library(dplyr)
#' f <- tf_rgp(3, arg = 7)
#' tf_evaluate(f) %>% str()
#' tf_evaluate(f, arg = .5) %>% str()
#' tf_evaluate(f, arg = .5) %>% str()
#' tf_evaluate(d) %>% glimpse()
#' tf_evaluate(d, -b) %>% glimpse()
#' tf_evaluate(d, a) %>% glimpse() 
#' grid <- seq(0, 1, l = 11)
#' # <arg> must be specified as named argument!
#' tf_evaluate(d, arg = grid) %>% glimpse() 
tf_evaluate.data.frame <- function(object, ..., arg) {
  # figure out which tf columns to evaluate:
  tf_cols <- names(object)[map_lgl(object, is_tf)]
  tf_to_evaluate <- enquos(...)
  if (!is_empty(tf_to_evaluate)) {
    tf_to_evaluate <- unname(vars_select(names(object), !!!tf_to_evaluate))
    tf_cols <- intersect(tf_cols, tf_to_evaluate)
  }
  if (!length(tf_cols)) {
    warning("Nothing to be done for tf_evaluate.")
    return(object)
  }
  if (!missing(arg) && !is.null(arg)) {
    arg <- ensure_list(arg)
    if (length(arg) == 1 & length(tf_cols) > 1) {
      arg <- replicate(length(tf_cols), arg, simplify = FALSE)
    }
  } else {
    arg <- map(object[tf_cols], ~ensure_list(tf_arg(.)))
  }
  stopifnot(length(arg) == length(tf_cols))
  names(arg) <- tf_cols
  # convert them to list-columns of data.frames
  for (f in tf_cols) {
    object[[f]] <- object[[f]][, arg[[f]], matrix = FALSE]
  }
  object
}
#--------------------------------------------------------------------------------

context("tf_evaluate for data frames")

d <- tibble(a = tf_rgp(3), b = tf_rgp(3)) 

test_that("tf_evaluate.data.frame basically works", {
  da <- d$a
  expect_identical(tf_evaluate(da)[["a"]],
                   da[["a"]][ , tf_arg(d$a), matrix = FALSE])
})

test_that("tf_evaluate.data.frame interface works", {
  expect_identical(tf_evaluate(d), 
                   tf_evaluate(d, a, b))
  expect_identical(tf_evaluate(d, a), 
                   tf_evaluate(d, -b))
  expect_identical(tf_evaluate(d, a, arg = seq(0, 1, l = 11))[["a"]], 
                   d[["a"]][ , seq(0, 1, l = 11), matrix = FALSE])
  expect_identical(tf_evaluate(d, b, arg = seq(0, 1, l=11)), 
                   tf_evaluate(d, arg = seq(0, 1, l = 11), b))
})
