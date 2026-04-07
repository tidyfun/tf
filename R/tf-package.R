#' @description `tf` is a light-weight package with few dependencies that
#' provides the class definitions and methods infrastructure for `tidyfun` --
#' `tf` gives you:
#'
#'   - new S3 data types for representing (vectors of) functional data:
#'     [tfd()] & [tfb()]
#'   - arithmetic operators for such data
#'   - simple descriptive statistics: e.g. [mean.tf()], [median.tf()]
#'   - base graphics functions for such data: [plot.tf()]
#'   - functions to do smoothing ([tf_smooth.tfd()]),
#'     differentiation ([tf_derive.tfd()]) and integration ([tf_integrate.tfd()])
#'
#'
#' The goal of the add-on package `tidyfun` is to make data wrangling and
#' exploratory analysis for functional data in `R` quick and easy, using
#' `tidyverse` syntax and standards. \cr\cr
#' Please also install `tidyfun` for the full functionality to
#' access the full documentation including a number of vignettes and case
#' studies, or visit the [`tidyfun`
#' website](https://tidyfun.github.io/tidyfun/).
#'
#' @keywords internal
#' @importFrom checkmate assert_choice assert_class assert_count
#' @importFrom checkmate assert_data_frame assert_flag assert_function
#' @importFrom checkmate assert_int assert_list assert_logical assert_matrix
#' @importFrom checkmate assert_number assert_numeric assert_string
#' @importFrom checkmate assert_set_equal assert_subset assert_true
#' @importFrom checkmate allMissing check_numeric makeAssertionFunction vname
#' @import rlang
#' @import vctrs
#' @rawNamespace import(purrr, except = c(flatten, flatten_lgl, flatten_int, flatten_dbl, flatten_chr, flatten_raw, splice, invoke, `%@%`))
#' @importFrom mgcv s Predict.matrix magic uniquecombs gam bam smooth.construct PredictMat scat
#' @importFrom methods formalArgs
#' @importFrom stats setNames na.omit complete.cases lowess
#' @importFrom utils head tail modifyList
#' @importFrom zoo zoo coredata rollmean rollmedian na.fill na.spline na.approx na.locf
"_PACKAGE"
