#' @description `tf` is a light-weight package with few dependencies that
#' provides the class definitions and methods infrastructure for `tidyfun` --
#' `tf` gives you:
#'
#'   - new S3 data types for representing (vectors of) functional data:
#'     [tfd()] & [tfb()]
#'   - arithmetic operators for such data ([Ops.tf()]),
#'   - simple descriptive statistics: e.g. [mean.tf()], [median.tf()]
#'   - base graphics functions for such data: [plot.tf()]
#'   - functions to do smoothing ([tf_smooth.tfd()]),
#'     differentiation [tf_derive.tfd()]) and integration ([tf_derive.tfd()])
#'
#'
#' The goal of the add-on package `tidyfun` is to make data wrangling and
#' exploratory analysis for functional data in \code{R} quick and easy, using
#' `tidyverse` syntax and standards. \cr\cr
#' Please also install `tidyfun` for the full functionality to
#' access the full documentation including a number of vignettes and case
#' studies, or visit the [`tidyfun`
#' website](https://tidyfun.github.io/tidyfun/).
#'
#' @keywords internal
"_PACKAGE"
