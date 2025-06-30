#' Pinch force data
#'
#' Measurements of pinch force during 20 replications, with 151 observations
#' recorded every 2 milliseconds over 300 milliseconds. The data captures the
#' dynamics of finger pinch force applied during controlled motor tasks.
#'
#' Data is also include in the \CRANpkg{fda} package in another format.
#'
#' @references
#' `r format_bib("ramsay2009functional", "ramsay2005functional", "ramsay2002applied")`
#'
#' @examples
#' pinch
"pinch"

#' Berkeley growth study data
#'
#' Heights of 39 boys and 54 girls measured from age 1 to 18 years as part
#' of the Berkeley Growth Study. The data tracks physical development over
#' time with measurements at 31 different ages that are not equally spaced.'
#'
#' Data is also include in the \CRANpkg{fda} package in another format.
#'
#' @references
#' `r format_bib("ramsay2009functional", "ramsay2005functional", "ramsay2002applied", "tuddenham1954physical")`
#'
#' @format A data frame with 93 rows and 2 variables:
#' \describe{
#'   \item{gender}{sex of the subject (boy/girl)}
#'   \item{height}{height in centimeters}
#' }
#' @examples
#' head(growth)
"growth"

#' Hip and knee angle while walking data
#'
#' Hip and knee angle measurements in degrees through a 20-point movement cycle
#' for 39 boys. The data represents the angular positions of hip and knee
#' joints during normal walking gait, captured at evenly spaced time points
#' throughout the gait cycle.
#'
#' Data is also include in the `datasets` package in another format.
#'
#' @format A data frame with 39 rows and 3 variables:
#' \describe{
#'   \item{subject_id}{subject identifier}
#'   \item{knee_angle}{knee joint angles in degrees}
#'   \item{hip_angle}{hip joint angle in degrees}
#' }
#' @examples
#' head(gait)
"gait"
