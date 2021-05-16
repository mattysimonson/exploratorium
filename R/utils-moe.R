
#' Margin of error
#'
#' Calcuates the margin of error.
#'
#' Under the usual frequentist assumptions, the underlying percentage
#'   of the quantity of interested in the population ought to be within
#'   ± the returned value of the original estimate
#'
#' @param est The point estimate as a percent.
#' @param n The sample size.
#' @param confidence The confidence level as a percent
#'
#' @return The margin of error in percentage points.
#' @export
#'
#' @examples
#' moe(50, 750, 95)
moe <- function(est, n = 400, confidence = 95){
  # calculated the z-score using Student's t distribution
  # to correct for small samples
  z_score <- qt(p = 1 - (1 - confidence/100)/2,
                df = n -1)
  est <- est/100
  ci_radius <- z_score*sqrt(est*(1-est)/n)
  ci_radius*100
}
