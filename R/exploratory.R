# This script hold functions that are useful for making crosstab tables and plots
# By Matthew Simonson
# Last Updated May 15, 2021

# Status: Go!

# Note: Source this.

# Functions from Elsewhere ------------------------------------------------

#' Convert proportions to percents
#'
#' This function converts decimals to precents and rounds them
#'
#' @param x numeric vector or data.frame
#' @param decimals integer indicating how many decimal places to round to
#'
#' @return object of same class as x
#' @export
#'
#' @examples
#' vec <- c(0.4, 0.0265, 0.3234)
#' pct(vec, decimals = 1)
pct <- function(x, decimals = 1) {
  round(x*100, decimals)
  }

# roxygen2::roxygenise()
