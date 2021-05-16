#' Create a table estimating the percentage of "yes" replies to a yes/no survey question
#' and the margin of error
#'
#' @param dat A tbl_svy object created using the srvyr package
#' @param binary_outcome The name of an outcome variable in `dat`. No quotes needed.
#' @param na.rm A logical indicating whether to drop NAs from the outcome.
#' @param vartype A string indicting how to display the variance.
#' See `srvyr::survey_mean()`. "ci" is preferable.
#'
#' @return tibble with up to three numeric columns showing point estimate,
#' and uncertainty (e.g., variance or upper and lower bounds of the 95%
#' confidence interval)
#' @export
#'
#' @examples
#' data(api)
#'
#' my_tbl_svy <- apisrs %>% as_survey_design(ids = 1, fpc = fpc)
#'
#' my_tbl_svy %>% mutate(elementary = stype=="E") %>%
#'     simple_binary_tab(elementary, vartype = "ci")
#'
#' rm(apiclus1, apiclus2, apipop, apisrs, apistrat, my_tbl_svy)
simple_binary_tab <- function(dat, binary_outcome, na.rm = T,
                              vartype = NULL){
  dat %>% summarise(mean = pct(survey_mean({{binary_outcome}},
                                           na.rm = na.rm,
                                           vartype = vartype)))
}

# roxygen2::roxygenise()
