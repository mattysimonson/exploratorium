#' Create an neatly html formatted table
#'
#' Takes a rectangular data structure and outs and table that prints
#' to the Viewer screen in Rstudio for each coping to other programs
#' like Excel, GoogleSheets, or Datawrapper
#'
#' @param t A data.frame or similar rectangular object to format.
#'
#' @return html code for a sleek and simple table
#' @export
#'
#' @examples
#' wrap(cars)
wrap <- function(t) {
  out <- knitr::kable(t) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F,  position = "left")
  return(out)
}

