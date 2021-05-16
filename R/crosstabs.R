#' A workhourse function to construct survey crosstabs
#'
#' @param s A tbl_svy object created using the srvyr package.
#' @param ... Names columns to be used in as grouping variables followed by
#' the column to be used as the outcome variable
#' @param binary A logical indicating whether the outcome variable is
#' binary and hence only one column should be displayed
#'
#' @return A long format tibble. Each of the initial character columns is named
#' for a grouping variable and list combination of grouping categories for that row
#' The later numeric columns are named for the outcome categories and list the
#' percentages falling into each. Rows should sum to 100, unless outcome is binary
#' in which case only the column for "TRUE" is displayed to avoid redudancy.
#' @export
#'
#' @examples
#' load("data/ca_school_svy.rda")
#' ca_school_svy %>% basic_survtable("sch.wide", "comp.imp", "stype")
basic_survtable <- function(s, ..., binary = F){
  fmla <- formula(append("~", paste(c(...), collapse = "+")))
  tab <- svytable(fmla, s)
  nvars <- length(dim(tab))
  out <- tab %>%
    prop.table(margin = 1:(nvars-1)) %>%
    pct() %>%
    as_tibble() %>%
    tidyr::pivot_wider(names_from = all_of(nvars), values_from=all_of(nvars+1))
  if(binary == T){
    out <- out %>%
      select(-`FALSE`) %>%
      rename(percent = `TRUE`)
  }
  out
}

#' An intermediate helper function for survtable
#' @inheritParams basic_survtable
#' @param outcome_vars A character vector naming the column(s) in `s` to be used
#' as outcomes variable(s). These must be binary or categorial columns in s (convert
#' beforehand if needed).
#' @param grouping_vars A character vector naming the column(s) in `s` to be used
#' as grouping variable(s). These must be binary or categorial columns in s
#' (convert beforehand if needed).
#' @param new_outcome_name Character with intended name of the column
#' to hold the names of the outcome variables. Ignored if only 1 outcome.
#' @param binary Logical indicating whether the outcome(s) are binary
#' in which case only one column of outcome values will be displayed.
#'
#' @return A long format tibble. Each of the initial character columns is named
#' for a grouping variable and list combination of grouping categories for that row
#' The later numeric columns are named for the outcome categories and list the
#' percentages falling into each. Rows should sum to 100, unless outcome is binary
#' in which case only the column for "TRUE" is displayed to avoid redudancy.
#' @export
#'
#' @examples
#' load("data/ca_school_svy.rda")
#' # Multiple grouping variables
#' ca_school_svy %>%
#'   intermediate_survtable(outcome_vars = "stype",
#'                  grouping_vars = c("sch.wide", "comp.imp"))
#'
#' # Multiple outcomes
#' ca_school_svy %>%
#'  intermediate_survtable(outcome_vars = c("sch.wide", "comp.imp"),
#'                 grouping_vars = "stype",
#'                 new_outcome_name = "met_standards")
#'
#' # Binary outcome
#' ca_school_svy %>%
#'  intermediate_survtable(outcome_vars = "winner",
#'                 grouping_vars = "stype",
#'                 binary = T)
intermediate_survtable <- function(s, outcome_vars, grouping_vars,
                       new_outcome_name = "outcome",
                       binary = F){
  out.list <- purrr::map(outcome_vars, ~
                    basic_survtable(s, as.list(append(grouping_vars, .x)),
                                binary = binary)
  )
  if(length(out.list)>1){
    names(out.list) <- outcome_vars
    out <- bind_rows(out.list, .id = new_outcome_name)
  } else{
    out <- out.list[[1]]
  }
  out
}



#' Create easy-to-export survey crosstabs
#'
#' This function creates crosstabs for any number of grouping, filter,
#' and outcome variables.
#'
#' This function outputs a wide format table with a column for each
#' grouping variable listing its categories and a column of values for each
#' response category of the outcome(s). Rows will sum to 100%
#' If outcomes are binary only one column of values is shown
#' If there are multiple outcomes, an outcome column is added to indicate
#' which outcome is being used for each row. If multiple filter variables,
#' a filter column is added to indicated which filter is used on each row
#' (this allows you to create groups that overlap)
#'
#' @inheritParams intermediate_survtable
#' @param filter_vars character or character vector with names of
#' filter varibles. These must be binary columns in s of type logical
#' (convert beforehand if needed). They will be applied seperately, not
#' in succession, in order to allow on create more overlapping groups.
#' Each variable is set to equal T. If more elaborate filtering is
#' required, use filter( ) beforehand.
#' @param new_group_name A character with intended name of the column to
#' hold the names of the filter variables being treated as groups.
#' Ignored if less than 2 filter_vars.
#' @param wrap A logical indicating to return html code that will print
#' the data to the Viewer window in Rstudio for copy and pasting into
#' other programs.
#'
#' @return A long formate tibble, or if wrap = T, a kableExtra table in html.
#' Each of the initial character columns is named for a grouping variable and
#' list combination of grouping categories for that row.
#' The later numeric columns are named for the outcome categories and list the
#' percentages falling into each. Rows should sum to 100, unless outcome is binary
#' in which case only the column for "TRUE" is displayed to avoid redudancy.
#' @export
#'
#' @examples
#' load("data/ca_school_svy.rda")
#'
#' # Simple, no groups
#' ca_school_svy %>%
#'   survtable(outcome_vars = "stype")
#'
#' # Multiple grouping variables
#' ca_school_svy %>%
#'   survtable(outcome_vars = "stype",
#'                  grouping_vars = c("sch.wide", "comp.imp"))
#'
#' # Multiple outcomes
#' ca_school_svy %>%
#'  survtable(outcome_vars = c("sch.wide", "comp.imp"),
#'                 grouping_vars = "stype",
#'                 new_outcome_name = "met_standards")
#'
#' # Binary outcome
#' ca_school_svy %>%
#'  survtable(outcome_vars = "winner",
#'                 grouping_vars = "stype")
#'
#' # Overlapping groupting variables
#' ca_school_svy %>%
#'   mutate(school_wide = sch.wide == "Yes",
#'          comparable_improvement = comp.imp == "Yes") %>%
#'  survtable(outcome_vars = "poverty",
#'            filter_vars = c("school_wide","comparable_improvement"),
#'            new_group_name = "met_standards")
#'
#' # Delay wrapping in order to modify the output first
#' ca_school_svy %>%
#' survtable(outcome_vars = c("sch.wide", "comp.imp"),
#'                 grouping_vars = "stype",
#'                 new_outcome_name = "met_standards",
#'                 wrap = F) %>%
#' mutate(met_standards = recode(met_standards, sch.wide = "school wide",
#'                               comp.imp = "comparable improvement")) %>%
#' wrap()
#' rm(ca_school_svy)
survtable <- function(s,
                   outcome_vars,
                   grouping_vars = NULL,
                   filter_vars = NULL,
                   new_outcome_name = "outcome",
                   new_group_name = "group",
                   wrap = T){

  if(is.null(grouping_vars)){
    s <- s %>% mutate(temp_var := 1)
    grouping_vars <- "temp_var"
  }

  outcome_type <- s %>%
    summarise(across(all_of(outcome_vars), class)) %>%
    pull()

  binary <-  "logical" %in% outcome_type

  if(is.null(filter_vars)){
    out <- intermediate_survtable(s=s,
                                  outcome_vars = outcome_vars,
                                  grouping_vars = grouping_vars,
                                  new_outcome_name = new_outcome_name,
                                  binary = binary)
  } else {
    out.list <- purrr::map(filter_vars, ~
                             intermediate_survtable(
                               s = filter(s, .data[[.x]]==T),
                               outcome_vars = outcome_vars,
                               grouping_vars = grouping_vars,
                               new_outcome_name = new_outcome_name,
                               binary = binary)
    )

    if(length(filter_vars)>1){
      names(out.list) <- filter_vars
      out <- bind_rows(out.list, .id = new_group_name)
    } else {
      out <- out.list[[1]]
    }
  }

  if(grouping_vars[1] == "temp_var"){
    out <- out %>% select(-temp_var)
  }

  if(wrap){
    out <- wrap(out)
  }
  out

}

# roxygen2::roxygenise()
