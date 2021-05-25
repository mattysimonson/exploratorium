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
#' data(ca_school_tbl_svy)
#' ca_school_tbl_svy %>% basic_surv_prop_table("sch.wide.imp.goal", "comparative.imp.goal", "school.level")
basic_surv_prop_table <- function(s, ..., binary = F){
  fmla <- formula(append("~", paste(c(...), collapse = "+")))
  tab <- survey::svytable(fmla, s)
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

#' An intermediate helper function for surv_prop_table
#' @inheritParams basic_surv_prop_table
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
#' data(ca_school_tbl_svy)
#' # Multiple grouping variables
#' ca_school_tbl_svy %>%
#'   intermediate_surv_prop_table(outcome_vars = "school.level",
#'                  grouping_vars = c("sch.wide.imp.goal", "comparative.imp.goal"))
#'
#' # Multiple outcomes
#' ca_school_tbl_svy %>%
#'  intermediate_surv_prop_table(outcome_vars = c("sch.wide.imp.goal", "comparative.imp.goal"),
#'                 grouping_vars = "school.level",
#'                 new_outcome_name = "met_standards")
#'
#' # Binary outcome
#' ca_school_tbl_svy %>%
#'  intermediate_surv_prop_table(outcome_vars = "eligible.for.award",
#'                 grouping_vars = "school.level",
#'                 binary = T)
intermediate_surv_prop_table <- function(s, outcome_vars, grouping_vars,
                       new_outcome_name = "outcome",
                       binary = F){
  out.list <- purrr::map(outcome_vars, ~
                    basic_surv_prop_table(s, as.list(append(grouping_vars, .x)),
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
#' @inheritParams intermediate_surv_prop_table
#' @param filter_vars character or character vector with names of
#' filter varibles. These must be binary columns in s of type logical or numeric
#' (convert beforehand if needed). Values equal to 1 (true) are retained.
#' They filters will be applied seperately, not in succession,
#' in order to allow one to create multiple overlapping groups.
#' If more elaborate filtering is required, use filter( ) beforehand.
#' @param new_group_name A character with intended name of the column to
#' hold the names of the filter variables being treated as groups.
#' Ignored if less than 2 filter_vars.
#' @param wrap A logical indicating to return html code that will print
#' the data to the Viewer window in Rstudio for copy and pasting into
#' other programs.
#' @return A long formate tibble, or if wrap = T, a kableExtra table in html.
#' The intial columns are named for the grouping variables (including new groups
#' created from multiple outcomes and filters) and list all of a row's group
#' memberships. The later columns are named for the outcome categories and list the
#' percentages for each. Rows should sum to 100, unless outcome is binary
#' in which case only one column is displayed to avoid redudancy.
#' @export
#'
#' @examples
#' data(ca_school_tbl_svy)
#'
#' # No groups
#' ca_school_tbl_svy %>%
#'   surv_prop_table("poverty",
#'   wrap = T)
#'
# # One grouping
#' ca_school_tbl_svy %>%
#'   surv_prop_table("poverty", "size",
#'   wrap = T)
#'
#' # One grouping, binary outcome (only the "yes" column will be retained)
#' ca_school_tbl_svy %>%
#'   surv_prop_table("eligible.for.award", "school.level",
#'   wrap = T)
#'
#' # Multiple groupings
#' ca_school_tbl_svy %>%
#'   surv_prop_table(outcome_vars = "poverty",
#'                  grouping_vars = c("size", "school.level"),
#'                  wrap = T)
#'
#' # Multiple groupings and outcomes
#' # Note: these outcomes are yes/no variables, so only the "yes" column will be retained
#' ca_school_tbl_svy %>%
#'  surv_prop_table(outcome_vars = c("sch.wide.imp.goal", "comparative.imp.goal"),
#'                 grouping_vars = c("size", "school.level"),
#'                 new_outcome_name = "standards met",
#'                 wrap = T)
#'
#' # Overlapping groupting variables
#' ca_school_tbl_svy %>%
#'   mutate(sch.wide.imp.goal = sch.wide.imp.goal == "Yes",
#'          comparative.imp.goal = comparative.imp.goal == "Yes") %>%
#'  surv_prop_table(outcome_vars = "poverty",
#'            filter_vars = c("sch.wide.imp.goal","comparative.imp.goal"),
#'            new_group_name = "standards met",
#'            wrap = T)
#'
#' # Delay wrapping in order to modify the output first
#' ca_school_tbl_svy %>%
#'   surv_prop_table(outcome_vars = c("sch.wide.imp.goal", "comparative.imp.goal"),
#'                 grouping_vars = "school.level",
#'                 new_outcome_name = "standards met",
#'                 wrap = F) %>%
#'  mutate(`standards met` =
#'           recode(`standards met`,
#'                  sch.wide.imp.goal = "schoolwide target",
#'                  comparative.imp.goal = "comparative improvement target")) %>%
#'   rename(`school level` = school.level) %>%
#'   wrap()
#' rm(ca_school_tbl_svy)
surv_prop_table <- function(s,
                   outcome_vars,
                   grouping_vars = NULL,
                   filter_vars = NULL,
                   new_outcome_name = "outcome",
                   new_group_name = "group",
                   wrap = F){

  if(is.null(grouping_vars)){
    s <- s %>% mutate(temp_var := 1)
    grouping_vars <- "temp_var"
  }

  # outcome_type <- s %>%
  #   summarise(across(all_of(outcome_vars), class)) %>%
  #   pull()
  #
  # binary <-  "logical" %in% outcome_type

  if(is.null(filter_vars)){
    out <- intermediate_surv_prop_table(s=s,
                                  outcome_vars = outcome_vars,
                                  grouping_vars = grouping_vars,
                                  new_outcome_name = new_outcome_name,
                                  binary = F)
  } else {
    out.list <- purrr::map(filter_vars, ~
                             intermediate_surv_prop_table(
                               s = filter(s, .data[[.x]]==T),
                               outcome_vars = outcome_vars,
                               grouping_vars = grouping_vars,
                               new_outcome_name = new_outcome_name,
                               binary = F)
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

  if(out %>% select(where(is.numeric)) %>% ncol() == 2){
    col_to_drop <- out %>% select(last_col()) %>% colnames()

    if(col_to_drop=="TRUE") col_to_drop <- "FALSE"
    if(col_to_drop=="Yes") col_to_drop <- "No"
    if(col_to_drop=="yes") col_to_drop <- "no"
    if(col_to_drop=="1") col_to_drop <- "0"
    message(paste("Dropping column:", col_to_drop))
    out <- out %>%
      select(!all_of(col_to_drop)) %>%
      rename(percent = last_col())
  }

  if(wrap){
    out <- wrap(out)
  }
  out
}

# roxygen2::roxygenise();
# rm(list=ls())
