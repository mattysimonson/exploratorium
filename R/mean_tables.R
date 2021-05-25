#' Create a table estimating the percentage of "yes" replies to a yes/no survey question
#' and the margin of error
#'
#' @param dat A tbl_svy object created using the srvyr package
#' @param binary_outcome The name of an outcome variable in `dat`. No quotes needed.
#' @param na.rm A logical indicating whether to drop NAs from the outcome.
#' @param ci_level A numeric indicating the confidence level. Default it 0.95
#' @param vartype A string indicting how to display the variance.
#' See `srvyr::survey_mean()`. "ci" is preferable.
#'
#' @return tibble with up to three numeric columns showing point estimate,
#' and uncertainty (e.g., variance or upper and lower bounds of a
#' confidence interval)
#' @export
#'
#' @examples
#' data(ca_school_tbl_svy)
#'
#' ca_school_tbl_svy %>% simple_binary_tab(year.round.sch, vartype = "ci")
#'
#' rm(ca_school_tbl_svy)
simple_binary_tab <- function(dat, binary_outcome, na.rm = T,
                              ci_level = 0.95,
                              vartype = NULL){
  dat %>% summarise(mean = pct(survey_mean({{binary_outcome}},
                                           na.rm = na.rm,
                                           level = ci_level,
                                           vartype = vartype)))
}

#' Create crosstab of means and confidence intervals for numeric outcome(s)
#'
#' Takes in a `tbl_svy` built with the srvyr package, one or more grouping
# variables (no quotes needed), one or more outcomes
#'
#' @param s A `tbl_svy` object created with the srvyr package.
#' @param outcome_vars A character vector of outcome variable names. The elements are the
#' existing names of numeric or logical columns to take the means of. If it is
#' a named vector, the names are what they variables should be renamed as.
#' @param grouping_vars Name of factor or character column to group by
#' @param na.rm A logical indicating whether to drop NAs from output table
#' @param ci_level A numeric indicating the confidence level. Default it 0.95
#' @param displayN A logical indicating whether to append the number of
#' observations to each group's name. Ignored if there are zero or multiple
#' grouping variables.
#' @param prop.to.pct A logical indicating whether the outcome_vars are
#' proportions. If `TRUE`, convert estimates to percents and truncate
#' confidence intervals at 0 and 100
#' @param wrap A logical indicating to return html code that will print
#' the data to the Viewer window in Rstudio for copy and pasting into
#' other programs.
#'
#' @return A long formate tibble, or if wrap = T, a kableExtra table in html.
#'
#' @export
#'
#' @examples
#' data(ca_school_tbl_svy)
#'
#' # one outcome, no groups
#' surv_mean_table(ca_school_tbl_svy, "qual.teach.prop")
#'
#' # multiple outcomes, no groups
#'
#' outcome_vec <- c("eng.lang.learn.prop", "qual.teach.prop", "new.stu.prop")
#' surv_mean_table(ca_school_tbl_svy, outcome_vec)
#'
#' # if you plan to rename these outcome variables, create a named vector
#'   # and let the function rename them to avoid repetition
#'
#' named_outcome_vec <- c(`Fully qualified teachers` ="qual.teach.prop",
#'                       `New students`= "new.stu.prop",
#'                       `English language learners` = "eng.lang.learn.prop")
#'
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec)
#'
#' # prepare for copying or export using wrap (i.e., knitr::knit)
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec, wrap = T)
#'
#' # one grouping varible
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec, "poverty")
#'
#' # one grouping varible, N's not appended to names
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec, "poverty", displayN = F)
#'
#' # multiple grouping variables (note that N's can't be appended)
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec, c("poverty", "size"))
#'
#' # turn all columns into percentages and trim the CIs and 0 and 100
#' surv_mean_table(ca_school_tbl_svy, named_outcome_vec, c("poverty", "size"),
#'                  prop.to.pct =T)
#' rm(ca_school_tbl_svy)
surv_mean_table <- function(s,
                            outcome_vars,
                            grouping_vars = NULL,
                            na.rm = T,
                            ci_level = 0.95,
                            displayN = T,
                            prop.to.pct = F,
                            wrap = F){

  if(!is.null(names(outcome_vars))){
    s <- s %>%
      rename({{outcome_vars}})
    outcome_vars <- names(outcome_vars)
  }

  s <- s %>%
    group_by(across(all_of(grouping_vars)))

  out <- summarise(s, across(all_of(outcome_vars),
                     ~srvyr::survey_mean(., na.rm=T, level = ci_level, vartype = "ci"),
                     .names = "{.col}"),
                   n = srvyr::unweighted(n()))

  # rename in order to make it easy to select which columns to pivot
  out <- out %>%
    rename_with(~stringr::str_c(.,"__est"), all_of(outcome_vars)) %>%
    rename_with(~stringr::str_replace(.,"_low","__low"), ends_with("_low")) %>%
    rename_with(~stringr::str_replace(.,"_upp","__upp"), ends_with("_upp"))

  out <- out %>%
    tidyr::pivot_longer(cols = contains("__"), names_sep = "__",
                 names_to = c("outcome", "stat")) %>%
    tidyr::pivot_wider(names_from = "stat", values_from = "value")

  if(prop.to.pct){
    # turn everything into percentages and truncate CIs at 0 and 100
    out <- out %>%
      mutate(est = 100*est, low = 100*low, upp = 100*upp) %>%
      mutate(low = if_else(low<0, 0, low),
             upp = if_else(upp>100, 100, upp))
  }

  if(na.rm){
    out <- na.omit(out)
  }

  # if there are multiple grouping variables, do not attempt to
  # paste N into the group names
  if(displayN==T & length(grouping_vars)==1){
    # determined if group is ordered so we can restore this after
    .grp_var_1 <- sym(grouping_vars)
    ord <- out %>% select({{.grp_var_1}}) %>% pull() %>% is.ordered()

    out <- out %>%
      mutate(N=n) %>%
      mutate(leftside = " (N=", rightside = ")") %>%
      tidyr::unite({{.grp_var_1}}, {{.grp_var_1}}, leftside, N, rightside, sep = "") %>%
      mutate({{.grp_var_1}} := forcats::fct_inorder({{.grp_var_1}}, ordered = ord)) %>%
      select(-n)
  }

  if(wrap){
    out <- exploratorium::wrap(out)
  }
  out
}

#' Plot multiple outcomes and confidence intervals efficiently
#'
#' @param tab A data.frame with columns containing an estimate of the mean,
#' and upper bound for the CI, and a lower bound for the CI
#' (for point estimate, upper CI bound, and lower CI bound)
#' @param y_mean Name of column where estimate of mean is stored
#' @param y_max Name of column where upper bound of CI is stored
#' @param y_min Name of column where lower bound of CI is stored
#' @param y_names Name of column where names of outcomes are stored
#' @param color_var Name of grouping variable to differtiate by color (if any)
#' @param shape_var Name of grouping variable to differtiate by shape (if any)
#' @param dodge Numeric indicating the distance between estimates
#' @param outcome_order Character determining how outcomes should be ordered. Default preserves
#' the order in which values first appear in the data.frame
#' @param title A string supplying the title. Often good to standarized half of
#' it at the top of the section or document and then customize it with paste()
#' @param subtitle A string supplying the subtitle which is typically the question
#' stem of survey questions that are being presented as outcomes
#' @param dates A string supplying the dates of the survey to go in the caption.
#' Supplying dates trigger the following format for the captaion:
#' the words "National Sample, N = XXX, Time Period: `dates`" where the XXX is caluated
#' by the function
#' calculates
#' @param caption A string
#' @param x_axis_lab  A string w/ desired name of x-axis.
#' @param y_axis_lab A string w/ desired name of y-axis.
#' @param x_scale A string defaulting to "default" in which case the x-axis will
#' go from 0 to `x_max`.
#' @param color_scheme String supplying the color scheme.
#' @param x_max Numberic indicating highest deisred value for x-axis
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' data(ca_school_tbl_svy)
#'
#' named_outcome_vec <- c(`Fully qualified teachers` ="qual.teach.prop",
#'                        `New students`= "new.stu.prop",
#'                        `English language learners` = "eng.lang.learn.prop")
#'
#' tab <- surv_mean_table(ca_school_tbl_svy, named_outcome_vec,
#'                        c("poverty", "year.round.sch"),
#'                        prop.to.pct = T, wrap = F)
#'
#' plot_mean_table(tab, color_var = poverty, shape_var = year.round.sch,
#'                x_max = 110)
#'
#' rm(tab)
plot_mean_table <- function(tab,
                            y_mean = est,
                            y_max = upp,
                            y_min = low,
                            y_names = outcome,
                            color_var = NULL,
                            shape_var = NULL,
                            dodge = 0.5,
                            outcome_order = c("in_order", "reverse_order", "by_value", "user_supplied"),
                            title = NA,
                            subtitle = NA,
                            dates = NA,
                            caption = NA,
                            reverse_legend = T,
                            x_axis_lab = element_blank(),
                            y_axis_lab = element_blank(),
                            x_scale = "default",
                            color_scheme = "Dark2",
                            x_min = 0,
                            x_max = NA_real_){
  #message("Check2")
  if(!is.na(dates)){
    temp <- tab %>% group_by({{color_var}}, {{shape_var}}) %>%
      summarise(N = mean(n), .groups= "drop")
    N <- sum(temp$N)
    caption <- paste0("National Sample, N = ", scales::comma_format()(N), ", Time Period: ", dates)
  }
  #message("Check3")
  if(length(outcome_order)>1) outcome_order <- "in_order"

  if(outcome_order=="in_order"){
    tab <- tab %>% mutate({{y_names}} := fct_rev(fct_inorder({{y_names}})))
  }
  if(outcome_order=="reverse_order"){
    tab <- tab %>% mutate({{y_names}} := fct_inorder({{y_names}}))
  }
  if(outcome_order=="by_value"){
    tab <- tab %>% mutate({{y_names}} := reorder({{y_names}},{{y_mean}}))
  }

  #message("Check4")
  p <- tab %>% ggplot +
    geom_pointrange(aes(x = {{y_mean}},
                        y = {{y_names}},
                        xmin = {{y_min}},
                        xmax = {{y_max}},
                        color = {{color_var}},
                        shape = {{shape_var}}),
                    position = position_dodge(width = dodge)) +
    labs(x = x_axis_lab,
         y = y_axis_lab,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    guides(color = guide_legend(reverse = reverse_legend)) +
    theme_minimal()

  if(is.na(title)){
    ti.style <- element_blank()
  } else( ti.style <- ggtext::element_markdown(lineheight = 1.2))

  if(is.na(subtitle)){
    sub.style <- element_blank()
  } else( sub.style <- ggtext::element_markdown(lineheight = 1.2))

  if(is.na(caption)){
    cap.style <- element_blank()
  } else( cap.style <- ggtext::element_markdown(hjust = 0, lineheight = 1.2))

  p <- p +
    theme(
      plot.title = ti.style,
      plot.subtitle = sub.style,
      plot.caption = cap.style
      #, axis.text.y = element_markdown()
      #, axis.text.x = element_markdown()
    )

  if(!is.na(color_scheme)){
    p <- p + scale_color_brewer(palette= color_scheme)
  }

  if(x_scale == "default"){
    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(x_min, x_max))
  }
  p
}

# roxygen2::roxygenise()
# rm(list = ls())
