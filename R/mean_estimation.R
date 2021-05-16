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
#' load("data/ca_school_svy.rda")
#' ca_school_svy %>% simple_binary_tab(winner, vartype = "ci")
simple_binary_tab <- function(dat, binary_outcome, na.rm = T,
                              vartype = NULL){
  dat %>% summarise(mean = pct(survey_mean({{binary_outcome}},
                                           na.rm = na.rm,
                                           vartype = vartype)))
}


#' Create crosstab of means and confidence intervals for numeric outcome(s)
#'
#' Takes in a `tbl_svy` built with the srvyr package, one or more grouping
# variables (no quotes needed), one or more outcomes
#'
#' @param .data A `tbl_svy` object created with the srvyr package.
#' @param .named_outcome_vec A named character vector. The elements are the
#' existing names of numeric or logical columns to take the means of. The names
#' are what they should be renamed as.
#' @param .grp_var_1 Name of factor or character column to group by
#' @param ... Option additional factor or character columns to group by
#' @param .na.rm A logical indicating whether to drop NAs from output table
#' @param .displayN A logical indicating whether to append the number of
#' observations to each group's name. Ignored if there are zero or multiple
#' grouping variables.
#' @param .proportions A logical indicating whether the outcomes are
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
#' data(ca_school_svy)
#'
#' named_outcome_vec <- c(`percent tested` ="pcttest",
#'                        `free and reduced price lunch`= "meals",
#'                        `English language learners` = "ell")
#' # no grouping variables
#' survestimates(ca_school_svy, named_outcome_vec)
#'
#' # one grouping varible
#' survestimates(ca_school_svy, named_outcome_vec, comp.imp, stype, winner)
#'
#' # multiple grouping variables
#' survestimates(ca_school_svy, named_outcome_vec, comp.imp)
survestimates <- function(.s, .named_outcome_vec,
                           .grp_var_1 = NULL, ...,
                           .na.rm = T, .displayN = T, .proportions = F,
                           .wrap = F){
  new_outcome_names <- names(.named_outcome_vec)
  .s <- .s %>%
    group_by({{.grp_var_1}}) %>%
    group_by(..., .add = T) %>%
    rename({{.named_outcome_vec}})

  out <- summarise(.s, across(all_of(new_outcome_names),
                     ~survey_mean(., na.rm=T, vartype = "ci"),
                     .names = "{.col}"),
                   n = unweighted(n()))

  # rename in order to make it easy to select which columns to pivot
  out <- out %>%
    rename_with(~stringr::str_c(.,"__est"), all_of(new_outcome_names)) %>%
    rename_with(~stringr::str_replace(.,"_low","__low"), ends_with("_low")) %>%
    rename_with(~stringr::str_replace(.,"_upp","__upp"), ends_with("_upp"))

  out <- out %>%
    tidyr::pivot_longer(cols = contains("__"), names_sep = "__",
                 names_to = c("outcome", "stat")) %>%
    tidyr::pivot_wider(names_from = "stat", values_from = "value")

  if(.proportions){
    # turn everything into percentages and truncate CIs at 0 and 100
    out <- out %>%
      mutate(est = 100*est, low = 100*low, upp = 100*upp) %>%
      mutate(low = if_else(low<0, 0, low),
             upp = if_else(upp>100, 100, upp))
  }

  if(.na.rm){
    out <- na.omit(out)
  }

  # create a column for the raw unweighted number of obs in each group
 # out <- out %>% left_join(summarise(.s, n = unweighted(n())))
  # if there are multiple grouping variables, do not attempt to
  # paste N into the group names
  if(names(out)[2]!="n"){
    .displayN  <- F
  }

  # paste N into the group names
  if(.displayN==T){
    # determined if group is ordered so we can restore this after
    ord <- out %>% select({{.grp_var_1}}) %>% pull() %>% is.ordered()

    out <- out %>%
      mutate(N=n) %>%
      mutate(leftside = " (N=", rightside = ")") %>%
      tidyr::unite({{.grp_var_1}}, {{.grp_var_1}}, leftside, N, rightside, sep = "") %>%
      mutate({{.grp_var_1}} := forcats::fct_inorder({{.grp_var_1}}, ordered = ord)) %>%
      select(-n)
  }

  if(.wrap){
    out <- wrap(out)
  }
  out
}



#' Plot multiple outcomes and confidence intervals efficiently
#'
#' @param tab A table
#' @param y_var Name of outcome varible
#' @param color_var Name of grouping variable to differtiate by color (if any)
#' @param shape_var Name of grouping variable to differtiate by shape (if any)
#' @param dodge Numeric indicating the distance between estimates
#' @param order_by_est Logical determining whether to reorder outcomes accoring to
#' which was had the higher mean
#' @param title A string supplying the title. Often good to standarized half of
#' it at the top of the section or document and then customize it with paste()
#' @param subtitle A string supplying the subtitle which is typically the question
#' stem of survey questions that are being presented as outcomes
#' @param dates A string supplying the dates of the survey to go in the  caption.
#' Overridden by `caption`.
#' @param caption String. Useful if you want one caption for entire section or script.
#' @param x_axis_lab String w/ desired name of x-axis.
#' @param y_axis_lab String w/ desired name of y-axis.
#' @param x_scale String defaulting to "default" in which case the x-axis will
#' go from 0 to `x_max`.
#' @param color_scheme String supplying the color scheme.
#' @param x_max Numberic indicating highest deisred value for x-axis
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' #' data(ca_school_svy)
#'
#' named_outcome_vec <- c(`percent tested` ="pcttest",
#'                        `free and reduced price lunch`= "meals",
#'                        `English language learners` = "ell")
#'
#'
#' tab <- survestimates(ca_school_svy, named_outcome_vec, comp.imp, stype, wrap = F)
#'
#' cross_tab_plot(tab, color_var = comp.imp, shape_var = stype)
cross_tab_plot <- function(tab, y_var = outcome, color_var = NULL, shape_var = NULL,
                           dodge = 0.5,
                           order_by_est = T,
                           title = NA,
                           subtitle = NA,
                           dates = NA,
                           caption = "default",
                           x_axis_lab = element_blank(),
                           y_axis_lab = element_blank(),
                           x_scale = "default",
                           color_scheme = "Dark2",
                           x_max = NA_real_){
  #message("Check2")
  if(caption == "default"){
    temp <- tab %>% group_by({{color_var}}, {{shape_var}}) %>%
      summarise(N = mean(n), .groups= "drop")
    N <- sum(temp$N)
    caption <- paste0("National Sample, N = ", scales::comma_format()(N), ", Time Period: ", dates)
  }
  #message("Check3")
  if(order_by_est){
    tab <- tab %>% mutate(y := reorder({{y_var}}, est))
  } else tab <- tab %>% mutate(y := {{y_var}})
  #message("Check4")
  p <- tab %>% ggplot +
    geom_pointrange(aes(x = est, y = y,
                        xmin = low,
                        xmax = upp,
                        color = {{color_var}},
                        shape = {{shape_var}}),
                    position = position_dodge(width = -dodge)) +
    labs(x = x_axis_lab, y = y_axis_lab,
         title = title,
         subtitle = subtitle,
         caption = caption) +
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
    p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, x_max))
  }
  print(p) %>% suppressWarnings()
}



# roxygen2::roxygenise()

