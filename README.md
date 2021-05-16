# exploratorium

<!-- badges: start -->
<!-- badges: end -->

The goal of exploratorium is to make it easy to conduct exploratory data analysis of complex survey data and produce figures and tables from it. It's named after a delightful science museum in San Francisco.

## Installation

I hope to have the package available on CRAN by the end of the 2021. For now, please install it from GitHub. To do so, will will need to have the `devtools` package installed.

``` r
devtools::install_github("exploratorium")
```
I will be continuing to make updates until the first CRAN release, so check the change log here. Let me know if any problems or requests by raising an Issue on Github.

## Examples

### Categorial outcomes: Make a table showing the proportion of every outcome category in each group

In a survey of California school districts in the early 2000s, researchers recorded whether each school's test scores had met the state's standards for improvement.  There were (apparently) two such standards: "school-wide improvement" and "comparative improvement."  Suppose you want to compare what proportion of elementary, middle, and high schools met each type of standard.  School type (elementary/middle/high) is recorded in the `stype` column, while the standards are recorded in the `sch.wide` and `comp.imp` columns respectively. Both standards have the same set of response options ("Yes" and "No"), making them easy to compare. Therefore, we'll stack the results and generate a new column named `met_standards` to indicate what kind of standard was or was not being met.

``` r
library(exploratorium)
data(ca_school_svy)
ca_school_svy %>%
 surv_prop_table(outcome_vars = c("sch.wide", "comp.imp"),
                grouping_vars = "stype",
                new_outcome_name = "met_standards")
```
You can easily copy and paste the output from your viewer screen into Excel, GoogleSheets, or Datawrapper, or save and upload it. It's ready to go! Note that we could have supplied any number of outcome or grouping variables. Now suppose that we would like to compare poverty levels (based on % of students on free and reduced-price lunhc) among school thats the met each type of standard. I could list "poverty" as the `outcome_var` and the two standards as my `grouping_vars`:

```
ca_school_svy %>%
 surv_prop_table(outcome_vars = "poverty",
                grouping_vars = c("sch.wide", "comp.imp"))
```
However, I'd like compare the overall poverty rate among the set of all schools with met the School-wide Improvement standard to the set of all school that met the Comparative Improvement standard. Since these group overlap, I'll treat them as `filter_vars`, and then create a new column call `met standards` to indicate which standard was met. Because `filter_vars` retain only values for which as given variable is `TRUE`, I need to tranform these from two variables from "Yes/No" (`factor`) variables into ("True/False") (`logical`) variables.

```
ca_school_svy %>%
  mutate(school_wide = sch.wide == "Yes",
         comparable_improvement = comp.imp == "Yes") %>%
 surv_prop_table(outcome_vars = "poverty",
           filter_vars = c("school_wide","comparable_improvement"),
           new_group_name = "met_standards")
```
That's all there is to it! 

### Numeric outcomes: Make a table showing the mean every outcome in each group

So far we've only been working with outcomes that are categorical or binary. To work with outcomes that are numeric (e.g., to compare the means rather than the proportions across groups) and/or to obtain confidence intervals, use the `surv_mean_table` function. Suppose we'd like to compare several statistics across school types: the average % of students tested, the average % of students on free and reduced price lunch, and the average % of English langauge learners. Since these outcomes are all on the same scale (0-100), they should be easy to compare in a table or graph. First, we need to creat a vector renaming each outcome so it reads the way we'll want it to look in the output.

```
outcomes <- c(`percent tested` ="pcttest",
                       `free and reduced price lunch`= "meals",
                       `English language learners` = "ell")
```

Then run

```
ca_school_svy %>% surv_mean_table(.named_outcome_vec = outcomes, 
                                  .grp_var_1 = stype, 
                                  .proportions = T
                                  .wrap = F)
```

Notice that `stype` does not require quotes. In fact, I can enter addition grouping variables without quotes or parenetheses.

```
ca_school_svy %>% surv_mean_table(outcomes, stype, sch.wide, comp.imp,
                                  .proportions = T
                                  .wrap = F)
```
See `?surv_mean_table` for further documentation.
