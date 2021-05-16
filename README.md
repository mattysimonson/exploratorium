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

The data here comes from a survey of California school districts in the early 2000s.
What proportion of schools of each size and level had high poverty? Low poverty?

We begin with a `tbl_svy` object created using the `srvyr` package. This basically a `svydesign` object from Thomas Lumley `survey` package, enhanced to allow `tidyverse` operations.

``` r
library(exploratorium)
data(ca_school_tbl_svy)
ca_school_tbl_svy %>%
   surv_prop_table(outcome_vars = "poverty",
                  grouping_vars = c("size", "school.level"))
```
You can easily copy and paste the output from your viewer screen into Excel, GoogleSheets, or Datawrapper, or save and upload it. It's ready to go! Note that we could have supplied any number of outcome or grouping variables. 

Now suppose that we would like to compare poverty levels among schools that either met their schoolwide improvement goal or their comparative improvement goal the previous year. We could list "poverty" as the `outcome_var` and the two standards as `grouping_vars`:

```
ca_school_tbl_svy %>%
 surv_prop_table(outcome_vars = "poverty",
                grouping_vars = c("sch.wide.imp.goal", "comparative.imp.goal"))
```
This table yieds for rows for each combination of the two goals, which may be helpful in some contexts. However, if we'd like to compare the poverty rates for school that met the schoolwide goal (regardless of whether they met the second goal) to schools that met the second goal (regardless of whether they met the first goal), then we treat both of them as `filter_vars`. The function will filter by one goal first, then the other, retaining observations for which the answer was `TRUE`. Therefore, we must first convert them from `yes/no` to `TRUE/FALSE`.

```
ca_school_tbl_svy %>%
  mutate(sch.wide.imp.goal = sch.wide.imp.goal == "Yes",
         comparative.imp.goal = comparative.imp.goal == "Yes") %>%
 surv_prop_table(outcome_vars = "poverty",
                 filter_vars = c("sch.wide.imp.goal","comparative.imp.goal"))
```
That's all there is to it! 

### Numeric outcomes: Make a table showing the mean of every outcome in each group

So far we've only been working with outcomes that are categorical or binary. To work with outcomes that are numeric (e.g., to compare the means rather than the proportions across groups), confidence intervals, use the `surv_mean_table` function. Suppose we'd like to compare the mean
proportions of English-language learners and new students among schools of various sizes and types.

```
surv_mean_table(ca_school_tbl_svy, 
                outcome_vars = c("eng.lang.learn.prop", "new.stu.prop"), 
                grouping_vars = c("school.level", "size"),
                wrap = T)

```

It might be nice to rename those outcomes. We can avoid having to type
the old or new names more than once if we create a named vector. This can be particularly
useful if we want run multiple tables with the same outcomes but different groups.

```
named_outcome_vec <- c(`New students`= "new.stu.prop",
                        `English language learners` = "eng.lang.learn.prop")
                        
surv_mean_table(ca_school_tbl_svy, 
                outcome_vars = named_outcome_vec, 
                grouping_vars = c("school.level", "size"),
                wrap = T)
```
Rather than moving it over to Excel or some other program for plotting, we let `wrap=F` (the default) and can plot the results using the `plot_mean_table` function.
results if R if we wish the 

```
surv_mean_table(ca_school_tbl_svy, 
                outcome_vars = named_outcome_vec, 
                grouping_vars = c("school.level", "size"),
                wrap = F) %>% 
  plot_mean_table(color_var = school.level, shape_var = size)
                                  
```

The three main functions in the package `surv_prop_table`, `surv_mean_table`, and `plot_mean_table` all have more examples in their documentation. There are also helper functions like `wrap` and `pct` which may be handy. More updates to come. Good luck! 
