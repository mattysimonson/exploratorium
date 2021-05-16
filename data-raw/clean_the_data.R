library(dplyr)
data(api, package = "survey")

ca_school_data <- apistrat %>%
  mutate(
    # create two ordered factors
    poverty = ordered(case_when(meals <= 25 ~ "low",
                                meals <= 50 ~ "mid-low",
                                meals <= 75 ~ "mid-high",
                                T ~ "high"),
                      levels = c( "low", "mid-low", "mid-high", "high")),
    size = ordered(case_when(enroll > 1000 ~ "large",
                             enroll > 500 ~ "medium",
                             T ~ "small"),
                   levels = c( "small", "medium", "large")),
    school.level = ordered(case_when(stype == "E" ~ "Elementary",
                                     stype == "M" ~ "Middle",
                                     stype == "H" ~ "High"),
                                     levels = c("Elementary", "Middle", "High")),
    # create two logicals
    eligible.for.award = awards == "Yes",
    year.round.sch = yr.rnd == "Yes",

    # create three numeric (proportion) variables
    new.stu.prop = mobility/100,
    eng.lang.learn.prop = ell/100,
    qual.teach.prop = full/100,

    # rescale weights
    simple.weight = pw/sum(apistrat$pw)
    ) %>%
  rename(
    # rename two binary variables (non-ordered factors)
    sch.wide.imp.goal = sch.wide,
    comparative.imp.goal = comp.imp,
    # rename two numeric variables
    test.score.1999 = api00,
    test.score.2000 = api99,
    # rename admin variables
    id = cds,
  ) %>%
  select(id, simple.weight, # admin
         eligible.for.award, year.round.sch, # logical
         poverty, size, school.level, # ordered factor
         sch.wide.imp.goal, comparative.imp.goal,# factor
         test.score.1999, test.score.2000, # numeric
         new.stu.prop, eng.lang.learn.prop, qual.teach.prop # numeric (proportion)
         ) %>%
  relocate(id, simple.weight, # admin
           eligible.for.award, year.round.sch, # logical
         poverty, size, school.level, # ordered factor
         sch.wide.imp.goal, comparative.imp.goal, # factor
         test.score.1999, test.score.2000, # numeric
         new.stu.prop, eng.lang.learn.prop, qual.teach.prop # numeric (proportion)
  )
ca_school_svy <- survey::svydesign(id=~1, weights=~simple.weight, data=ca_school_data)
ca_school_tbl_svy <- ca_school_data %>%
  srvyr::as_survey_design(ids = id, weights = simple.weight)

usethis::use_data(ca_school_data, ca_school_svy, ca_school_tbl_svy, overwrite = TRUE)
rm(list=ls())
