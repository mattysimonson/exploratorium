library(survey)
library(srvyr)
data(api)
ca_school_data <- apisrs %>% mutate(poverty = factor(case_when(meals <= 25 ~ "low",
                                      meals <= 50 ~ "mid-low",
                                      meals <= 75 ~ "mid-high",
                                      T ~ "high")),
                  winner = awards == "Yes") %>%
  select(cds, stype, fpc, pcttest, cname, meals, poverty, winner, ell, comp.imp, sch.wide)
ca_school_svy <- ca_school_data %>% as_survey_design(ids = 1, fpc = fpc)
usethis::use_data(ca_school_data, ca_school_svy, overwrite = TRUE)
