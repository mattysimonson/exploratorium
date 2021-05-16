#' Student performance in California schools.
#'
#' Derived from `apistrat` data frame in the `api` data in the survey package.
#'
#' @format A data frame with 200 observations of 11 variables:
#' \describe{
#' \item{cds}{Unique identifier}
#' \item{fpc}{finite population correct}
#' \item{stype}{Elementary/Middle/High School}
#' \item{cname}{County name}
#' \item{pcttest}{percentage of students tested}
#' \item{ell}{Percent English language learners}
#' \item{meals}{Percent of students on free and reduced price lunch}
#' \item{poverty}{Federally-defined poverty level based meals variable}
#' \item{winner}{Eligible for awards program}
#' \item{sch.wide}{Met school-wide growth target}
#' \item{comp.imp}{Met Comparable Improvement target}
#' }
#' @source \url{http://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm}
"ca_school_data"

#' Student performance in California schools (srvyr object).
#'
#' Built from the `ca_school_data` data frame, which in turn derives from `apistrat`
#' in the `api` data in the survey package.
#'
#' @format A simple probability survey with 200 observations of 11 variables:
#' \describe{
#' \item{cds}{Unique identifier}
#' \item{fpc}{finite population correct}
#' \item{stype}{Elementary/Middle/High School}
#' \item{cname}{County name}
#' \item{pcttest}{percentage of students tested}
#' \item{ell}{Percent English language learners}
#' \item{meals}{Percent of students on free and reduced price lunch}
#' \item{poverty}{Federally-defined poverty level based meals variable}
#' \item{winner}{Eligible for awards program}
#' \item{sch.wide}{Met school-wide growth target}
#' \item{comp.imp}{Met Comparable Improvement target}
#' }
#' @source \url{http://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm}
"ca_school_svy"
