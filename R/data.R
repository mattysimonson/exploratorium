#' Student performance in California schools.
#'
#' Derived from `apistrat` data frame in the `api` data in the survey package.
#'
#' @format A data frame with 200 observations of 14 variables:
#' \describe{
#' \item{id}{Unique identifier}
#' \item{simple.weight}{Sampling Weight}
#' \item{year.round.sch}{Whether school runs year-round}
#' \item{eligible.for.award}{Whether the schools test score improvements from
#' 1999 to 2000 rendered it eligible for an award}
#' \item{poverty}{Federally-defined poverty level based on proportion
#' of students on free and reduced-price meals}
#' \item{size}{Total enrollment is over 1000 (large), over 500 (medium),
#' or under 500 (small)}
#' \item{school.level}{Elementary/Middle/High School}
#' \item{sch.wide.imp.goal}{Met target for schoolwide growth}
#' \item{comparative.imp.goal}{Met target for comparative improvement}
#' \item{test.score.1999}{Average student test score in 1999}
#' \item{test.score.2000}{Average student test score in 2000}
#' \item{new.stu.prop}{Proportion of students who are new that year}
#' \item{eng.lang.learn.prop}{Proportion of English-language learners}
#' \item{qual.teach.prop}{Proportion of teachers who are fully qualified}
#' }
#' @source \url{http://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm}
"ca_school_data"


#' Student performance in California schools (survey package object).
#'
#' A simple probability survey built from the `ca_school_data` data frame,
#' which in turn derives from `apistrat` in the `api` data in the survey package.
#'
#' @format survey design object with 200 observations of 14 variables:
#' \describe{
#' \item{id}{Unique identifier}
#' \item{simple.weight}{Sampling Weight}
#' \item{year.round.sch}{Whether school runs year-round}
#' \item{eligible.for.award}{Whether the schools test score improvements from
#' 1999 to 2000 rendered it eligible for an award}
#' \item{poverty}{Federally-defined poverty level based on proportion
#' of students on free and reduced-price meals}
#' \item{size}{Total enrollment is over 1000 (large), over 500 (medium),
#' or under 500 (small)}
#' \item{school.level}{Elementary/Middle/High School}
#' \item{sch.wide.imp.goal}{Met target for schoolwide growth}
#' \item{comparative.imp.goal}{Met target for comparative improvement}
#' \item{test.score.1999}{Average student test score in 1999}
#' \item{test.score.2000}{Average student test score in 2000}
#' \item{new.stu.prop}{Proportion of students who are new that year}
#' \item{eng.lang.learn.prop}{Proportion of English-language learners}
#' \item{qual.teach.prop}{Proportion of teachers who are fully qualified}
#' }
#' @source \url{http://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm}
"ca_school_svy"


#' Student performance in California schools (srvyr package object).
#'
#' A simple probability survey built from the `ca_school_data` data frame,
#' which in turn derives from `apistrat` in the `api` data in the survey package.
#'
#' @format tbl.svy with 200 observations of 14 variables:
#' \describe{
#' \item{id}{Unique identifier}
#' \item{simple.weight}{Sampling Weight}
#' \item{year.round.sch}{Whether school runs year-round}
#' \item{eligible.for.award}{Whether the schools test score improvements from
#' 1999 to 2000 rendered it eligible for an award}
#' \item{poverty}{Federally-defined poverty level based on proportion
#' of students on free and reduced-price meals}
#' \item{size}{Total enrollment is over 1000 (large), over 500 (medium),
#' or under 500 (small)}
#' \item{school.level}{Elementary/Middle/High School}
#' \item{sch.wide.imp.goal}{Met target for schoolwide growth}
#' \item{comparative.imp.goal}{Met target for comparative improvement}
#' \item{test.score.1999}{Average student test score in 1999}
#' \item{test.score.2000}{Average student test score in 2000}
#' \item{new.stu.prop}{Proportion of students who are new that year}
#' \item{eng.lang.learn.prop}{Proportion of English-language learners}
#' \item{qual.teach.prop}{Proportion of teachers who are fully qualified}
#' }
#' @source \url{http://www.ats.ucla.edu/stat/stata/Library/svy_survey.htm}
"ca_school_tbl_svy"
