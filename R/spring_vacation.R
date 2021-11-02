#' Create dummies for Spring vacation (In dutch: 'Voorjaarsvakantie')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of spring break. The day(s) before or after spring break
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after spring break, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-ngevative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-1-1", "2021-12-31"),
#'spring_vac = spring_vacation(dates)
#')
#'#View(df)
spring_vacation <- function(dates, pre=0, post=0){

  # get carnaval date
  years <- as.integer(unique(lubridate::year(dates)))
  carnival_dates <- sapply(years, gauss_easter_algorithm_Cpp)
  carnival_dates <- as.Date(carnival_dates, format = "%Y-%m-%d") - 49
  weeknr_carnaval <- as.integer(strftime(carnival_dates, format = "%V"))

  # get vacation dates
  weeknr <- as.integer(strftime(dates, format = "%V"))
  weekday <- lubridate::wday(dates)
  vacation_dates <- dates[weeknr == 7 & weekday == 7]
  vacation_dates_adjusted <- dates[weeknr == 6 & weekday == 7]

  # if carnaval is in week 6, the vacation also starts that week
  vacation_dates <- ifelse(
    weeknr_carnaval == 6,
    as.character(vacation_dates_adjusted),
    as.character(vacation_dates)
  )
  vacation_dates <- add_intervals(as.Date(vacation_dates), pre=pre, post=15+post)

  # create dummies from vacation dates
  vector <- add_holiday_dummies(dates, vacation_dates)
  return(vector)
}
