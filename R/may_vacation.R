#' Create dummies for May vacation (In dutch: 'Meivakantie')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of may vacation. The day(s) before or after May vacation
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after May vacation, set
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
#'may_holiday = may_vacation(dates)
#')
#'#View(df)
may_vacation <- function(dates, pre=0, post=0){
  may_dates <- find_dates(dates, "-05-01$")

  bin <- vector("character", length = length(may_dates))
  for(date in seq_along(may_dates)){
    if(lubridate::wday(may_dates[date])==7 | lubridate::wday(may_dates[date])==1){
      bin[date] <- get_wday_before_dates("monday", may_dates[date], search = "after")

    } else if(lubridate::wday(may_dates[date])==2){
      bin[date] <- as.character(may_dates[date])

    } else {
      bin[date] <- get_wday_before_dates("monday", may_dates[date], search = "before")
    }
  }
  may_vacation_days <- add_intervals(as.Date(bin), pre = 0 - pre, post = 6 + post)
  dummies <- add_holiday_dummies(dates, may_vacation_days)
  return(dummies)
}
