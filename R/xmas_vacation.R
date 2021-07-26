#' Create dummies for Christmas vacation (In dutch: 'Kerstvakantie')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of christmas vacation. The day(s) before or after christmas holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after christmas vacation, set
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
#'xmas_holiday = xmas_vacation(dates)
#')
#'#View(df)
xmas_vacation <- function(dates, pre=0, post=0){
  # get christmas dates
  xmas_dates <- find_dates(dates, "-12-25$")
  # loop through each date
  bin <- vector("character", length = length(xmas_dates))
  for(date in seq_along(xmas_dates)){
    # if xmas falle on the weekend, take the monday after xmas
    if(lubridate::wday(xmas_dates[date])==7 | lubridate::wday(xmas_dates[date])==1){
      bin[date] <- get_wday_before_dates("monday", xmas_dates[date], search = "after")
      # if xmas falls on monday, then the xmas vacation starts that day
    } else if(lubridate::wday(xmas_dates[date])==2){
      bin[date] <- as.character(xmas_dates[date])
      # if xmas falls between tuesday and friday, then the xmas vacation start on the monday before
    } else {
      bin[date] <- get_wday_before_dates("monday", xmas_dates[date], search = "before")
    }
  }
  # extend the dates to get the entire vacation period
  xmas_vacation_days <- add_intervals(as.Date(bin), pre = 2 + pre, post = 13 + post)
  # return binary dummies
  dummies <- add_holiday_dummies(dates, xmas_vacation_days)
  return(dummies)
}
