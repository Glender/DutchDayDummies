#' Create dummies for king's day (In dutch: 'Koningsdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of king's day. The day(s) before or after king's day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after king's day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' dates <- seq_days("2021-4-25", "2021-4-30")
#' tibble::tibble(
#'    dates,
#'    kday = kings_day(dates)
#')
kings_day <- function(dates, pre=0, post=0){
  # create kday and qday dates from 1949 to 2025
  kday_dates <- "20[1][4-9]-04-27|20[2-9][0-9]-04-27"
  qday_dates <- "19[4-9][9]-04-30|19[5-9][0-9]-04-30|200[0-9]-04-30|20[0-1][0-3]-04-30"
  kdates_regexp <- paste(kday_dates, qday_dates, sep="|")
  holiday_dates <- find_dates(dates, kdates_regexp) %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
