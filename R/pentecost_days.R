#' Create dummies for both Pentecost days (In dutch: 'Pinksteren')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of White Monday & Sunday. The day(s) before or after these holidays
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after the Pentecost, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-negative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-5-20", "2021-5-29"),
#'pentecost = pentecost_days(dates)
#')
#'print(df)
pentecost_days <- function(dates, pre=0, post=0){

  # Including both white sunday & monday
  years <- as.integer(unique(lubridate::year(dates)))
  easter_dates <- sapply(years, gauss_easter_algorithm_Cpp)

  # convert easter days to white sunday and monday
  w_sundays <- as.Date(easter_dates, format = "%Y-%m-%d") + 49
  w_mondays <- as.Date(easter_dates, format = "%Y-%m-%d") + 50
  easter_dates <- c(w_sundays, w_mondays) %>%
    add_intervals(pre=pre, post=post)

  # add both days to your dataset in the form of dummy variables
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
