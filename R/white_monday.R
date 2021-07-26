#' Create dummies for White Monday (In dutch: 'Tweede Pinksterdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of White Monday. The day(s) before or after White Monday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after White Monday, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-5-20", "2021-5-29"),
#'w_monday = white_monday(dates)
#')
#'print(df)
white_monday <- function(dates, pre=0, post=0){
  # find `tweede pinksterdag` based on years
  years <- as.integer(unique(lubridate::year(dates)))
  easter_dates <- sapply(years, gauss_easter_algorithm_Cpp)
  # subtract days to shift to white monday
  easter_dates <- as.Date(easter_dates, format = "%Y-%m-%d") + 50
  easter_dates <- add_intervals(easter_dates, pre=pre, post=post)
  # add the holiday as dummies in your df
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
