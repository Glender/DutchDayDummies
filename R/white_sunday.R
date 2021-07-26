#' Create dummies for White Sunday (In dutch: 'Eerste Pinksterdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of White Sunday. The day(s) before or after White Sunday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after White Sunday, set
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
#'w_sunday = white_sunday(dates)
#')
#'print(df)
white_sunday <- function(dates, pre=0, post=0){
  # find `eerste pinksterdag` based on years
  years <- as.integer(unique(lubridate::year(dates)))
  easter_dates <- sapply(years, gauss_easter_algorithm_Cpp)
  # subtract days to shift to white sunday
  easter_dates <- as.Date(easter_dates, format = "%Y-%m-%d") + 49
  easter_dates <- add_intervals(easter_dates, pre=pre, post=post)
  # add the holiday as dummies in your df
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
