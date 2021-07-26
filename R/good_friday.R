#' Create dummies for Good Friday (In dutch: 'Goede Vrijdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Easter day. The day(s) before or after Good Friday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Good Friday, set
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
#'dates = seq_days("2021-4-1", "2021-4-10"),
#'good_fday = good_friday(dates)
#')
#'print(df)
good_friday <- function(dates, pre=0, post=0){
  # find `Goede vrijdag` based on years
  years <- as.integer(unique(lubridate::year(dates)))
  easter_dates <- sapply(years, gauss_easter_algorithm_Cpp)
  # subtract two days to shift to friday
  easter_dates <- as.Date(easter_dates) - 2
  easter_dates <- add_intervals(easter_dates, pre=pre, post=post)
  # add the holiday as dummies in your df
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
