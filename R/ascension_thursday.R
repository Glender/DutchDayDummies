#' Create dummies for Ascension Day (In dutch: 'Hemelvaartdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Ascension Day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Ascension Day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-negative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-5-24", "2021-5-31"),
#'ascension_day = ascension_thursday(dates)
#')
#'print(df)
ascension_thursday <- function(dates, pre=0, post=0){

  # find  'Hemelvaartdag' based on years
  years <- as.integer(unique(lubridate::year(dates)))
  easter_dates <- sapply(years, gauss_easter_algorithm_Cpp)

  # add days to shift to ascension thurday
  easter_dates <- as.Date(easter_dates, format = "%Y-%m-%d") + 39
  easter_dates <- add_intervals(easter_dates, pre=pre, post=post)

  # add the holiday as dummies in your df
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
