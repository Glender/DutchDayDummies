#' Create dummies for Easter weekend (In dutch 'Paasweekend')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Easter day. The day(s) before or after Easter weekend
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Easter weekend, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-ngevative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-4-5", "2023-4-10"),
#'easter_wknd = easter_weekend(dates)
#')
#'subset(df, easter_wknd == 1)
easter_weekend <- function(dates, pre=0,post=0){
  # filter years and cacluate on which day of the year easter day falls
  years <- as.integer(unique(lubridate::year(dates)))
  easter_sundays <- sapply(years, gauss_easter_algorithm_Cpp)
  # add one to shift from sunday to monday
  easter_mondays <- as.Date(easter_sundays, format = "%Y-%m-%d") + 1
  easter_dates <- c(as.Date(easter_sundays, format = "%Y-%m-%d"), easter_mondays) %>%
    add_intervals(pre=pre, post=post)
  # add those days to your dataframe in dummy format
  output <- add_holiday_dummies(dates, easter_dates)
  return(output)
}
