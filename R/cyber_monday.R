#' Create dummies for Cyber Monday
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Cyber monday. The day(s) before or after Cyber Monday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Cyber monday, set the
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param country "US" or "Canada"
#' @param post non-negative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2022-11-22", "2022-11-29"),
#'cyber_mday = cyber_monday(dates, country="US")
#')
#'df
cyber_monday <- function(dates, pre=0, post=0, country="US"){
  if(country=="US"){
    # In the U.S., thanksgiving falls on the 4th thursday of November
    thanksgiving_dates <- nth_weekday_of_a_month(dates, nth = 4, weekday = 5, month = 11)
  } else if(country=="Canada"){
    # In Canada, it falls on the second monday of October
    thanksgiving_dates <- nth_weekday_of_a_month(dates, nth = 2, weekday = 2, month = 10)
  } else {
    stop("country must be `US` or `Canada`.", call.=FALSE)
  }
  # Black friday falls on one day after thanksgiving
  cyber_monday_dates <- as.Date(thanksgiving_dates, format = "%Y-%m-%d") + 4
  cyber_monday_dates <- add_intervals(cyber_monday_dates, pre=pre, post=post)
  output <- add_holiday_dummies(dates, cyber_monday_dates)
  return(output)
}
