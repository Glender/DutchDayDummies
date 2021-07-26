#' Create dummies for Thanksgiving
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Thanksgiving day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Thanksgiving, set the
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param country "US" or "Canada"
#' @param post non-negative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2022-11-22", "2022-11-29"),
#'thxgivin = thanksgiving_day(dates, country="US")
#')
#'df
thanksgiving_day <- function(dates, pre=0, post=0, country="US"){
  if(country=="US"){
    # In the U.S., thanksgiving falls on the 4th thursday of November
    thanksgiving_dates <- nth_weekday_of_a_month(dates, nth = 4, weekday = 5, month = 11) %>%
      add_intervals(pre=pre, post=post)
  } else if(country=="Canada"){
    # In Canada, it falls on the second monday of October
    thanksgiving_dates <- nth_weekday_of_a_month(dates, nth = 2, weekday = 2, month = 10) %>%
      add_intervals(pre=pre, post=post)
  } else {
    stop("country must be `US` or `Canada`.", call.=FALSE)
  }
  # convert dates to dummies
  output <- add_holiday_dummies(dates, thanksgiving_dates)
  return(output)
}
