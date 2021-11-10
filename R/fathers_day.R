#' Create dummies for Father's Day (In dutch: 'Vaderdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Father's Day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Father's Day, set the
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
#'dates = seq_days("2022-6-14", "2022-6-24"),
#'dad_day = fathers_day(dates, pre=1,post=0)
#')
#'df
fathers_day <- function(dates, pre=0, post=0){

  # fathers day falls on the third sunday of june
  fathers_day_dates <- nth_weekday_of_a_month(dates, nth = 3, weekday = 1, month = 6)
  fathers_day_dates <- add_intervals(
    as.Date(fathers_day_dates), pre=pre, post=post
  )
  # convert father's day dates to dummies
  output <- add_holiday_dummies(dates, fathers_day_dates)
  return(output)
}
