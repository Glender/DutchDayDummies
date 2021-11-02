#' Create dummies for Mother's Day (In dutch: 'Moederdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Mother's Day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Mother's Day, set the
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2022-5-1", "2022-5-31"),
#'mum_day = mothers_day(dates, pre=0,post=0)
#')
#'subset(df, mum_day == 1)
mothers_day <- function(dates, pre=0, post=0){

  # mother's day falls on the second sunday of may, thus:
  mothers_day_dates <- nth_weekday_of_a_month(dates, nth = 2, weekday = 1, month = 5) %>%
    add_intervals(pre=pre, post=post)

  # convert mother's day dates to dummies
  output <- add_holiday_dummies(dates, mothers_day_dates)
  return(output)
}
