#' Create dummies for Valentines day (In dutch: 'Valentijnsdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Valentines day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Valentines day, set the
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
#'dates = seq_days("2021-2-12", "2021-2-22"),
#'valentine = valentines_day(dates)
#')
#'print(df)
valentines_day <- function(dates, pre=0, post=0){

  holiday_dates <- find_dates(dates, "....-02-14") %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
