#' Create dummies for Liberation day (In dutch: 'Bevrijdingsdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Liberation day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Liberation day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (bniary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-5-1", "2021-5-10")
#' tibble::tibble(
#'    dates,
#'    lib_day = liberation_day(dates)
#')
liberation_day <- function(dates, pre=0,post=0){
  # In dutch: bevrijdingsdag
  lib_day_regex <- "19[4-9][5-9]-05-05|19[5-9].-05-05|2...-05-05"
  holiday_dates <- find_dates(dates, lib_day_regex) %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
