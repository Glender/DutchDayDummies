#' Create dummies for Remembrance day (In dutch: 'Dodenherdenking')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Remembrance day. The day(s) before or after Remembrance day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Remembrance day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-5-1", "2021-5-10")
#' tibble::tibble(
#'    dates,
#'    remem_day = remembrance_day(dates)
#')
remembrance_day <- function(dates, pre=0,post=0){

  # In dutch: dodenherdenking
  rem_day_regex <- "19[4-9][5-9]-05-04|19[5-9].-05-04|2...-05-04"
  holiday_dates <- find_dates(dates, rem_day_regex) %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
