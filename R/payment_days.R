#' Create dummies for payment days (In dutch: 'Betaaldag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of paydays. The day(s) before or after payment days
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after payday, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-1-1", "2021-12-31")
#' df <- tibble::tibble(
#'dates,
#'payday=payment_days(dates)
#')
#'subset(df, payday == 1)
payment_days <- function(dates, pre = 0, post = 0){
  # take the 25th of each month, except for december which is 23th
  regex_patterns <- c(
    "-0[1-9]-25$|-1[0-1]-25$|-12-23$"
  )
  payment_dates <- find_dates(dates, regex_patterns) %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  dummy_vector <- add_holiday_dummies(dates, payment_dates)
  return(dummy_vector)
}
