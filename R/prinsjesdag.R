#' Create dummies for Prinsjesdag
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Prinsjesdag. The day(s) before or after Prinsjesdag
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Prinsjesdag, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-9-19", "2021-9-27")
#' tibble::tibble(
#'dates,
#'prnsdag=prinsjesdag(dates)
#')
prinsjesdag <- function(dates, pre=0, post=0){

  prinsjesdag_dates <- nth_weekday_of_a_month(dates, nth = 3, weekday = 3, month = 9) %>%
    add_intervals(pre=pre, post=post)

  output <- add_holiday_dummies(dates, prinsjesdag_dates)
  return(output)
}
