#' Create dummies for Halloween
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Halloween. The day(s) before or after Halloween
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Halloween, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-10-29", "2021-11-4")
#' tibble::tibble(
#'dates,
#'hween=halloween(dates)
#')
halloween <- function(dates, pre=0, post=0){

  halloween_dates <- find_dates(dates, "10-31$") %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, halloween_dates)
  return(vector)
}
