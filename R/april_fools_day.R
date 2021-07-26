#' Create dummies for April Fools' Day (In dutch: '1 April')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of April Fools' Day. The day(s) before or after April Fools' Day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after April Fools' Day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-3-31", "2021-4-6")
#' tibble::tibble(
#'dates,
#'frst_april=april_fools_day(dates)
#')
april_fools_day <- function(dates, pre=0, post=0){
  frst_of_april <- find_dates(dates, "04-01$") %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, frst_of_april)
  return(vector)
}
