#' Create dummies for Autumn vacation (In dutch: 'Herfstvakantie')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of autumn holiday. The day(s) before or after autumn break
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after autumn break, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-ngevative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-1-1", "2021-12-31"),
#'autumn_holiday = autumn_vacation(dates)
#')
#'#View(df)
autumn_vacation <- function(dates, pre = 0 , post = 0){

  # The autumn vacation presumably falls on the saturday of week 41
  weeknr <- as.integer(strftime(dates, format = "%V"))
  weekday <- lubridate::wday(dates)
  autumn_vacation_dates <- dates[weeknr == 41 & weekday == 7] %>%
    add_intervals(pre=pre, post=15+post) %>%
    sort()

  # create dummies from vacation dates
  vector <- add_holiday_dummies(dates, autumn_vacation_dates)
  return(vector)
}
