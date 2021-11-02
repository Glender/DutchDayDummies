#' Create dummies for Easter sunday (In dutch: 'Eerste Paasdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Easter day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after Easter sunday, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-negative int. To encode the days after the holiday dates.
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2021-4-5", "2023-4-10"),
#'easter_sun = easter_sunday(dates, post = 1)
#')
#'subset(df, easter_sun == 1)
easter_sunday <- function(dates, pre=0,post=0){

  # easter sunday is called `1e paasdag` in dutch
  years <- as.integer(unique(lubridate::year(dates)))

  # cacluates on which day of the year easter day falls
  easter_dates <- as.Date(sapply(years, gauss_easter_algorithm_Cpp)) %>%
    add_intervals(pre = pre, post = post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, easter_dates)
  return(vector)
}
