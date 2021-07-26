#' Create dummies for Christmas day (In dutch: Eerste Kerstdag)
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of christmas day. The day(s) before or after christmas
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after christmas, set
#' post argument to 2.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative int. To encode the days before the holiday dates.
#' @param post non-negative int. To encode the days after the holiday dates.
#'
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' dates <- seq_days("2021-12-23", "2021-12-31")
#' tibble::tibble(
#'    dates,
#'    xmas = xmas_day(dates, pre = 1, post = 2)
#')
xmas_day <- function(dates, pre=0, post=0){
  # find the xmas days
  holiday_dates <- find_dates(dates, "12-25$") %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}

#' Create dummies for boxing day (In dutch: 'Tweede kerstdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of boxing day. The day(s) before or after boxing day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after boxing day, set
#' post argument to 2.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre nonnegative integer. To encode the days before the holiday dates
#' @param post nonngevative integer. To encode the days after the holiday dates
#'
#' @return A dummy (binary vector)
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' dates <- seq_days("2021-12-23", "2021-12-31")
#' tibble::tibble(
#'    dates,
#'    box_day = boxing_day(dates, pre = 1, post = 2)
#')
boxing_day <- function(dates, pre=0, post=0){
  # In Dutch: `tweede kerstdag`
  holiday_dates <- find_dates(dates, "12-26$") %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}

#' Create dummies for christmas and boxing day
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of christmas and boxing day. The day(s) before or after christmas and boxing day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after christmas and boxing day, set
#' post argument to 2.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param pre nonnegative integer. To encode the days before the holiday dates
#' @param post nonngevative integer. To encode the days after the holiday dates
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' dates <- seq_days("2021-12-23", "2021-12-31")
#' tibble::tibble(
#'    dates,
#'    box_day = boxing_day(dates, pre = 0, post = 0)
#')
xmas_and_boxing_day <- function(dates, pre=0, post=0){
  # find the xmas days; add pre-post-intervals
  holiday_dates <- find_dates(dates, "12-25$|12-26$") %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}


