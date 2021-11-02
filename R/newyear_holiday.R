#' Create dummies for new year's day (In dutch: 'Nieuwjaarsdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of new year's day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after new year's day, set
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
#' dates <- seq_days("2021-12-27", "2022-1-2")
#' tibble::tibble(
#'    dates,
#'    nyd = newyears_day(dates)
#')
newyears_day <- function(dates, pre=0,post=0){

  holiday_dates <- find_dates(dates, "01-01$") %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}

#' Create dummies for new year's eve (In dutch: 'Oudjaarsdag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of new year's eve. The day(s) before or after new year's eve
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after new year's eve, set
#' post argument to 2.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-12-27", "2022-1-2")
#' tibble::tibble(
#'    dates,
#'    nye = newyears_eve(dates)
#')
newyears_eve <- function(dates, pre=0, post=0){

  holiday_dates <- find_dates(dates, "12-31$") %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
