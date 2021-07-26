#' Create dummies for St. Martins Day (In dutch: 'Sint Maarten')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of St. Martins day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after St. Martins day, set the
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#'df <- tibble::tibble(
#'dates = seq_days("2022-11-10", "2022-11-20"),
#'sint_maarten = st_martins_day(dates, pre = 2)
#')
#'print(df)
st_martins_day <- function(dates, pre=0, post=0){
  holiday_dates <- find_dates(dates, "....-11-11") %>%
    add_intervals(pre=pre, post=post)
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
