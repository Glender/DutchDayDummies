#' Create dummies for St. Nicholoas Day (In dutch: 'Sinterklaas')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of St. Nicholas Day. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Sinterklaas, set the
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
#'dates = seq_days("2022-12-1", "2022-12-10"),
#'sinterklaas = st_nicholas_day(dates, pre=0,post=0)
#')
#'subset(df, sinterklaas == 1)
st_nicholas_day <- function(dates, pre=0, post=0){

  holiday_dates <- find_dates(dates, "12-05$") %>%
    add_intervals(pre=pre, post=post)

  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
