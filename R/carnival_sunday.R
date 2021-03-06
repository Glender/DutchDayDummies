#' Create dummies for Carnival Sunday (In dutch: 'Carnaval')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of Carnival Sunday. The day(s) before or after this holiday
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the day after Carnival Sunday, set the
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
#'dates = seq_days("2022-1-1", "2022-12-31"),
#'carnival = carnival_sunday(dates, pre=0,post=0)
#')
#'subset(df, carnival == 1)
carnival_sunday <- function(dates, pre=0, post=0){

  # find the first carnivals day based on years
  years <- as.integer(unique(lubridate::year(dates)))
  carnival_dates <- sapply(years, gauss_easter_algorithm_Cpp)

  # subtract two days to shift to carnivals thursday
  carnival_dates <- as.Date(carnival_dates, format = "%Y-%m-%d") - 49

  # add the holiday as dummies in your df
  carnival_dates <- add_intervals(carnival_dates, pre=pre, post=post)

  # add the holiday as dummies in your df
  output <- add_holiday_dummies(dates, carnival_dates)
  return(output)
}
