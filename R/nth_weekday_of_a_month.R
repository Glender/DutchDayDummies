#' Function to find the nth weekday in a certain month
#'
#' @param dates A vector with dates in the format YYYY-MM-DD
#' @param nth int. The nth weekend you want to search for given a day and month
#' @param weekday int. Sunday counts as 1, monday as 2, etc.
#' @param month int. The month of interest (1 for January, 2 for February etc.)
#' @return Date written in type character.
#' @export
#' @examples
#'dates <- seq_days(from = "2021-1-1", to = "2021-12-31")
#'# Suppose you want to find the date of mother's day
#'# Mother's day falls on the second sunday of may, thus:
#'nth_weekday_of_a_month(dates, nth = 2, weekday = 1, month = 5)
nth_weekday_of_a_month <- function(dates, nth, weekday, month){
  # create sequence of days
  years <- as.integer(unique(lubridate::year(dates)))
  year_dates <- paste0(years, "-01-01")
  if(length(years)>1){
    day_dates <- seq_days(
      from = year_dates[1],
      to = year_dates[length(year_dates)]
    )
  } else {
    day_dates <- seq_days(
      from = year_dates[1],
      to = paste0(as.character(years), "-12-31")
    )
  }
  # create dataframe w required info
  df <- data.frame(
    dates = as.character(day_dates),
    years = as.integer(lubridate::year(day_dates)),
    months = lubridate::month(day_dates),
    # For the weekdays, sunday counts as 1, saturday as 7, etc.
    weekdays = lubridate::wday(day_dates),
    stringsAsFactors = FALSE
  )
  # filter your dataframes and extract dates
  df <- subset(df, months == month & weekdays == weekday)
  l <- split(df, df$years)
  l <- lapply(l, function(x) {x[nth,1]})
  holiday_dates <- unname(unlist(l))
  return(holiday_dates)
}
