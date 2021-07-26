#' Get the dates of a weekday before a specified date
#'
#' @param weekday Character. Name of the week (e.g. "monday")
#' @param dates The date of interest. Format: YYYY-MM-DD.
#' @param search Search "before" of "after" the specified date.
#' @return Character vector in the format YYYY-MM-DD.
#' @export
#' @examples
#'# search the monday before the "2021-12-29"
#'date_of_monday <- get_wday_before_dates(weekday = "monday", dates = "2021-12-29", search = "before")
#'# Confirm
#'weekdays(as.Date(date_of_monday))
get_wday_before_dates <- function(weekday, dates, search = "before"){
  # Example: give the dates of the mondays after christmas day, the 25th
  years <- unique(lubridate::year(dates))
  seq_of_dates <- seq_days(
    from = paste0(min(years), "-1-1"),
    to = paste0(max(years), "-12-31")
  )
  # extend dates to prevent out of bounds error
  seq_of_dates <- c(
    as.Date(sort(add_intervals(min(seq_of_dates), pre = 7, post = 0)[c(-1)])),
    seq_of_dates,
    as.Date(add_intervals(max(seq_of_dates), pre = 0, post = 7)[c(-1)])
  )
  day_search_dates <- vector("character", length = length(dates))
  for(date in seq_along(dates)){
    idx <- which(seq_of_dates == dates[date])
    # change indices depending on search argument
    if(search=="before") idx_to_search <- (idx-1):(idx-7)
    if(search=="after") idx_to_search <- (idx+1):(idx+7)
    # grab the weekday before a date
    day_ <- seq_of_dates[idx_to_search][
      which(lubridate::wday(seq_of_dates[idx_to_search])==switch_weekday(weekday))
      ]
    day_search_dates[date] <- as.character(day_)
  }
  return(day_search_dates)
}
