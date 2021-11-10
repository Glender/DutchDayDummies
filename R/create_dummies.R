# find dates in a year
find_dates <- function(date_vector, date_regex){
  years <- unique(lubridate::year(date_vector))
  dates <- seq_days(
    from = paste0(min(years), "-1-1"),
    to = paste0(max(years), "-12-31")
  )
  search_dates <- dates[
    stringr::str_detect(
      dates, date_regex)
    ]
  return(search_dates)
}

# recreate dates in a year and add intervals
# for each date, create a new date that shifts
# the date j with i backwards/forewards.
# Do that for each x desired shift (pre/post)
add_intervals <- function(holiday_dates, pre, post){
  pre_dates <- c()
  if(pre > 0){
    for(j in seq_along(holiday_dates)){
      bin <- vector("character", pre)
      for(i in 1:pre){
        bin[i] <- as.character(holiday_dates[j] - i)
      }
      pre_dates <- append(pre_dates, bin)
    }
  }
  post_dates <- c()
  if(post > 0){
    for(j in seq_along(holiday_dates)){
      bin <- vector("character", post)
      for(i in 1:post){
        bin[i] <- as.character(holiday_dates[j] + i)
      }
      post_dates <- append(post_dates, bin)
    }
  }
  holiday_dates <- c(
    as.character(holiday_dates),
    pre_dates,
    post_dates
  )
  return(holiday_dates)
}

add_holiday_dummies <-function(dates, holiday_dates){
  # create new column and recode holidays
  vector <- rep(0, length(dates))
  for(date in holiday_dates){
    vector <- ifelse(dates == date, 1, vector)
  }
  return(vector)
}

#' Create a dummy variable from a date vector
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of a holiday. The day(s) before or after the holiday dates
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the two days after your specified holiday dates, set
#' post argument to 2.
#'
#' @param dates date
#' @param holiday_dates date
#' @param pre non-negative int. To recode the x days after the holiday dates.
#' @param post non-negative int. To recode the x days before the holiday dates.
#'
#' @return A dummy (binary vector)
#' @export
#'
#' @examples
#' dates <- seq_days(from = "2021-12-24", to = "2021-12-31")
#' create_dummy(dates, as.Date("2021-12-25"), post = 1)
create_dummy <- function(dates, holiday_dates, pre=0, post=0){
  holiday_dates <- add_intervals(holiday_dates, pre = pre, post = post)
  vector <- rep(0, length(dates))
  for(date in holiday_dates){
    vector <- ifelse(dates == date, 1, vector)
  }
  return(vector)
}
