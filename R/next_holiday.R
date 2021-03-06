#' Find the number of days until the next holiday.
#'
#' @export
#' @return list
#'
#' @examples
#' next_holiday()
next_holiday <- function(date=lubridate::today()){

  # set constants to find nearest holiday
  #date <- lubridate::today()
  start_date <- date
  row_sum <- 0

  while (row_sum == 0) {

    # create dataframe with presence/absence of
    # each available holiday
    off <- official_holidays(date)
    non_off <- non_official_holidays(date)
    df <- cbind(off, non_off)

    # when a holiday occurs, the row sums of
    # the dummy vars exceeds zero
    row_sum <- rowSums(df)
    if (row_sum == 0) {
      date <- date + 1
    }
  }

  # find the next holidays
  bools <- as.logical(df[colnames(df)] == 1)
  next_holidays <- colnames(df[bools])
  days_until_nex_holiday <- as.numeric(date - start_date)

  # assure pretty printing of output
  next_holidays <- stringr::str_remove_all(next_holidays, "non.off.|off.")
  next_holidays <- stringr::str_replace(next_holidays, "\\.", " ")

  # create textual output
  out <- paste(
    as.character(days_until_nex_holiday),
    "day(s) until",
    paste0(next_holidays, collapse = ", ")
  )
  return(
    list(
      days_until_nex_holiday, next_holidays
    )
  )
}

#' Find the next x holidays
#'
#' @export
show_upcoming_holidays <-function(nr_of_holidays=5){

  holiday_date <- lubridate::today()
  days_count <- 0

  # for each iteration, find the next holiday
  # print its date
  # and keep track of a cumulative sum to
  for(i in 1:nr_of_holidays){

    data <-  next_holiday(holiday_date)
    days_till_next_holiday <- data[[1]]
    next_holidays <- data[[2]]

    # add one day, but only after the first iteration
    # because that day isn't part of the cumsum
    if(i == 1){
      days_count <- days_count + days_till_next_holiday
    } else {
      days_count <- days_count + days_till_next_holiday + 1
    }

    message(paste(
      as.character(days_count),
      "day(s) until",
      paste0(next_holidays, collapse = ", ")
    ))

    # increase holiday date to find next holiday
    # after the latest found holiday
    holiday_date <- holiday_date + days_till_next_holiday + 1
  }
}




