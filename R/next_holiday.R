#' Find the number of days until the next holiday.
#'
#' @export
#' @return character
#'
#' @examples
#' next_holiday()
next_holiday <- function(){

  # set constants to find nearest holiday
  date <- lubridate::today()
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
  return(out)
}
