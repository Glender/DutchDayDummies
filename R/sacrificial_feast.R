when_is_sacrificial_feast <- function(years){
  seq_of_dates <- seq_days(
    from = paste0(min(years), "-1-1"),
    to = paste0(max(years), "-12-31")
  )
  idates <- sapply(seq_of_dates, ummalqura_algorithm)
  holiday_dates <- seq_of_dates[stringr::str_detect(idates, "^10-12-....") == TRUE]
  return(holiday_dates)
}

#' Create dummies for Sugar feast (estimated by the Umm al-Qura algorithm)
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of the start of Sacrificial feast, also known as Eid al-Adha.
#' The day(s) before or after this holy holiday can also be recoded by using the pre and post arguments.
#' For example, if you also want to recode the days after Sacrificial feast, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @useDynLib DutchDayDummies
#' @importFrom Rcpp sourceCpp
#' @examples
#' dates <- seq_days("2021-7-17", "2021-7-25")
#' tibble::tibble(
#'    dates,
#'    eid_al_adha = sacrificial_feast(dates)
#')
sacrificial_feast <- function(dates, pre=0, post=0){
  years <- as.integer(unique(lubridate::year(dates)))
  holiday_dates <- when_is_sacrificial_feast(years)
  holiday_dates <- add_intervals(holiday_dates, pre=pre, post=post)
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
