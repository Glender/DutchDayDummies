#' Umm al-Qura algorithm to conver Gregorian Dates to Islamic Dates
#'
#' Function converts gregorian to islamic daes. Yields only valid dates for the period from 1937 to 2077.
#'
#' @param date Date in format YYYY-MM-DD.
#'
#' @return Character
#' @export
#'
#' @examples
#' date <- lubridate::today()
#' # convert to islamic date
#' ummalqura_algorithm(date)
ummalqura_algorithm <- function(date){
  idate <- ummalqura_algorithm_Cpp(
    day = lubridate::day(date),
    month = lubridate::month(date),
    year = lubridate::year(date)
  )
  return(idate)
}





