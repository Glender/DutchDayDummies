validate_dates <- function(dates){
  # raise error when:
  stopifnot(lubridate::is.Date(dates))
  if(!stringr::str_detect(dates, "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}")){
    stop(
      glue::glue(
        "Assure that your date ",
        dates, " complies to the format yyyy-mm-dd. For example: '2021-12-25'."
      ),
      call. = FALSE
    )
  }
}

#' Load the dummy variables from all Dutch official Holidays in a data frame.
#'
#' The following holidays are included: \cr
#' \code{\link[DutchDayDummies]{newyears_day}} \cr
#' \code{\link[DutchDayDummies]{easter_sunday}} \cr
#' \code{\link[DutchDayDummies]{easter_monday}} \cr
#' \code{\link[DutchDayDummies]{kings_day}} \cr
#' \code{\link[DutchDayDummies]{white_monday}} \cr
#' \code{\link[DutchDayDummies]{white_sunday}} \cr
#' \code{\link[DutchDayDummies]{xmas_day}} \cr
#' \code{\link[DutchDayDummies]{boxing_day}} \cr
#'
#' @param dates date vector of format YYYY-MM-DD.
#'
#' @return data.frame of class tibble
#' @export
#'
#' @examples
#' date <- lubridate::today()
#' df <- official_holidays(date)
#' df
official_holidays <- function(dates){
  # create dataframe with all official dutch holidays
  suppressWarnings(validate_dates(dates))
  df <- tibble::tibble(
    "off.New.Years.Day" = newyears_day(dates),
    "off.Easter.Sunday" = easter_sunday(dates),
    "off.Easter.Monday" = easter_monday(dates),
    "off.Kings.Day" = kings_day(dates),
    "off.Ascension.Thursday" = ascension_thursday(dates),
    "off.White.Monday" = white_monday(dates),
    "off.White.Sunday" = white_sunday(dates),
    "off.Xmas.Day" = xmas_day(dates),
    "off.Boxing.Day" = boxing_day(dates)
  )
  return(df)
}

#' Load the dummies from all non-official Holidays in a data frame.
#'
#' The following holidays are included: \cr
#' \code{\link[DutchDayDummies]{carnival_sunday}} \cr
#' \code{\link[DutchDayDummies]{valentines_day}} \cr
#' \code{\link[DutchDayDummies]{good_friday}} \cr
#' \code{\link[DutchDayDummies]{ramadan}} \cr
#' \code{\link[DutchDayDummies]{remembrance_day}} \cr
#' \code{\link[DutchDayDummies]{liberation_day}} \cr
#' \code{\link[DutchDayDummies]{sugar_feast}} \cr
#' \code{\link[DutchDayDummies]{fathers_day}} \cr
#' \code{\link[DutchDayDummies]{sacrificial_feast}} \cr
#' \code{\link[DutchDayDummies]{prinsjesdag}} \cr
#' \code{\link[DutchDayDummies]{world_animal_day}} \cr
#' \code{\link[DutchDayDummies]{thanksgiving_day}} \cr
#' \code{\link[DutchDayDummies]{halloween}} \cr
#' \code{\link[DutchDayDummies]{st_martins_day}} \cr
#' \code{\link[DutchDayDummies]{black_friday}} \cr
#' \code{\link[DutchDayDummies]{cyber_monday}} \cr
#' \code{\link[DutchDayDummies]{st_nicholas_day}} \cr
#' \code{\link[DutchDayDummies]{newyears_eve}} \cr
#'
#' @param dates date vector of format YYYY-MM-DD.
#'
#' @return data.frame of class tibble
#' @export
#'
#' @examples
#' dates <- lubridate::today()
#' df <- non_official_holidays(dates)
#' df
non_official_holidays <- function(dates){
  suppressWarnings(validate_dates(dates))
  df <- tibble::tibble(
    "non.off.Carnival.Sunday" = carnival_sunday(dates),
    "non.off.Valentines.Day" = valentines_day(dates),
    "non.off.Good.Friday" = good_friday(dates),
    "non.off.Ramadan" = ramadan(dates),
    "non.off.Remembrance.Day" = remembrance_day(dates),
    "non.off.Liberation.Day" = liberation_day(dates),
    "non.off.Mothers.Day" = mothers_day(dates),
    "non.off.Sugar.Feast" = sugar_feast(dates),
    "non.off.Fathers.Day" = fathers_day(dates),
    "non.off.Sacrificial.Feast" = sacrificial_feast(dates),
    "non.off.Prinsjesdag" = prinsjesdag(dates),
    "non.off.World.Animal.Day" = world_animal_day(dates),
    "non.off.Thanksgiving.Day" = thanksgiving_day(dates),
    "non.off.Halloween" = halloween(dates),
    "non.off.St.Martins.Day" = st_martins_day(dates),
    "non.off.black.Friday" = black_friday(dates),
    "non.off.Cyber.Monday" = cyber_monday(dates),
    "non.off.St.Nicholas.Day" = st_nicholas_day(dates),
    "non.off.New.Years.Eve" = newyears_eve(dates)
  )
  return(df)
}

#' Load the dummies from all dutch vacations into a dataframe.
#'
#' The following vacations are included: \cr
#' \code{\link[DutchDayDummies]{autumn_vacation}} \cr
#' \code{\link[DutchDayDummies]{xmas_vacation}} \cr
#' \code{\link[DutchDayDummies]{spring_vacation}} \cr
#' \code{\link[DutchDayDummies]{may_vacation}} \cr
#' \code{\link[DutchDayDummies]{summer_vacation}} \cr
#'
#' @param dates date vector of format YYYY-MM-DD.
#'
#' @return data.frame of class tibble
#' @export
#'
#' @examples
#' dates <- lubridate::today()
#' df <- holiday_vacations(dates)
#' df
holiday_vacations <- function(dates){
  suppressWarnings(validate_dates(dates))
  df <- tibble::tibble(
  "vacation.Autunn" = autumn_vacation(dates),
  "vacation.Xmas" = xmas_vacation(dates),
  "vacation.Spring" = spring_vacation(dates),
  "vacation.May" = may_vacation(dates),
  "vacation.Summer" = summer_vacation(dates)
  )
  return(df)
}


#' Load the data from all holiday weekends.
#'
#' The following holiday weekends are included: \cr
#' \code{\link[DutchDayDummies]{xmas_and_boxing_day}} \cr
#' \code{\link[DutchDayDummies]{pentecost_days}} \cr
#' \code{\link[DutchDayDummies]{easter_weekend}} \cr
#'
#' @param dates date vector of format YYYY-MM-DD.
#'
#' @return data.frame of class tibble
#' @export
#'
#' @examples
#' dates <- lubridate::today()
#' df <- holiday_weekends(dates)
#' df
holiday_weekends <- function(dates){
  suppressWarnings(validate_dates(dates))
  df <- tibble::tibble(
    "wknd.Xmas" = xmas_and_boxing_day(dates),
    "wknd.Pentecost" = pentecost_days(dates),
    "wknd.Easter" = easter_weekend(dates)
  )
  return(df)
}

#' Return the dummies for each available holiday, official and non-officials, weekends and vacations.
#'
#' @param dates date vector of format YYYY-MM-DD.
#'
#' @return data.frame of class tibble
#' @export
#'
#' @examples
#' dates <- lubridate::today()
#' df <- load_all_days(dates)
#' df
load_all_days <- function(dates){

  # retrieve multiple holidays
  off <- official_holidays(dates)
  non_off <- non_official_holidays(dates)
  vacations <- holiday_vacations(dates)
  weekends <- holiday_weekends(dates)
  pdays <- payment_days(dates)

  # merge data into a tibble df
  df <- tibble::as_tibble(
    cbind(off, non_off, vacations, weekends, pdays)
  )
  return(df)
}














